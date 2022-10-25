! SUMMA - Structure for Unifying Multiple Modeling Alternatives
! Copyright (C) 2014-2020 NCAR/RAL; University of Saskatchewan; University of Washington
!
! This file is part of SUMMA
!
! For more information see: http://www.ral.ucar.edu/projects/summa
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module read_forcingActors_module

! data types
USE nrtype                                          ! variable types, etc.

! derived data types
USE data_types,only:var_d         ! x%var(:)     (dp)
USE data_types,only:file_info
USE data_types,only:file_info_array

! constants
USE multiconst,only:secprday                  ! number of seconds in a day

! access missing values
USE globalData,only:realMissing               ! real missing value
USE globalData,only:integerMissing            ! integer missing value

! access the mapping betweeen GRUs and HRUs
USE globalData,only:gru_struc                 ! gru-hru mapping structures

! access the minimum and maximum HRUs in the file
USE globalData,only:ixHRUfile_min,ixHRUfile_max

! global data on the forcing file
USE globalData,only:data_step                 ! length of the data step (s)
USE globalData,only:dJulianStart              ! julian day of start time of simulation
USE globalData,only:nHRUfile                  ! number of HRUs in the file
USE globalData,only:refJulday                 ! reference time (fractional julian days)
USE globalData,only:refJulday_data            ! reference time for data files (fractional julian days)

! global data for forcing_data
USE globalData,only:forcingDataStruct
USE globalData,only:vecTime

! global metadata
USE globalData,only:time_meta,forc_meta       ! metadata structures
USE var_lookup,only:iLookTIME,iLookFORCE      ! named variables to define structure elements
USE var_lookup,only:iLookDECISIONS            ! named variables for elements of the decision structure

! file paths
USE summaActors_FileManager,only:FORCING_PATH        ! path of the forcing data file

USE netcdf_util_module,only:nc_file_close  ! close netcdf file


! privacy
implicit none
private
public::read_forcingActors

! global parameters
real(dp),parameter  :: verySmall=1e-3_rkind      ! tiny number
real(dp),parameter  :: smallOffset=1.e-8_rkind   ! small offset (units=days) to force ih=0 at the start of the day

contains


! ************************************************************************************************
! public subroutine read_forcingActors: read in forcing data
! ************************************************************************************************
subroutine read_forcingActors(&
                    indxGRU,             & ! Index of GRU in gru_struc
                    istep,               & 
                    iFile,               &
                    iRead,               &
                    time_data,           &
                    forcStruct,          &
                    fracJulDay,          &
                    tmZoneOffsetFracDay, &
                    yearLength,          &
                    err,message)
  ! provide access to subroutines
  USE netcdf                                            ! netcdf capability
  USE time_utils_module,only:compJulday                 ! convert calendar date to julian day
  USE time_utils_module,only:compcalday                 ! convert julian day to calendar date
  USE time_utils_module,only:elapsedSec                 ! calculate the elapsed time
  USE netcdf_util_module,only:nc_file_close              ! close netcdf file

  implicit none
  ! define input variables
  integer(i4b),intent(in)             :: indxGRU          ! index of the GRU in gru_struc
  integer(i4b),intent(in)             :: istep            ! time index AFTER the start index
  ! define input-output variables
  integer(i4b),intent(inout)          :: iFile            ! index of current forcing file in forcing file list
  integer(i4b),intent(inout)          :: iRead            ! index of read position in time dimension in current netcdf file
  ! define output variables
  integer(i4b),intent(inout)          :: time_data(:)     ! vector of time data for a given time step
  type(var_d),intent(inout)           :: forcStruct       ! x%var(:)     -- model forcing data
  real(dp),intent(inout)              :: fracJulDay
  real(dp),intent(inout)              :: tmZoneOffsetFracDay 
  integer(i4b),intent(inout)          :: yearLength       
  integer(i4b),intent(out)            :: err              ! error code
  character(*),intent(out)            :: message          ! error message
  ! define local variables
  integer(i4b)                        :: nHRUlocal        ! number of HRUs in the local simulation
  character(len=256)                  :: cmessage         ! error message for downwind routine
  real(dp)                            :: startJulDay      ! julian day at the start of the year
  real(dp)                            :: currentJulday    ! Julian day of current time step
  logical(lgt),parameter              :: checkTime=.false.  ! flag to check the time
    ! Start procedure here
  err=0; message="read_forcingActors/"
  

  ! get the number of HRUs in the local simulation
  nHRUlocal = sum(gru_struc(:)%hruCount)

  ! determine the julDay of current model step (istep) we need to read
  if(istep==1)then
    currentJulDay = dJulianStart
    ! identify the first time step, and fill and tmZoneOffsetFracDay
    ! This function sets the vaule of iRead which is the current timestep in the forcing data
    ! istep is the model's timestep
    call getFirstTimestep(currentJulday,iFile,iRead,tmZoneOffsetFracDay,err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
  else
    currentJulDay = dJulianStart + (data_step*real(iStep-1,dp))/secprday
  end if

  ! **********************************************************************************************
  ! ***** part 1: if file open, check to see if we've reached the end of the file, if so close it,
  ! *****         and open new file
  ! *****         Then read the data
  ! **********************************************************************************************
  ! check to see if we've passed end of netcdf file
  if(iRead>forcingDataStruct(iFile)%nTimeSteps)then

    ! increment iFile so we look in the correct location in the forcingDataStruct structure
    iFile = iFile+1
    if(size(forcingDataStruct(:))<iFile)then; message=trim(message)//'files in list do not include desired data'; err=20; return; endif

    !  ! open up the forcing file
    call openForcingFile(iFile,tmZoneOffsetFracDay,err,cmessage)
    ! print*, "Err = ", err
    if(err/=0)then; message=trim(message)//trim(cmessage); return; end if

    iRead=1

  end if

  ! read forcing data
  call readForcingDataActors(indxGRU,currentJulday,iFile,iRead,time_data,forcStruct,err,message)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if


 ! **********************************************************************************************
 ! ***** part 2: compute time
 ! **********************************************************************************************

 ! compute the julian day at the start of the year
 call compjulday(time_data(iLookTIME%iyyy),          & ! input  = year
                 1, 1, 1, 1, 0._dp,                  & ! input  = month, day, hour, minute, second
                 startJulDay,err,cmessage)             ! output = julian day (fraction of day) + error control
 if(err/=0)then; message=trim(message)//trim(cmessage); return; end if

 ! compute the fractional julian day for the current time step
 call compjulday(time_data(iLookTIME%iyyy),           & ! input  = year
                 time_data(iLookTIME%im),             & ! input  = month
                 time_data(iLookTIME%id),             & ! input  = day
                 time_data(iLookTIME%ih),             & ! input  = hour
                 time_data(iLookTIME%imin),0._dp,     & ! input  = minute/second
                 currentJulday,err,cmessage)            ! output = julian day (fraction of day) + error control
 if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
 ! compute the time since the start of the year (in fractional days)
 fracJulDay = currentJulday - startJulDay
 ! set timing of current forcing vector (in seconds since reference day)
 ! NOTE: It is a bit silly to have time information for each HRU and GRU
 forcStruct%var(iLookFORCE%time) = (currentJulday-refJulday)*secprday

 ! compute the number of days in the current year
 yearLength = 365
 if(mod(time_data(iLookTIME%iyyy),4) == 0)then
  yearLength = 366
  if(mod(time_data(iLookTIME%iyyy),100) == 0)then
   yearLength = 365
   if(mod(time_data(iLookTIME%iyyy),400) == 0)then
    yearLength = 366
   end if
  end if
 end if

 ! test
 if(checkTime)then
  write(*,'(i4,1x,4(i2,1x),f9.3,1x,i4)')  time_data(iLookTIME%iyyy),           & ! year
                                          time_data(iLookTIME%im),             & ! month
                                          time_data(iLookTIME%id),             & ! day
                                          time_data(iLookTIME%ih),             & ! hour
                                          time_data(iLookTIME%imin),           & ! minute
                                          fracJulDay,                          & ! fractional julian day for the current time step
                                          yearLength                             ! number of days in the current year
  !pause ' checking time'
 end if
 
end subroutine read_forcingActors

! *******************************************************************************************************************
! *******************************************************************************************************************
! *******************************************************************************************************************
! *******************************************************************************************************************
! *******************************************************************************************************************

! *************************************************************************
! * private subroutine: find first timestep in any of the forcing files...
! *************************************************************************
subroutine getFirstTimestep(currentJulday,iFile,iRead,tmZoneOffsetFracDay,err,message)
 USE netcdf                                            ! netcdf capability
 USE nr_utility_module,only:arth                       ! get a sequence of numbers
 implicit none
 ! define input
 real(dp),intent(in)               :: currentJulday    ! Julian day of current time step
 ! define input-output variables
 integer(i4b),intent(inout)        :: iFile            ! index of current forcing file in forcing file list
 integer(i4b),intent(inout)        :: iRead            ! index of read position in time dimension in current netcdf file
 ! define output variables
 real(dp),intent(inout)            :: tmZoneOffsetFracDay 
 integer(i4b),intent(out)          :: err              ! error code
 character(*),intent(out)          :: message          ! error message
 ! ------------------------------------------------------------------------------------------------------------------
 integer(i4b)                      :: nSteps

 ! other local variables
 integer(i4b)                      :: nFiles           ! number of forcing files
 real(dp)                          :: timeVal(1)       ! single time value (restrict time read)
 real(dp),allocatable              :: fileTime(:)      ! array of time from netcdf file
 real(dp),allocatable              :: diffTime(:)      ! array of time differences


 ! Start procedure here
 err=0; message="getFirstTimestep/"
 ! get the number of forcing files
 nFiles=size(forcingDataStruct(:))  ! number of forcing files

 ! keep going until we find where in the forcingDataStruct structure the first timestep is
 do iFile=1,nFiles

  nSteps = forcingDataStruct(iFile)%nTimeSteps
  call openForcingFile(iFile,tmZoneOffsetFracDay,err,message)

  ! allocate space for time vectors
  if(allocated(fileTime)) deallocate(fileTime)
  if(allocated(diffTime)) deallocate(diffTime)
  allocate(fileTime(nSteps),diffTime(nSteps),stat=err)
  if(err/=0)then; message=trim(message)//'problem allocating time vectors'; return; end if

  timeVal(1) = vecTime(iFile)%dat(1)
  ! get time vector & convert units based on offset and data step

  fileTime = arth(0,1,nSteps) * data_step/secprday + refJulday_data &
              + timeVal(1)/forcingDataStruct(iFile)%convTime2Days

  ! find difference of fileTime from currentJulday
  diffTime=abs(fileTime-currentJulday)
  ! start time is in the current file
  if(any(diffTime < verySmall))then
   iRead=minloc(diffTime,1)
   exit

  ! time step is not in current index in forcingDataStruct
  else
   ! check that it is not the last file
   if(iFile==nFiles)then; err=99; message=trim(message)//'first requested simulation timestep not in any forcing file'; return; end if

  end if  ! first time step is not in any forcing files

 end do ! end of search for model first time step in forcing files

end subroutine getFirstTimestep

! *************************************************************************
! * open the NetCDF forcing file and get the time information
! *************************************************************************
subroutine openForcingFile(iFile,tmZoneOffsetFracDay,err,message)
 USE time_utils_module,only:fracDay                      ! compute fractional day
 USE time_utils_module,only:extractTime                  ! extract time info from units string
 USE time_utils_module,only:compJulday                   ! convert calendar date to julian day
 !USE globalData,only:tmZoneOffsetFracDay                ! time zone offset in fractional days
 USE globalData,only:ncTime                              ! time zone information from NetCDF file (timeOffset = longitude/15. - ncTimeOffset)
 USE globalData,only:utcTime                             ! all times in UTC (timeOffset = longitude/15. hours)
 USE globalData,only:localTime                           ! all times local (timeOffset = 0)
 USE summaActors_filemanager,only:NC_TIME_ZONE
 ! dummy variables
 integer(i4b),intent(in)           :: iFile              ! index of current forcing file in forcing file list
 real(dp),intent(inout)            :: tmZoneOffsetFracDay
 integer(i4b) ,intent(out)         :: err                ! error code
 character(*) ,intent(out)         :: message            ! error message
 ! local variables
 character(len=256)                :: cmessage           ! error message for downwind routine
 integer(i4b)                      :: iyyy,im,id,ih,imin ! date
 integer(i4b)                      :: ih_tz,imin_tz      ! time zone information
 real(dp)                          :: dsec,dsec_tz       ! seconds
 character(len=256)                :: refTimeString      ! reference time string

 ! initialize error control
 err=0; message='openForcingFile/'

 ! Copy String
 ! May not actually be needed by an HRU
 refTimeString = forcingDataStruct(iFile)%refTimeString

 ! define the reference time for the model simulation
 call extractTime(refTimeString,                         & ! input  = units string for time data
                 iyyy,im,id,ih,imin,dsec,                & ! output = year, month, day, hour, minute, second
                 ih_tz, imin_tz, dsec_tz,                &  ! output = time zone information (hour, minute, second)
                 err,cmessage)                            ! output = error code and error message
 if(err/=0)then; message=trim(message)//trim(cmessage); return; end if

 select case(trim(NC_TIME_ZONE))
  case('ncTime'); tmZoneOffsetFracDay = sign(1, ih_tz) * fracDay(ih_tz,   & ! time zone hour
                                                                 imin_tz, & ! time zone minute
                                                                 dsec_tz)   ! time zone second
  case('utcTime');   tmZoneOffsetFracDay = 0._dp
  case('localTime'); tmZoneOffsetFracDay = 0._dp
  case default; err=20; message=trim(message)//'unable to identify time zone info option'; return
 end select ! (option time zone option)

end subroutine openForcingFile

! *************************************************************************
! * read the NetCDF forcing data
! *************************************************************************
subroutine readForcingDataActors(&
    indxGRU,currentJulday,iFile,iRead,time_data,forcStruct,err,message)
    USE netcdf                                            ! netcdf capability
    USE time_utils_module,only:compcalday                 ! convert julian day to calendar date
    USE time_utils_module,only:compJulday                 ! convert calendar date to julian day
    USE get_ixname_module,only:get_ixforce                ! identify index of named variable
    ! dummy variables
    integer(i4b),intent(in)                 :: indxGRU          ! index of GRU in gru_struc  
    real(dp),intent(in)                     :: currentJulday    ! Julian day of current time step
    integer(i4b) ,intent(in)                :: iFile            ! index of forcing file
    integer(i4b) ,intent(in)                :: iRead            ! index in data file
    integer(i4b),intent(inout)              :: time_data(:)     ! vector of time data for a given time step
    type(var_d),intent(inout)               :: forcStruct       ! x%var(:)     -- model forcing data 
    integer(i4b) ,intent(out)               :: err              ! error code
    character(*) ,intent(out)               :: message          ! error message
    ! local variables      
    character(len=256)                      :: cmessage         ! error message for downwind routine
    character(len = nf90_max_name)          :: varName          ! dimenison name
    
    ! other local variables
    integer(i4b)                            :: iline            ! loop through lines in the file
    integer(i4b)                            :: iNC              ! loop through variables in forcing file
    integer(i4b)                            :: iVar             ! index of forcing variable in forcing data vector
    logical(lgt),parameter                  :: checkTime=.false.  ! flag to check the time
    real(dp)                                :: dsec             ! double precision seconds (not used)
    real(dp)                                :: dataJulDay       ! julian day of current forcing data step being read
    real(dp),parameter                      :: dataMin=-1._dp   ! minimum allowable data value (all forcing variables should be positive)
    logical(lgt),dimension(size(forc_meta)) :: checkForce ! flags to check forcing data variables exist
    ! Start procedure here
    err=0; message="readForcingDataActors/"

    ! initialize time and forcing data structures
    time_data(:) = integerMissing

    ! check that the computed julian day matches the time information in the NetCDF file
    dataJulDay = vecTime(iFile)%dat(iRead)/forcingDataStruct(iFile)%convTime2Days + refJulday_data
    if(abs(currentJulday - dataJulDay) > verySmall)then
      write(message,'(a,f18.8,a,f18.8)') trim(message)//'date for time step: ',dataJulDay,' differs from the expected date: ',currentJulDay
      err=40
      return
    end if

    ! convert julian day to time vector
    ! NOTE: use small offset to force ih=0 at the start of the day
    call compcalday(dataJulDay+smallOffset,         & ! input  = julian day
                    time_data(iLookTIME%iyyy),      & ! output = year
                    time_data(iLookTIME%im),        & ! output = month
                    time_data(iLookTIME%id),        & ! output = day
                    time_data(iLookTIME%ih),        & ! output = hour
                    time_data(iLookTIME%imin),dsec, & ! output = minute/second
                    err,cmessage)                     ! output = error control
    if(err/=0)then; message=trim(message)//trim(cmessage); return; end if

    ! check to see if any of the time data is missing -- note that it is OK if ih_tz or imin_tz are missing
    if((time_data(iLookTIME%iyyy)==integerMissing) .or. (time_data(iLookTIME%im)==integerMissing) .or. (time_data(iLookTIME%id)==integerMissing) .or. (time_data(iLookTIME%ih)==integerMissing) .or. (time_data(iLookTIME%imin)==integerMissing))then
        do iline=1,size(time_data)
            if(time_data(iline)==integerMissing)then; err=40; message=trim(message)//"variableMissing[var='"//trim(time_meta(iline)%varname)//"']"; return; end if
        end do
    end if

 ! initialize flags for forcing data
 checkForce(:) = .false.
 checkForce(iLookFORCE%time) = .true.  ! time is handled separately

 do iNC=1,forcingDataStruct(iFile)%nVars
  ! check variable is desired
  if(forcingDataStruct(iFile)%var_ix(iNC)==integerMissing) cycle

  ! get index in forcing structure
  iVar = forcingDataStruct(iFile)%var_ix(iNC)
  checkForce(iVar) = .true.

  ! check individual data value
  if(forcingDataStruct(iFile)%var(ivar)%dataFromFile(indxGRU,iRead)<dataMin)then
   write(message,'(a,f13.5)') trim(message)//'forcing data for variable '//trim(varname)//' is less than minimum allowable value ', dataMin
   err=20; return
  endif
  ! put the data into structures
  forcStruct%var(ivar) = forcingDataStruct(iFile)%var(ivar)%dataFromFile(indxGRU,iRead)
 end do  ! loop through forcing variables

 ! check if any forcing data is missing
 if(count(checkForce)<size(forc_meta))then
  do iline=1,size(forc_meta)
   if(.not.checkForce(iline))then
    message=trim(message)//"checkForce_variableMissing[var='"//trim(forc_meta(iline)%varname)//"']"
    err=20; return
   endif    ! if variable is missing
  end do   ! looping through variables
 end if   ! if any variables are missing
end subroutine readForcingDataActors

end module read_forcingActors_module
