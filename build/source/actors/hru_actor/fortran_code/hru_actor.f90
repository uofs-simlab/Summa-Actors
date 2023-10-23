module hru_actor
USE,intrinsic :: iso_c_binding
USE nrtype
USE data_types,only:&
                    var_i,          &  
                    var_i8,         &
                    var_d,          &
                    var_ilength,    &
                    var_dlength,    &
                    flagVec,        &
                    hru_type
implicit none

public::getFirstTimestep
public::setTimeZoneOffset
public::setIDATolerances

real(dp),parameter  :: verySmall=1e-3_rkind      ! tiny number
real(dp),parameter  :: smallOffset=1.e-8_rkind   ! small offset (units=days) to force ih=0 at the start of the day

contains


! Find the first timestep within the forcing file
subroutine getFirstTimestep(iFile, iRead, err) bind(C, name="getFirstTimestep")
  USE globalData,only:forcingDataStruct         ! forcing structure
  USE globalData,only:vecTime                   ! time structure for forcing 
  USE globalData,only:dJulianStart              ! julian day of start time of simulation
  USE globalData,only:data_step                 ! length of the data step (s)
  USE globalData,only:refJulDay_data            ! reference time for data files (fractional julian days)
   
  USE multiconst,only:secprday                  ! number of seconds in a day
  
  USE nr_utility_module,only:arth               ! get a sequence of numbers

  implicit none

  integer(c_int),intent(in)                               :: iFile
  integer(c_int),intent(out)                              :: iRead
  integer(c_int),intent(out)                              :: err
  ! local variables
  character(len=256)                                      :: message
  real(dp)                                                :: timeVal(1)    ! single time value (restrict time read)
  real(dp),dimension(forcingDataStruct(iFile)%nTimeSteps) :: fileTime      ! array of time from netcdf file
  real(dp),dimension(forcingDataStruct(iFile)%nTimeSteps) :: diffTime      ! array of time differences

  err=0; message="hru_actor.f90 - getFirstTimeStep"

  ! get time vector & convert units based on offset and data step
  timeVal(1) = vecTime(iFile)%dat(1)
  fileTime = arth(0,1,forcingDataStruct(iFile)%nTimeSteps) * data_step/secprday + refJulDay_data &
              + timeVal(1)/forcingDataStruct(iFile)%convTime2Days

  ! find difference of fileTime from currentJulDay
  diffTime=abs(fileTime-dJulianStart)
  
  if(any(diffTime < verySmall))then
    iRead=minloc(diffTime,1)
  else
    iRead=-1 ! set to -1 to designinate this forcing file is not the start
  endif
  
end subroutine getFirstTimestep

! set the refTimeString and extract the time to set the tmZonOffsetFracDay
subroutine setTimeZoneOffset(iFile, tmZoneOffsetFracDay, err) bind(C, name="setTimeZoneOffset")
  USE globalData,only:forcingDataStruct         ! forcing structure


  USE time_utils_module,only:extractTime        ! extract time info from units string
  USE time_utils_module,only:fracDay            ! compute fractional day

  USE summafilemanager,only:NC_TIME_ZONE
  implicit none

  integer(c_int),intent(in)             :: iFile
  real(c_double),intent(out)            :: tmZoneOffsetFracDay
  integer(c_int),intent(out)            :: err

  ! local variables
  character(len=256)                    :: message
  character(len=256)                    :: cmessage
  integer(i4b)                          :: iyyy,im,id,ih,imin ! date
  integer(i4b)                          :: ih_tz,imin_tz      ! time zone information
  real(dp)                              :: dsec,dsec_tz       ! seconds

  err=0; message="hru_actor.f90 - setForcingTimeInfo";

 ! define the reference time for the model simulation
 call extractTime(forcingDataStruct(iFile)%refTimeString,& ! input  = units string for time data
                 iyyy,im,id,ih,imin,dsec,                & ! output = year, month, day, hour, minute, second
                 ih_tz, imin_tz, dsec_tz,                & ! output = time zone information (hour, minute, second)
                 err,cmessage)                             ! output = error code and error message
  if(err/=0)then; message=trim(message)//trim(cmessage); print*, "message"; return; end if
  
 ! set the timezone offset
 select case(trim(NC_TIME_ZONE))
    case('ncTime'); tmZoneOffsetFracDay = sign(1, ih_tz) * fracDay(ih_tz,   & ! time zone hour
                                                                  imin_tz, & ! time zone minute
                                                                  dsec_tz)   ! time zone second
    case('utcTime');   tmZoneOffsetFracDay = 0._dp
    case('localTime'); tmZoneOffsetFracDay = 0._dp
    case default; err=20; message=trim(message)//'unable to identify time zone info option'; return
  end select ! (option time zone option)

end subroutine setTimeZoneOffset


subroutine readForcingHRU(indxGRU, iStep, iRead, handle_hru_data, iFile, err) bind(C, name="readForcingHRU")
  USE multiconst,only:secprday                  ! number of seconds in a day
  ! global Data
  USE globalData,only:data_step                 ! length of the data step (s)
  USE globalData,only:dJulianStart              ! julian day of start time of simulation
  USE globalData,only:refJulDay_data            ! reference time for data files (fractional julian days)
  USE globalData,only:integerMissing            ! integer missing value
  USE globalData,only:vecTime
  USE globalData,only:forcingDataStruct
  USE globalData,only:time_meta,forc_meta
  USE var_lookup,only:iLookTIME,iLookFORCE
  USE data_types,only:var_i,var_d
  USE netcdf                                   ! used for nf90_max_name
  USE time_utils_module,only:compcalday                 ! convert julian day to calendar date

  implicit none

  integer(c_int),intent(in)               :: indxGRU          ! Index of the GRU in gru_struc
  integer(c_int),intent(in)               :: istep            ! Model Timestep
  integer(c_int),intent(in)               :: iRead            ! Model Timestep 
  type(c_ptr),intent(in),value            :: handle_hru_data  ! vector of time data for a given time step
  integer(c_int),intent(in)               :: iFile            ! index of current forcing file from forcing file list 
  integer(c_int),intent(out)              :: err              ! Model Timestep
  ! local variables
  type(hru_type),pointer                  :: hru_data         !  model time data
  real(dp)                                :: currentJulDay    ! Julian day of current time step
  real(dp)                                :: dataJulDay       ! julian day of current forcing data step being read
  ! Counters
  integer(i4b)                            :: iline            ! loop through lines in the file
  integer(i4b)                            :: iVar
  integer(i4b)                            :: iNC
  ! other
  logical(lgt),dimension(size(forc_meta)) :: checkForce       ! flags to check forcing data variables exist
  
  real(dp)                                :: dsec             ! double precision seconds (not used)
  real(dp),parameter                      :: dataMin=-1._dp   ! minimum allowable data value (all forcing variables should be positive)
  character(len = nf90_max_name)          :: varName          ! dimenison name

  character(len=256)                      :: message          ! error message
  character(len=256)                      :: cmessage         ! error message

  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_hru_data, hru_data)

  err=0;message="hru_actor.f90 - readForcingHRU";

  ! determine the julDay of current model step (istep) we need to read
  currentJulDay = dJulianStart + (data_step*real(iStep-1,dp))/secprday

  hru_data%timeStruct%var(:) = integerMissing
  dataJulDay = vecTime(iFile)%dat(iRead)/forcingDataStruct(iFile)%convTime2Days + refJulDay_data
 if(abs(currentJulDay - dataJulDay) > verySmall)then
    write(message,'(a,f18.8,a,f18.8)') trim(message)//'date for time step: ',dataJulDay,' differs from the expected date: ',currentJulDay
    print*, message
    err=40
    return
  end if

  ! convert julian day to time vector
  ! NOTE: use small offset to force ih=0 at the start of the day
  call compcalday(dataJulDay+smallOffset,         & ! input  = julian day
                  hru_data%timeStruct%var(iLookTIME%iyyy),      & ! output = year
                  hru_data%timeStruct%var(iLookTIME%im),        & ! output = month
                  hru_data%timeStruct%var(iLookTIME%id),        & ! output = day
                  hru_data%timeStruct%var(iLookTIME%ih),        & ! output = hour
                  hru_data%timeStruct%var(iLookTIME%imin),dsec, & ! output = minute/second
                  err,cmessage)                     ! output = error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
  
  ! check to see if any of the time data is missing -- note that it is OK if ih_tz or imin_tz are missing
  if((hru_data%timeStruct%var(iLookTIME%iyyy)==integerMissing) .or. (hru_data%timeStruct%var(iLookTIME%im)==integerMissing) .or. (hru_data%timeStruct%var(iLookTIME%id)==integerMissing) .or. (hru_data%timeStruct%var(iLookTIME%ih)==integerMissing) .or. (hru_data%timeStruct%var(iLookTIME%imin)==integerMissing))then
      do iline=1,size(hru_data%timeStruct%var)
          if(hru_data%timeStruct%var(iline)==integerMissing)then; err=40; message=trim(message)//"variableMissing[var='"//trim(time_meta(iline)%varname)//"']"; return; end if
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
    hru_data%forcStruct%var(ivar) = forcingDataStruct(iFile)%var(ivar)%dataFromFile(indxGRU,iRead)
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
end subroutine readForcingHRU 

! This is part 2: from the reading of forcing - separated it so we could call from C++
subroutine computeTimeForcingHRU(handle_hru_data, fracJulDay, yearLength, err) bind(C, name="computeTimeForcingHRU")
  USE var_lookup,only:iLookTIME,iLookFORCE
  USE time_utils_module,only:compJulDay         ! convert calendar date to julian day
  USE data_types,only:var_i,var_d
  USE multiconst,only:secprday                  ! number of seconds in a day
  USE globalData,only:refJulDay                 ! reference time (fractional julian days)

  implicit none
  type(c_ptr),intent(in),value        :: handle_hru_data      ! vector of time data for a given time step
  real(c_double),intent(out)          :: fracJulDay
  integer(c_int),intent(out)          :: yearLength    
  integer(c_int),intent(out)          :: err   
  ! local variables
  type(hru_type),pointer              :: hru_data         !  model time data
  real(dp)                            :: startJulDay      ! julian day at the start of the year
  real(dp)                            :: currentJulDay    ! Julian day of current time step
  character(len=256)                  :: message          ! error message for downwind routine
  character(len=256)                  :: cmessage         ! error message for downwind routine
  logical(lgt),parameter              :: checkTime=.false.  ! flag to check the time

  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_hru_data, hru_data)

  err=0; message="hru_actor.f90 - computTimeForcingHRU/"

  ! compute the julian day at the start of the year
  call compjulday(hru_data%timeStruct%var(iLookTIME%iyyy),          & ! input  = year
                  1, 1, 1, 1, 0._dp,                  & ! input  = month, day, hour, minute, second
                  startJulDay,err,cmessage)             ! output = julian day (fraction of day) + error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if

  ! compute the fractional julian day for the current time step
  call compjulday(hru_data%timeStruct%var(iLookTIME%iyyy),           & ! input  = year
                  hru_data%timeStruct%var(iLookTIME%im),             & ! input  = month
                  hru_data%timeStruct%var(iLookTIME%id),             & ! input  = day
                  hru_data%timeStruct%var(iLookTIME%ih),             & ! input  = hour
                  hru_data%timeStruct%var(iLookTIME%imin),0._dp,     & ! input  = minute/second
                  currentJulDay,err,cmessage)            ! output = julian day (fraction of day) + error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
  ! compute the time since the start of the year (in fractional days)
  fracJulDay = currentJulDay - startJulDay
  ! set timing of current forcing vector (in seconds since reference day)
  ! NOTE: It is a bit silly to have time information for each HRU and GRU
  hru_data%forcStruct%var(iLookFORCE%time) = (currentJulDay-refJulDay)*secprday

  ! compute the number of days in the current year
  yearLength = 365
  if(mod(hru_data%timeStruct%var(iLookTIME%iyyy),4) == 0)then
    yearLength = 366
    if(mod(hru_data%timeStruct%var(iLookTIME%iyyy),100) == 0)then
    yearLength = 365
    if(mod(hru_data%timeStruct%var(iLookTIME%iyyy),400) == 0)then
      yearLength = 366
    end if
    end if
  end if

  ! test
  if(checkTime)then
    write(*,'(i4,1x,4(i2,1x),f9.3,1x,i4)')  hru_data%timeStruct%var(iLookTIME%iyyy),           & ! year
                                            hru_data%timeStruct%var(iLookTIME%im),             & ! month
                                            hru_data%timeStruct%var(iLookTIME%id),             & ! day
                                            hru_data%timeStruct%var(iLookTIME%ih),             & ! hour
                                            hru_data%timeStruct%var(iLookTIME%imin),           & ! minute
                                            fracJulDay,                          & ! fractional julian day for the current time step
                                            yearLength                             ! number of days in the current year
    !pause ' checking time'
  end if
end subroutine computeTimeForcingHRU


! Set the HRU's relative and absolute tolerances
subroutine setIDATolerances(handle_hru_data,    &
                            relTolTempCas,      &
                            absTolTempCas,      &
                            relTolTempVeg,      &
                            absTolTempVeg,      &
                            relTolWatVeg,       &
                            absTolWatVeg,       &
                            relTolTempSoilSnow, &
                            absTolTempSoilSnow, &
                            relTolWatSnow,      &
                            absTolWatSnow,      &
                            relTolMatric,       &
                            absTolMatric,       &
                            relTolAquifr,       &
                            absTolAquifr) bind(C, name="setIDATolerances")
  USE data_types,only:var_dlength
  USE var_lookup,only:iLookPARAM

  implicit none

  type(c_ptr), intent(in), value          :: handle_hru_data    !  model time data
  real(c_double),intent(in)               :: relTolTempCas
  real(c_double),intent(in)               :: absTolTempCas
  real(c_double),intent(in)               :: relTolTempVeg
  real(c_double),intent(in)               :: absTolTempVeg
  real(c_double),intent(in)               :: relTolWatVeg
  real(c_double),intent(in)               :: absTolWatVeg
  real(c_double),intent(in)               :: relTolTempSoilSnow
  real(c_double),intent(in)               :: absTolTempSoilSnow
  real(c_double),intent(in)               :: relTolWatSnow
  real(c_double),intent(in)               :: absTolWatSnow
  real(c_double),intent(in)               :: relTolMatric
  real(c_double),intent(in)               :: absTolMatric
  real(c_double),intent(in)               :: relTolAquifr
  real(c_double),intent(in)               :: absTolAquifr
  ! local variables
  type(hru_type),pointer                  :: hru_data          !  model time data

  call c_f_pointer(handle_hru_data, hru_data)

  hru_data%mparStruct%var(iLookPARAM%relTolTempCas)%dat(1)       = relTolTempCas 
  hru_data%mparStruct%var(iLookPARAM%absTolTempCas)%dat(1)       = absTolTempCas
  hru_data%mparStruct%var(iLookPARAM%relTolTempVeg)%dat(1)       = relTolTempVeg
  hru_data%mparStruct%var(iLookPARAM%absTolTempVeg)%dat(1)       = absTolTempVeg
  hru_data%mparStruct%var(iLookPARAM%relTolWatVeg)%dat(1)        = relTolWatVeg
  hru_data%mparStruct%var(iLookPARAM%absTolWatVeg)%dat(1)        = absTolWatVeg
  hru_data%mparStruct%var(iLookPARAM%relTolTempSoilSnow)%dat(1)  = relTolTempSoilSnow
  hru_data%mparStruct%var(iLookPARAM%absTolTempSoilSnow)%dat(1)  = absTolTempSoilSnow
  hru_data%mparStruct%var(iLookPARAM%relTolWatSnow)%dat(1)       = relTolWatSnow
  hru_data%mparStruct%var(iLookPARAM%absTolWatSnow)%dat(1)       = absTolWatSnow
  hru_data%mparStruct%var(iLookPARAM%relTolMatric)%dat(1)        = relTolMatric
  hru_data%mparStruct%var(iLookPARAM%absTolMatric)%dat(1)        = absTolMatric
  hru_data%mparStruct%var(iLookPARAM%relTolAquifr)%dat(1)        = relTolAquifr
  hru_data%mparStruct%var(iLookPARAM%absTolAquifr)%dat(1)        = absTolAquifr
end subroutine setIDATolerances

end module hru_actor