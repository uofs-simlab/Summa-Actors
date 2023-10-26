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

module ffile_info_actors_module
USE nrtype
USE netcdf
USE data_types
USE actor_data_types,only:file_info_array
USE globalData,only:integerMissing
implicit none
private
public::ffile_info
private::getAndSetVars
private::popFileNames
private::popForcStructVars
private::getAndCheckDataStep
contains



 ! ************************************************************************************************
 ! public subroutine ffile_info: read information on model forcing files
 ! ************************************************************************************************
subroutine ffile_info(indxGRU,forcFileInfo,num_forcing_files,err,message)
  ! used to read metadata on the forcing data file
  USE ascii_util_module,only:file_open
  USE ascii_util_module,only:linewidth
  USE netcdf_util_module,only:nc_file_open           ! open netCDF file
  USE netcdf_util_module,only:netcdf_err             ! netcdf error handling function
  USE summaFileManager,only:SETTINGS_PATH            ! path for metadata files
  USE summaFileManager,only:FORCING_PATH             ! path for forcing files
  USE summaFileManager,only:FORCING_FILELIST         ! list of model forcing files
  USE globalData,only:forc_meta                      ! forcing metadata
  USE get_ixname_module,only:get_ixtime,get_ixforce  ! identify index of named variable
  USE ascii_util_module,only:get_vlines              ! get a vector of non-comment lines
  USE ascii_util_module,only:split_line              ! split a line into words
  USE time_utils_module,only:extractTime
  USE globalData,only:time_meta
  USE allocspace_module,only:allocLocal
  USE time_utils_module,only:extractTime     ! extract time info from units string
  USE summaFileManager,only: SIM_START_TM, SIM_END_TM! time info from control file module
  USE var_lookup,only:iLookTIME              ! named variables that identify indices in the time structures


  implicit none
  ! define input & output
  integer(i4b),intent(in)                  :: indxGRU          
  type(file_info_array),pointer,intent(in) :: forcFileInfo
  integer(i4b),intent(out)                 :: num_forcing_files
  integer(i4b),intent(out)                 :: err              ! error code
  character(*),intent(inout)               :: message          ! error message
  ! define local variables

  ! netcdf file i/o related
  integer(i4b)                             :: ncid             ! netcdf file id
  integer(i4b)                             :: mode             ! netCDF file open mode
  integer(i4b)                             :: dimId            ! netcdf dimension id
  character(LEN=nf90_max_name)             :: varName          ! character array of netcdf variable name
  integer(i4b)                             :: iNC              ! index of a variable in netcdf file
  integer(i4b)                             :: nvar             ! number of variables in netcdf local attribute file
  ! the rest
  character(LEN=linewidth),allocatable     :: dataLines(:)     ! vector of lines of information (non-comment lines)
  character(len=256)                       :: cmessage         ! error message for downwind routine
  character(LEN=256)                       :: infile           ! input filename
  integer(i4b)                             :: unt              ! file unit (free unit output from file_open)
  integer(i4b)                             :: ivar             ! index of model variable
  integer(i4b)                             :: iFile            ! counter for forcing files
  integer(i4b)                             :: nFile            ! number of forcing files in forcing file list
  integer(i4b)                             :: totalFiles       ! total number of forcing files defiend in the forcing file list
  integer(i4b)                             :: startIndx        ! total number of forcing files defiend in the forcing file list
  integer(i4b)                             :: file_nHRU        ! number of HRUs in current forcing file
  integer(i4b)                             :: nForcing         ! number of forcing variables
  real(dp)                                 :: dataStep_iFile   ! data step for a given forcing data file
  logical(lgt)                             :: xist             ! .TRUE. if the file exists
  ! Time Variables
  type(var_i)                              :: startTime
  type(var_i)                              :: forcingStart
  type(var_i)                              :: finishTime
  real(rkind)                              :: dsec,dsec_tz
  integer(i4b)                             :: ffinfo_index
  
  ! Start procedure here
  err=0; message="ffile_info/"
  ! ------------------------------------------------------------------------------------------------------------------
  ! (1) read from the list of forcing files
  ! ------------------------------------------------------------------------------------------------------------------
  
  ! build filename for forcing-file list file
  infile = trim(SETTINGS_PATH)//trim(FORCING_FILELIST)

  ! open file
  call file_open(trim(infile),unt,err,cmessage)
  if(err/=0)then;message=trim(message)//trim(cmessage);print*,message;return;end if;

  ! get a list of character strings from non-comment lines
  call get_vlines(unt,dataLines,err,cmessage)
  if(err/=0)then; err=20;message=trim(message)//trim(cmessage);print*,message;return;end if;

  nFile = size(dataLines)
    
  ! allocate space for forcing information
  if(allocated(forcFileInfo%ffile_list)) deallocate(forcFileInfo%ffile_list)
  allocate(forcFileInfo%ffile_list(nFile), stat=err)
  if(err/=0)then; err=20; message=trim(message)//'problem allocating space for forcFileInfo'; return; end if

  ! poputate the forcingInfo structure with filenames
  call popFileNames(forcFileInfo,dataLines,nFile,inFile,err,message)
  if(err/=0)then; err=20; message=trim(message);print*,message;return;endif

  ! close ascii file
  close(unit=unt,iostat=err); if(err/=0)then;message=trim(message)//'problem closing forcing file list'; return; end if

  ! ------------------------------------------------------------------------------------------------------------------
  ! (2) pull descriptive information from netcdf forcing file and check number of HRUs in each forcing file matches nHRU
  ! ------------------------------------------------------------------------------------------------------------------

  ! get the number of forcing variables
  nForcing = size(forc_meta)

  ! loop through files, and read descriptive information from each file
  do iFile=1,nFile

    ! ensure allocatable structure components are deallocated
    if(allocated(forcFileInfo%ffile_list(iFile)%data_id)) deallocate(forcFileInfo%ffile_list(iFile)%data_id)
    if(allocated(forcFileInfo%ffile_list(iFile)%varName)) deallocate(forcFileInfo%ffile_list(iFile)%varName)

    ! allocate space for structure components
    allocate(forcFileInfo%ffile_list(iFile)%data_id(nForcing), forcFileInfo%ffile_list(iFile)%varName(nForcing), stat=err)
    if(err/=0)then; err=41; message=trim(message)//"problemAllocateStructureElement"; return;endif;

    ! initialize variable ids to missing
    forcFileInfo%ffile_list(iFile)%data_id(:) = integerMissing

    ! build filename for actual forcing file
    infile = trim(FORCING_PATH)//trim(forcFileInfo%ffile_list(iFile)%filenmData)
    ! check if file exists
    inquire(file=trim(infile),exist=xist)
    if(.not.xist)then;message=trim(message)//"FileNotFound[file='"//trim(infile)//"']";print*,message;err=10;return;endif;

    ! open file
    mode=nf90_NoWrite
    call nc_file_open(trim(infile), mode, ncid, err, cmessage)
    if(err/=0)then;message=trim(message)//trim(cmessage);print*,message;return;endif;

    ! check how many variables are in the netCDF file
    ! populate forcFileInfo with variable information
    call getAndSetVars(ncid,forcFileInfo,iFile,nVar,err,cmessage)

    ! inquire nhru dimension size
    err = nf90_inq_dimid(ncid,'hru',dimId);                 if(err/=0)then; message=trim(message)//'cannot find dimension hru'; return; endif
    err = nf90_inquire_dimension(ncid,dimId,len=file_nHRU); if(err/=0)then; message=trim(message)//'cannot read dimension hru'; return; endif

    ! inquire time dimension size
    err = nf90_inq_dimid(ncid,'time',dimId);                                     if(err/=0)then; message=trim(message)//'cannot find dimension time'; return; end if
    err = nf90_inquire_dimension(ncid,dimId,len=forcFileInfo%ffile_list(iFile)%nTimeSteps); if(err/=0)then; message=trim(message)//'cannot read dimension time'; return; end if

    ! loop through all variables in netcdf file, check to see if everything needed to run the model exists and data_step is correct
    do iNC=1,nVar

      ! inquire about current variable name, type, number of dimensions
      err = nf90_inquire_variable(ncid,iNC,name=varName)
      if(err/=0)then; message=trim(message)//'problem inquiring variable: '//trim(varName); return; end if

      ! process variable
      select case(trim(varName))

        ! if variable is in the forcing vector
        case('time','pptrate','SWRadAtm','LWRadAtm','airtemp','windspd','airpres','spechum')
          
          call popForcStructVars(ncid,forcFileInfo,iFile,iNC,varName,err,message)
          if(err/=0)then;print*,message;return;endif

        case('data_step' )

          call getAndCheckDataStep(ncid,dataStep_iFile,iFile,err,message)
          if(err/=0)then;print*,message;return;endif
          
        ! HRU id -- required
        case('hruId')
          
          call setHRUID(ncid,indxGRU,varName,inFile,err,message)
          if(err/=0)then;print*,message;return;endif
       
        ! OK to have additional variables in the forcing file that are not used
        case default; cycle
      end select  ! select variable name
    end do ! (end of netcdf file variable loop)

    ! check to see if any forcing variables are missed
    if(any(forcFileInfo%ffile_list(iFile)%data_id(:)==integerMissing))then
      do iVar=1,size(forcFileInfo%ffile_list(iFile)%data_id)
        if(forcFileInfo%ffile_list(iFile)%data_id(iVar)==integerMissing)then; err=40; message=trim(message)//"variable missing [var='"//trim(forcFileInfo%ffile_list(iFile)%varname(iVar))//"']"; return; end if
      end do
    end if

    ! close file
    err = nf90_close(ncid)
    if(err/=nf90_noerr)then
      message=trim(message)//'trouble closing file '//trim(infile)
      print*, message
      return
    endif

  end do ! (loop through files)

  ! Get the number of forcing files we have
  num_forcing_files = size(forcFileInfo%ffile_list(:))
end subroutine ffile_info


  ! check how many variables are in the netCDF file
  ! populate forcFileInfo with variable information
subroutine getAndSetVars(ncid,forcFileInfo,iFile,nVar,err,message)
  USE netCDF
  USE netcdf_util_module,only:netcdf_err
  USE globalData,only:integerMissing

  implicit none
  integer(i4b),intent(in)                             :: ncid
  type(file_info_array),pointer,intent(in)            :: forcFileInfo
  integer(i4b),intent(in)                             :: iFile
  integer(i4b),intent(out)                            :: nVar
  integer(i4b),intent(out)                            :: err
  character(*),intent(inout)                          :: message
  
  ! how many variables are there?
  err = nf90_inquire(ncid, nvariables=nVar)
  call netcdf_err(err,message); if (err/=0) return

  ! set nVar attribute
  forcFileInfo%ffile_list(iFile)%nVars = nVar
  if(allocated(forcFileInfo%ffile_list(iFile)%var_ix))then;print*,"Already Allocated";endif;
  ! allocate space
  allocate(forcFileInfo%ffile_list(iFile)%var_ix(nVar), stat=err)
  if(err/=0)then;message=trim(message)//'problem allocating space for structure element';print*,message;err=20;return;endif;

  ! initialize data structure
  forcFileInfo%ffile_list(iFile)%var_ix(:) = integerMissing


end subroutine getAndSetVars

! Private subroutine that filles the array of forcing files with the names
! of the forcing files
subroutine popFileNames(forcFileInfo,dataLines,nFile,inFile,err,message)
  USE ascii_util_module,only:linewidth
  implicit none

  type(file_info_array),pointer                       :: forcFileInfo
  character(*),intent(out)                            :: dataLines(:) ! forcingFileList
  integer(i4b),intent(in)                             :: nFile        ! number of forcing files
  character(*),intent(inout)                          :: inFile       ! file that populated dataLines
  integer(i4b),intent(out)                            :: err
  character(*),intent(inout)                          :: message
  ! local variables
  character(LEN=256)                                  :: filenameData     ! name of forcing datafile
  integer(i4b)                                        :: iFile

  do iFile=1,nFile
    ! split the line into "words" (expect one word: the file describing forcing data for that index)
    read(dataLines(iFile),*,iostat=err) filenameData
    if(err/=0)then; message=trim(message)//'problem reading a line of data from file ['//trim(infile)//']'; return; end if
    ! set forcing file name attribute
    forcFileInfo%ffile_list(iFile)%filenmData = trim(filenameData)
  end do  ! (looping through files)


end subroutine popFileNames

! populate the variable indexes and names in ffile_list
subroutine popForcStructVars(ncid,forcFileInfo,iFile,iNC,varName,err,message)
  USE get_ixname_module,only:get_ixforce  ! identify index of named variable
  USE netCDF
  implicit none
  integer(i4b),intent(in)                              :: ncid
  type(file_info_array),pointer                        :: forcFileInfo
  integer(i4b),intent(in)                              :: iFile
  integer(i4b),intent(in)                              :: iNC ! index of a variable in netcdf file
  character(*),intent(in)                              :: varName
  integer(i4b),intent(out)                             :: err
  character(*),intent(inout)                           :: message
  ! local variables
  integer(i4b)                                         :: iVar
  
  
  ! get variable index
  ivar = get_ixforce(trim(varname))
  if(ivar < 0)then
    err=40
    message=trim(message)//"variableNotFound[var="//trim(varname)//"]"
    return
  end if
  if(ivar>size(forcFileInfo%ffile_list(iFile)%data_id))then
    err=35
    message=trim(message)//"indexOutOfRange[var="//trim(varname)//"]"
    return
  end if

  ! put netcdf file variable index in the forcing file metadata structure
  err = nf90_inq_varid(ncid, trim(varName), forcFileInfo%ffile_list(iFile)%data_id(ivar))
  if(err/=0)then; message=trim(message)//"problem inquiring forcing variable[var="//trim(varName)//"]"; return; end if

  ! put variable index of the forcing structure in the metadata structure
  if(trim(varName)/='time')then
    forcFileInfo%ffile_list(iFile)%var_ix(iNC)   = ivar
    forcFileInfo%ffile_list(iFile)%varName(ivar) = trim(varName)

  ! get first time from file, place into forcFileInfo
  else
    err = nf90_get_var(ncid,forcFileInfo%ffile_list(iFile)%data_id(ivar),forcFileInfo%ffile_list(iFile)%firstJulDay,start=(/1/))
    if(err/=0)then; message=trim(message)//'problem reading first Julian day'; return; end if
  end if  ! if the variable name is time

end subroutine popForcStructVars

! set the datastep from the first file
! check every file has the same data_step
subroutine getAndCheckDataStep(ncid,dataStep_iFile,iFile,err,message)
  USE globalData,only:data_step
  USE netCDF
  implicit none
  integer(i4b),intent(in)                           :: ncid
  real(dp),intent(out)                              :: dataStep_iFile   ! data step for a given forcing data file
  integer(i4b),intent(in)                           :: iFile            ! index of current forcing file
  integer(i4b),intent(out)                          :: err
  character(*),intent(inout)                        :: message
  ! local variables
  integer(i4b)                                      :: varid            ! netcdf variable id
                                      
  ! read data_step from netcdf file
  err = nf90_inq_varid(ncid, "data_step", varId); if(err/=0)then; message=trim(message)//'cannot find data_step'; return; end if
  err = nf90_get_var(ncid,varid,dataStep_iFile);  if(err/=0)then; message=trim(message)//'cannot read data_step'; return; end if

  ! check data_step is the same for all forcing files
  if(iFile == 1)then
    data_step = dataStep_iFile
  else
    if(abs(dataStep_iFile - data_step) > epsilon(dataStep_iFile))then
    write(message,'(a,i0,a)') trim(message)//'data step for forcing file ',iFile,'differs from the datastep of the first forcing file'
    err=20; return
    end if
  end if

end subroutine getAndCheckDataStep

! get the HRU_ID from the forcing file
! populate the gru_struc with the HRU ID
! Check if the hru is expected
subroutine setHRUID(ncid,indxGRU,varName,inFile,err,message)
  USE globalData,only:gru_struc                      ! gru-hru mapping structure
  USE netCDF
  implicit none
  integer(i4b),intent(in)                           :: ncid
  integer(i4b),intent(in)                           :: indxGRU
  character(*),intent(in)                           :: varName
  character(*),intent(in)                           :: inFile       ! file that populated dataLines
  integer(i4b),intent(out)                          :: err
  character(*),intent(inout)                        :: message
  ! local variables 
  integer(i4b)                                      :: localHRU_ix      ! index of HRU
  integer(i4b)                                      :: varid            ! netcdf variable id
  integer(8)                                        :: ncHruId(1)       ! hruID from the forcing files


  ! check to see if hruId exists as a variable, this is a required variable
  err = nf90_inq_varid(ncid,trim(varname),varId)
  if(err/=0)then; message=trim(message)//'hruID variable not present'; return; endif

  ! check that the hruId is what we expect
  ! NOTE: we enforce that the HRU order in the forcing files is the same as in the zLocalAttributes files (too slow otherwise)
  do localHRU_ix=1,gru_struc(indxGRU)%hruCount
    ! check the HRU is what we expect
    err = nf90_get_var(ncid,varId,ncHruId,start=(/gru_struc(indxGRU)%hruInfo(localHRU_ix)%hru_nc/),count=(/1/))
    if(gru_struc(indxGRU)%hruInfo(localHRU_ix)%hru_id /= ncHruId(1))then
      write(message,'(a,i0,a,i0,a,i0,a,a)') trim(message)//'hruId for global HRU: ',gru_struc(indxGRU)%hruInfo(localHRU_ix)%hru_nc,' - ',  &
          ncHruId(1), ' differs from the expected: ',gru_struc(indxGRU)%hruInfo(localHRU_ix)%hru_id, ' in file ', trim(infile)
      write(message,'(a)') trim(message)//' order of hruId in forcing file needs to match order in zLocalAttributes.nc'
      err=40; return
    endif
  end do

end subroutine

end module ffile_info_actors_module
