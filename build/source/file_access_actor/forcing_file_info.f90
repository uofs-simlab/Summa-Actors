module forcing_file_info
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE globalData,only:integerMissing   ! integer missing value
  USE data_types,only:dlength          ! global data structure for forcing data
  implicit none
  public::getNumFrocingFiles_fortran
  public::getFileInfoSizes_fortran
  public::getFileInfoCopy_fortran
  public::read_forcingFile
  public::openForcingFile
  public::freeForcingFiles_fortran

    
  type,public :: forcingFileData
    real(rkind), dimension (:,:), allocatable   :: dataFromFile  ! (hru, time)
  end type forcingFileData

  type,public :: var_forc
    type(forcingFileData), allocatable   :: var(:)       ! var(:)%dataFromFile(:,:)
    character(len=256)                   :: refTimeString
    real(rkind)                          :: convTime2Days
    integer(i4b)                         :: nVars
    integer(i4b),allocatable             :: var_ix(:)
    real(rkind)                          :: tmZoneOffsetFracDay
    real(rkind)                          :: refJulDay_data 
    integer(i4b)                         :: nTimeSteps    ! Number of Timesteps in the file
  end type var_forc


  ! Module Data Structures (global to hrus that import this module)
  type(var_forc),allocatable,save,public  :: forcingDataStruct(:) ! forcingDataStruct(:)%var(:)%dataFromFile(:,:)
  type(dlength),allocatable,save,public   :: vecTime(:)
  contains
! Initalize the fortran data structure and return the number of forcing files
subroutine getNumFrocingFiles_fortran(num_forcing_files) &
      bind(C, name="getNumFrocingFiles_fortran")
  USE globalData,only:forcFileInfo            ! Structure allocated by ffil info
  implicit none
  ! dummy variables
  integer(c_int), intent(out) :: num_forcing_files
  num_forcing_files = size(forcFileInfo)
end subroutine getNumFrocingFiles_fortran

! Get the sizes fo the vector components that make up a forcingFile
subroutine getFileInfoSizes_fortran(iFile, var_ix_size, data_id_size, &
    varName_size) bind(C, name="getFileInfoSizes_fortran")
  USE globalData,only:forcFileInfo
  implicit none
  integer(c_int),intent(in)  :: iFile
  integer(c_int),intent(out) :: var_ix_size
  integer(c_int),intent(out) :: data_id_size
  integer(c_int),intent(out) :: varName_size
  var_ix_size = size(forcFileInfo(iFile)%var_ix)
  data_id_size = size(forcFileInfo(iFile)%data_id)
  varName_size = size(forcFileInfo(iFile)%varName)
end subroutine getFileInfoSizes_fortran

! Get the file info for a specific file
subroutine getFileInfoCopy_fortran(iFile, filenmData, nVars, nTimeSteps, &
    varName_size, var_ix_size, data_id_size, var_name_arr, var_ix_arr, &
    data_id_arr, firstJulDay, convTime2Days) bind(C, name="getFileInfoCopy_fortran")
  USE globalData,only:forcFileInfo
  USE C_interface_module,only:f_c_string_ptr
  implicit none
  ! dummy variables
  integer(c_int),intent(in)   :: iFile
  type(c_ptr),intent(out)     :: filenmData
  integer(c_int),intent(out)  :: nVars
  integer(c_int),intent(out)  :: nTimeSteps
  integer(c_int),intent(in)   :: varName_size
  integer(c_int),intent(in)   :: var_ix_size
  integer(c_int),intent(in)   :: data_id_size  
  type(c_ptr),intent(out)     :: var_name_arr(varName_size)
  integer(c_int),intent(out)  :: var_ix_arr(var_ix_size)
  integer(c_int),intent(out)  :: data_id_arr(data_id_size)
  real(c_double),intent(out)  :: firstJulDay
  real(c_double),intent(out)  :: convTime2Days
  ! local variables
  integer(i4b)                :: i
  integer(i4b)                :: index
  character(len=256)          :: emptyString = ""
  
  call f_c_string_ptr(trim(forcFileInfo(iFile)%filenmData), filenmData)

  nVars = forcFileInfo(iFile)%nVars
  nTimeSteps = forcFileInfo(iFile)%nTimeSteps

  ! First just populate with an empty string
  do i=1, varName_size
    call f_c_string_ptr(trim(emptyString), var_name_arr(i))
  end do
  ! Then look for the actual values by index
  do i=1, size(forcFileInfo(iFile)%var_ix)
    index = forcFileInfo(iFile)%var_ix(i)
    if (index /= integerMissing) then
      call f_c_string_ptr(trim(forcFileInfo(iFile)%varName(index)), var_name_arr(index))
    end if
  end do 

  var_ix_arr(:) = forcFileInfo(iFile)%var_ix(:)
  data_id_arr(:) = forcFileInfo(iFile)%data_id(:)

  firstJulDay = forcFileInfo(iFile)%firstJulDay
  convTime2Days = forcFileInfo(iFile)%convTime2Days

end subroutine getFileInfoCopy_fortran

! Read an entire forcing file into the global structures defined in this module
subroutine read_forcingFile(iFile, startGRU, numGRU, err, message_r) &
    bind(C,name="read_forcingFile")
  USE netcdf                                             
  USE netcdf_util_module,only:nc_file_open        
  USE netcdf_util_module,only:nc_file_close  ! close netcdf file
  USE C_interface_module,only:f_c_string_ptr
  USE globalData,only:forcFileInfo              
  USE globalData,only:gru_struc
  USE globalData,only:forc_meta 
  USE var_lookup,only:iLookTIME,iLookFORCE      ! named variables to define structure elements
  USE summaFileManager,only:FORCING_PATH        ! path of the forcing data file
  USE globalData,only:ixHRUfile_min,ixHRUfile_max

  implicit none
  integer(c_int),intent(in)               :: iFile
  integer(c_int),intent(in)               :: startGRU
  integer(c_int),intent(in)               :: numGRU
  integer(c_int),intent(out)              :: err
  type(c_ptr), intent(out)                :: message_r
  ! local varibles
  integer(i4b)                            :: nHRUlocal            
  integer(i4b)                            :: iHRU_Global
  integer(i4b)                            :: varId
  integer(i4b)                            :: ncid
  integer(i4b)                            :: nFiles
  integer(i4b)                            :: nTimeSteps
  integer(i4b)                            :: numHRU
  integer(i4b)                            :: nVars
  integer(i4b)                            :: iVar
  integer(i4b)                            :: iNC
  integer(i4b)                            :: attLen           ! attribute length
  character(len=256)                      :: infile
  character(len=256)                      :: cmessage
  character(len = nf90_max_name)          :: varName          ! dimenison name
  logical(lgt),dimension(size(forc_meta)) :: checkForce       ! flags to check forcing data variables exist
  character(len=256)                      :: message          ! error message 

  nHRUlocal = sum(gru_struc(:)%hruCount)

  ! Start Procedure here
  err=0; message="read_forcingFile/"
  call f_c_string_ptr(trim(message),message_r)

  ! TODO: I wonder if I can wrap this in a shared pointer???
  nFiles=size(forcFileInfo(:))
  ! Allocate forcing data input Struct
  if (.not.allocated(forcingDataStruct))then
    allocate(forcingDataStruct(nFiles))
    ! Allocate timing variables from forcing File
    allocate(vecTime(nFiles))
  endif
 
  ! Files are assumed to be in the correct order
  infile=trim(FORCING_PATH)//trim(forcFileInfo(iFile)%filenmData)
  call openForcingFile(forcFileInfo(iFile),iFile,trim(infile),ncid,err,cmessage)
  if(err/=0)then;message=trim(message)//trim(cmessage);call f_c_string_ptr(trim(message),message_r);return; end if

  err = nf90_inq_varid(ncid,'time',varId);                              if(err/=nf90_noerr)then; message=trim(message)//'cannot find time variable/'//trim(nf90_strerror(err));call f_c_string_ptr(trim(message),message_r);return; endif
  err = nf90_inquire_attribute(ncid,varId,'units',len = attLen);        if(err/=nf90_noerr)then; message=trim(message)//'cannot find time units/'//trim(nf90_strerror(err));call f_c_string_ptr(trim(message),message_r);return; endif
  err = nf90_get_att(ncid,varid,'units',forcingDataStruct(iFile)%refTimeString);if(err/=nf90_noerr)then; message=trim(message)//'cannot read time units/'//trim(nf90_strerror(err));call f_c_string_ptr(trim(message),message_r);return; endif
  
  nTimeSteps = forcFileInfo(iFile)%nTimeSteps
  forcingDataStruct(iFile)%nTimeSteps = nTimeSteps
  if(.not.allocated(vecTime(iFile)%dat))then
    allocate(vecTime(iFile)%dat(nTimeSteps))
  end if

  ! Get Time Information
  err = nf90_inq_varid(ncid,'time',varId);
  if(err/=nf90_noerr)then; message=trim(message)//'trouble finding time variable/'//trim(nf90_strerror(err)); call f_c_string_ptr(trim(message),message_r); return; endif
  err = nf90_get_var(ncid,varId,vecTime(iFile)%dat(:),start=(/1/),count=(/nTimeSteps/))    
  if(err/=nf90_noerr)then; message=trim(message)//'trouble reading time variable/'//trim(nf90_strerror(err)); call f_c_string_ptr(trim(message),message_r); return; endif

  ! Need to loop through vars and add forcing data
  nVars = forcFileInfo(iFile)%nVars
  forcingDataStruct(iFile)%nVars = nVars
  if (.not.allocated(forcingDataStruct(iFile)%var))then
    allocate(forcingDataStruct(iFile)%var(nVars))
  endif
  if (.not.allocated(forcingDataStruct(iFile)%var_ix))then
    allocate(forcingDataStruct(iFile)%var_ix(nVars))
  endif
  forcingDataStruct(iFile)%var_ix(:) = integerMissing

  ! initialize flags for forcing data
  checkForce(:) = .false.
  checkForce(iLookFORCE%time) = .true.  ! time is handled separately
  do iNC=1,nVars
    ! populate var_ix so HRUs can access the values
    forcingDataStruct(iFile)%var_ix(iNC) = forcFileInfo(iFile)%var_ix(iNC)

    ! check variable is desired
    if(forcFileInfo(iFile)%var_ix(iNC)==integerMissing) cycle
          
    iVar = forcFileInfo(iFile)%var_ix(iNC)
    checkForce(iVar) = .true.
    if (.not.allocated(forcingDataStruct(iFile)%var(iVar)%dataFromFile))then
      allocate(forcingDataStruct(iFile)%var(iVar)%dataFromFile(nHRUlocal,nTimeSteps))
    endif

    ! Get Forcing Data
    ! get variable name for error reporting
    err=nf90_inquire_variable(ncid,iNC,name=varName)
    if(err/=nf90_noerr)then; message=trim(message)//'problem reading forcing variable name from netCDF: '//trim(nf90_strerror(err)); call f_c_string_ptr(trim(message),message_r); return; endif

    ! define global HRU
    iHRU_global = gru_struc(1)%hruInfo(1)%hru_nc
    numHRU = sum(gru_struc(:)%hruCount)
    

    err=nf90_get_var(ncid,forcFileInfo(iFile)%data_id(ivar), &
                     forcingDataStruct(iFile)%var(iVar)%dataFromFile, &
                     start=(/ixHRUfile_min,1/),count=(/nHRUlocal, nTimeSteps/))
    if(err/=nf90_noerr)then; message=trim(message)//'problem reading forcing data: '//trim(varName)//'/'//trim(nf90_strerror(err)); call f_c_string_ptr(trim(message),message_r); return; endif
  end do

  call nc_file_close(ncid,err,message)
  if(err/=0)then;message=trim(message)//trim(cmessage);call f_c_string_ptr(trim(message),message_r);return;end if  
end subroutine read_forcingFile

! *************************************************************************
! * open the NetCDF forcing file and get the time information
! *************************************************************************
subroutine openForcingFile(forc_file,iFile,infile,ncId,err,message)
  USE netcdf                                              ! netcdf capability
  USE netcdf_util_module,only:nc_file_open                ! open netcdf file
  USE data_types,only:file_info
  USE time_utils_module,only:fracDay                      ! compute fractional day
  USE time_utils_module,only:extractTime                  ! extract time info from units string
  USE time_utils_module,only:compJulDay                   ! convert calendar date to julian day
  USE globalData,only:ncTime                              ! time zone information from NetCDF file (timeOffset = longitude/15. - ncTimeOffset)
  USE globalData,only:utcTime                             ! all times in UTC (timeOffset = longitude/15. hours)
  USE globalData,only:localTime                           ! all times local (timeOffset = 0)
  USE globalData,only:refJulDay_data
  USE summafilemanager,only:NC_TIME_ZONE
  ! dummy variables
  type(file_info),intent(inout)     :: forc_file
  integer(i4b),intent(in)           :: iFile              ! index of current forcing file in forcing file list
  character(*) ,intent(in)          :: infile             ! input file
  integer(i4b) ,intent(out)         :: ncId               ! NetCDF ID
  integer(i4b) ,intent(out)         :: err                ! error code
  character(*) ,intent(out)         :: message            ! error message
  ! local variables
  character(len=256)                :: cmessage           ! error message for downwind routine
  integer(i4b)                      :: iyyy,im,id,ih,imin ! date
  integer(i4b)                      :: ih_tz,imin_tz      ! time zone information
  real(dp)                          :: dsec,dsec_tz       ! seconds
  integer(i4b)                      :: varId              ! variable identifier
  integer(i4b)                      :: mode               ! netcdf file mode
  integer(i4b)                      :: attLen             ! attribute length
  character(len=256)                :: refTimeString      ! reference time string
   
  ! initialize error control
  err=0; message='openForcingFile/'
  
  ! open file
  mode=nf90_NoWrite
  call nc_file_open(trim(infile),mode,ncid,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
  
  ! get definition of time data
  err = nf90_inq_varid(ncid,'time',varId);                       if(err/=nf90_noerr)then; message=trim(message)//'cannot find time variable/'//trim(nf90_strerror(err)); return; endif
  err = nf90_inquire_attribute(ncid,varId,'units',len = attLen); if(err/=nf90_noerr)then; message=trim(message)//'cannot find time units/'//trim(nf90_strerror(err));    return; endif
  err = nf90_get_att(ncid,varid,'units',refTimeString);          if(err/=nf90_noerr)then; message=trim(message)//'cannot read time units/'//trim(nf90_strerror(err));    return; endif
  
  ! define the reference time for the model simulation
  call extractTime(refTimeString,                        & ! input  = units string for time data
                   iyyy,im,id,ih,imin,dsec,              & ! output = year, month, day, hour, minute, second
                   ih_tz, imin_tz, dsec_tz,              & ! output = time zone information (hour, minute, second)
                   err,cmessage)                            ! output = error code and error message
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
   
  select case(trim(NC_TIME_ZONE))
    case('ncTime'); forcingDataStruct(iFile)%tmZoneOffsetFracDay = sign(1, ih_tz) * fracDay(ih_tz,   & ! time zone hour
                                                          imin_tz, & ! time zone minute
                                                          dsec_tz)                        ! time zone second
    case('utcTime');   forcingDataStruct(iFile)%tmZoneOffsetFracDay = 0._dp
    case('localTime'); forcingDataStruct(iFile)%tmZoneOffsetFracDay = 0._dp
    case default; err=20; message=trim(message)//'unable to identify time zone info option'; return
  end select ! (option time zone option)
   
  call compjulday(iyyy,im,id,ih,imin,dsec,                & ! output = year, month, day, hour, minute, second
                  refJulDay_data,err,cmessage)              ! output = julian day (fraction of day) + error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
  
  ! get the time multiplier needed to convert time to units of days
  select case( trim( refTimeString(1:index(refTimeString,' ')) ) )
    case('seconds') 
      forc_file%convTime2Days=86400._dp
      forcingDataStruct(iFile)%convTime2Days=86400._dp
    case('minutes') 
      forc_file%convTime2Days=1440._dp
      forcingDataStruct(iFile)%convTime2Days=1440._dp
    case('hours')
      forc_file%convTime2Days=24._dp
      forcingDataStruct(iFile)%convTime2Days=24._dp
    case('days')
      forc_file%convTime2Days=1._dp
      forcingDataStruct(iFile)%convTime2Days=1._dp
    case default;    message=trim(message)//'unable to identify time units'; err=20; return
  end select
   
end subroutine openForcingFile

subroutine freeForcingFiles_fortran() bind(C, name="freeForcingFiles_fortran")
  USE globalData,only:forcFileInfo
  implicit none
  if (allocated(forcFileInfo)) deallocate(forcFileInfo)
  if (allocated(forcingDataStruct)) deallocate(forcingDataStruct)
  if (allocated(vecTime)) deallocate(vecTime)
end subroutine freeForcingFiles_fortran

end module forcing_file_info
