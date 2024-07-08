module gru_struc_module
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE globalData,only:integerMissing
  implicit none

  public::f_readDimension
  public::f_setHruCount
  public::f_setIndexMap
  public::read_icond_nlayers_fortran
  public::get_num_hru_per_gru_fortran
  public::deallocate_gru_struc_fortran


  integer(8),allocatable,save,public     :: gru_id(:)
  integer(8),allocatable,save,public     :: hru_id(:)
  integer(8),allocatable,save,public     :: hru2gru_id(:)
  integer(i4b),allocatable,save,public   :: hru_ix(:)
  contains

subroutine f_readDimension(start_gru, num_gru, num_hru, file_gru, &
                                  file_hru, err, message_r) &
    bind(C, name="f_readDimension")
  USE globalData,only:startGRU                               ! index of the GRU for a single GRU run
  USE globalData,only:checkHRU                               ! index of the HRU for a single HRU run
  USE globalData,only:iRunMode                               ! define the current running mode    
  USE globalData,only:iRunModeFull, iRunModeGRU, iRunModeHRU ! define the running modes
  
  USE summaFileManager,only:SETTINGS_PATH, LOCAL_ATTRIBUTES
  ! USE read_attrb_module,only:read_dimension
  
  USE netcdf
  USE netcdf_util_module,only:nc_file_open                   ! open netcdf file
  USE netcdf_util_module,only:nc_file_close                  ! close netcdf file

  USE globalData,only:gru_struc                             ! gru->hru mapping structure
  USE globalData,only:index_map                              ! hru->gru mapping structure
  USE nr_utility_module,only:arth

  USE C_interface_module,only:f_c_string_ptr
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)      :: start_gru
  integer(c_int), intent(inout)   :: num_gru
  integer(c_int), intent(inout)   :: num_hru
  integer(c_int), intent(out)     :: file_gru
  integer(c_int), intent(out)     :: file_hru
  integer(c_int), intent(out)     :: err
  type(c_ptr),    intent(out)     :: message_r
  ! Local Variables
  integer(i4b)                    :: ncID         ! NetCDF file ID
 integer(i4b)                     :: varID        ! NetCDF variable ID
  integer(i4b)                    :: gruDimId     ! variable id of GRU dimension from netcdf file
  integer(i4b)                    :: hruDimId     ! variable id of HRU dimension from netcdf file

  integer(i4b)                    :: iGRU     

  character(len=256)              :: cmessage     ! error message for downwind routine

  character(len=256)              :: attrFile     ! attributes file name
  character(len=256)              :: message
  
  err = 0
  message = ""
  call f_c_string_ptr(trim(message), message_r)

  ! Set variables that were previosuly set by getCommandArguments()
  startGRU=start_gru
  iRunMode=iRunModeGRU
  checkHRU=integerMissing

  attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)
  if (iRunMode /= iRunModeGRU) then
    err = 20
    message = "Error: iRunMode is not set to iRunModeGRU - other modes not supported yet."  
  end if


  ! open nc file
  call nc_file_open(trim(attrFile),nf90_noWrite,ncID,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if

  ! *********************************************************************************************
  ! read and set GRU dimensions
  ! **********************************************************************************************
  ! get gru dimension of whole file
  err = nf90_inq_dimid(ncID,"gru",gruDimId);                   if(err/=nf90_noerr)then; message=trim(message)//'problem finding gru dimension/'//trim(nf90_strerror(err)); return; end if
  err = nf90_inquire_dimension(ncID, gruDimId, len = file_gru); if(err/=nf90_noerr)then; message=trim(message)//'problem reading gru dimension/'//trim(nf90_strerror(err)); return; end if

  ! get hru dimension of whole file
  err = nf90_inq_dimid(ncID,"hru",hruDimId);                   if(err/=nf90_noerr)then; message=trim(message)//'problem finding hru dimension/'//trim(nf90_strerror(err)); return; end if
  err = nf90_inquire_dimension(ncID, hruDimId, len = file_hru); if(err/=nf90_noerr)then; message=trim(message)//'problem reading hru dimension/'//trim(nf90_strerror(err)); return; end if

  ! allocate space for GRU indices
  allocate(gru_id(file_gru))
  allocate(hru_ix(file_hru),hru_id(file_hru),hru2gru_id(file_hru))

  ! read gru_id from netcdf file
  err = nf90_inq_varid(ncID,"gruId",varID);     if (err/=0) then; message=trim(message)//'problem finding gruId'; return; end if
  err = nf90_get_var(ncID,varID,gru_id);        if (err/=0) then; message=trim(message)//'problem reading gruId'; return; end if

  ! read hru_id from netcdf file
  err = nf90_inq_varid(ncID,"hruId",varID);     if (err/=0) then; message=trim(message)//'problem finding hruId'; return; end if
  err = nf90_get_var(ncID,varID,hru_id);        if (err/=0) then; message=trim(message)//'problem reading hruId'; return; end if

  ! read hru2gru_id from netcdf file
  err = nf90_inq_varid(ncID,"hru2gruId",varID); if (err/=0) then; message=trim(message)//'problem finding hru2gruId'; return; end if
  err = nf90_get_var(ncID,varID,hru2gru_id);    if (err/=0) then; message=trim(message)//'problem reading hru2gruId'; return; end if
 
  ! close netcdf file
  call nc_file_close(ncID,err,cmessage)
  if (err/=0) then; message=trim(message)//trim(cmessage); return; end if
  
  hru_ix=arth(1,1,file_hru)
  allocate(gru_struc(num_gru))

  if (err /= 0) then; call f_c_string_ptr(trim(message), message_r); end if
end subroutine f_readDimension

subroutine f_setHruCount(iGRU,sGRU) bind(C, name="f_setHruCount")
  USE globalData,only:gru_struc           ! gru->hru mapping structure
  USE nr_utility_module ,only:arth
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)      :: iGRU
  integer(c_int), intent(in)      :: sGRU

  gru_struc(iGRU)%hruCount = count(hru2gru_id == gru_id(iGRU))                 ! number of HRUs in each GRU
  gru_struc(iGRU)%gru_id   = gru_id(iGRU+sGRU-1)
  gru_struc(iGRU)%gru_nc   = iGRU+sGRU-1 

  allocate(gru_struc(iGRU)%hruInfo(gru_struc(iGRU)%hruCount))
  gru_struc(iGRU)%hruInfo(:)%hru_nc = pack(hru_ix,hru2gru_id == gru_struc(iGRU)%gru_id)
  gru_struc(iGRU)%hruInfo(:)%hru_ix = arth(iGRU,1,gru_struc(iGRU)%hruCount)                    ! set index of hru in run domain
  gru_struc(iGRU)%hruInfo(:)%hru_id = hru_id(gru_struc(iGRU)%hruInfo(:)%hru_nc)                ! set id of hru
end subroutine f_setHruCount


subroutine f_setIndexMap() bind(C, name="f_setIndexMap")
  USE globalData,only:gru_struc,index_map
  implicit none
  ! Local Variables
  integer                         :: iGRU

  allocate(index_map(sum(gru_struc(:)%hruCount)))

  do iGRU = 1,sum(gru_struc(:)%hruCount)
    index_map(gru_struc(iGRU)%hruInfo(:)%hru_ix)%gru_ix   = iGRU                                 ! index of gru in run domain to which the hru belongs
    index_map(gru_struc(iGRU)%hruInfo(:)%hru_ix)%localHRU_ix = hru_ix(1:gru_struc(iGRU)%hruCount)! index of hru within the gru
  enddo ! iGRU = 1,nGRU

end subroutine f_setIndexMap

subroutine read_icond_nlayers_fortran(num_gru, err, message_r)& 
    bind(C, name="read_icond_nlayers_fortran")
  USE globalData,only:indx_meta                     ! metadata structures
  
  USE summaFileManager,only:SETTINGS_PATH,STATE_PATH,MODEL_INITCOND                    
  USE read_icond_module,only:read_icond_nlayers               ! module to read initial condition dimensions
  USE C_interface_module,only:f_c_string_ptr
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)      :: num_gru
  integer(c_int), intent(out)     :: err
  type(c_ptr),    intent(out)     :: message_r
  ! Local Variables
  character(len=256)              :: restartFile        ! restart file name
  character(len=256)              :: message

  err = 0
  message = ""
  call f_c_string_ptr(trim(message), message_r)

  ! *****************************************************************************
  ! *** read the number of snow and soil layers
  ! *****************************************************************************
  ! set restart filename and read the number of snow and soil layers from the initial conditions (restart) file
  if(STATE_PATH == '') then
    restartFile = trim(SETTINGS_PATH)//trim(MODEL_INITCOND)
  else
    restartFile = trim(STATE_PATH)//trim(MODEL_INITCOND)
  endif
  call read_icond_nlayers(trim(restartFile),num_gru,indx_meta,err,message)
  if(err/=0)then; call f_c_string_ptr(trim(message), message_r); endif

end subroutine read_icond_nlayers_fortran

subroutine get_num_hru_per_gru_fortran(num_gru, num_hru_per_gru_array) &
    bind(C, name="get_num_hru_per_gru_fortran")
  USE globalData,only:gru_struc           ! gru->hru mapping structure
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)      :: num_gru
  integer(c_int), intent(out)     :: num_hru_per_gru_array(num_gru)
  ! Local Variables
  integer                         :: iGRU

  do iGRU = 1, num_gru
    num_hru_per_gru_array(iGRU) = gru_struc(iGRU)%hruCount
  end do
  
end subroutine 

subroutine deallocate_gru_struc_fortran() bind(C, name="deallocate_gru_struc_fortran")
    USE globalData,only:gru_struc           ! gru->hru mapping structure
    USE globalData,only:index_map
    implicit none
    if(allocated(gru_struc))then; deallocate(gru_struc);endif
    if(allocated(index_map))then; deallocate(index_map);endif
end subroutine




end module gru_struc_module