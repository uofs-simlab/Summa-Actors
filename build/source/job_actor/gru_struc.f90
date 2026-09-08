module gru_struc_module
  USE, intrinsic :: iso_c_binding
  USE nr_type
  USE globalData,only:integerMissing
  implicit none

  public::f_readDimension
  public::f_setHruCount
  public::f_readGridDimension
  public::f_setIndexMap
  public::f_getNumHru
  public::f_readIcondNlayers
  public::f_getNumHruPerGru
  public::f_deallocateGruStruc


  integer(8),allocatable,save,public     :: gru_id(:)
  integer(8),allocatable,save,public     :: hru_id(:)
  integer(8),allocatable,save,public     :: hru2gru_id(:)
  integer(i4b),allocatable,save,public   :: hru_ix(:)
  integer(i4b),allocatable,save,public   :: nGlac_file(:)   ! number of glaciers per GRU, read from the attributes file
  integer(i4b),allocatable,save,public   :: nWtld_file(:)   ! number of wetlands per GRU, read from the attributes file
  contains

subroutine f_readDimension(start_gru, num_gru, file_gru, file_hru, &
    err, message_r) bind(C, name="f_readDimension")
  USE globalData,only:startGRU                               ! index of the GRU for a single GRU run
  USE globalData,only:checkHRU                               ! index of the HRU for a single HRU run
  USE globalData,only:iRunMode                               ! define the current running mode    
  USE globalData,only:iRunModeFull, iRunModeGRU, iRunModeHRU ! define the running modes
  
  USE summaFileManager,only:SETTINGS_PATH, LOCAL_ATTRIBUTES
  
  USE netcdf
  USE netcdf_util_module,only:nc_file_open                   ! open netcdf file
  USE netcdf_util_module,only:nc_file_close                  ! close netcdf file

  USE globalData,only:gru_struc                              ! gru->hru mapping structure
  USE globalData,only:index_map                              ! hru->gru mapping structure
  USE nr_utils_module,only:arth

  USE C_interface_module,only:f_c_string_ptr
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)      :: start_gru
  integer(c_int), intent(inout)   :: num_gru
  integer(c_int), intent(out)     :: file_gru
  integer(c_int), intent(out)     :: file_hru
  integer(c_int), intent(out)     :: err
  type(c_ptr),    intent(out)     :: message_r
  ! Local Variables
  integer(i4b)                    :: ncID         ! NetCDF file ID
  integer(i4b)                    :: varID        ! NetCDF variable ID
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

  ! read per-GRU glacier / wetland counts (optional -- absent means none, for backwards compatibility)
  allocate(nGlac_file(file_gru),nWtld_file(file_gru))
  err = nf90_inq_varid(ncID,"nGlac",varID)
  if (err/=nf90_noerr) then
    nGlac_file = 0; err = nf90_noerr
  else
    err = nf90_get_var(ncID,varID,nGlac_file); if (err/=0) then; message=trim(message)//'problem reading nGlac'; return; end if
  end if
  err = nf90_inq_varid(ncID,"nWtld",varID)
  if (err/=nf90_noerr) then
    nWtld_file = 0; err = nf90_noerr
  else
    err = nf90_get_var(ncID,varID,nWtld_file); if (err/=0) then; message=trim(message)//'problem reading nWtld'; return; end if
  end if

  ! close netcdf file
  call nc_file_close(ncID,err,cmessage)
  if (err/=0) then; message=trim(message)//trim(cmessage); return; end if
  
  hru_ix=arth(1,1,file_hru)
  allocate(gru_struc(num_gru))

  if (err /= 0) then; call f_c_string_ptr(trim(message), message_r); end if
end subroutine f_readDimension

subroutine f_setHruCount(iGRU,sGRU) bind(C, name="f_setHruCount")
  USE globalData,only:gru_struc           ! gru->hru mapping structure
  USE nr_utils_module ,only:arth
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

  ! per-GRU glacier / wetland counts from the attributes file (grid dimensions are set later by
  ! f_readGridDimension, which must run serially after this parallel loop)
  gru_struc(iGRU)%nGlac = nGlac_file(iGRU+sGRU-1)
  gru_struc(iGRU)%nWtld = nWtld_file(iGRU+sGRU-1)
end subroutine f_setHruCount

! Read the glacier-grid dimensions (grid / xgrid / ygrid) and per-GRU gridInfo from the attributes
! file.  Mirrors read_dimension's grid block; the actor path builds gru_struc itself so it cannot
! call read_dimension directly.  Must be called after all f_setHruCount calls (needs %nGlac/%gru_nc).
subroutine f_readGridDimension(start_gru, num_gru, file_gru, err, message_r) &
    bind(C, name="f_readGridDimension")
  USE summaFileManager,only:SETTINGS_PATH, LOCAL_ATTRIBUTES
  USE netcdf
  USE netcdf_util_module,only:nc_file_open
  USE netcdf_util_module,only:nc_file_close
  USE globalData,only:gru_struc
  USE read_attrb_module,only:read_dimensionGrid
  USE C_interface_module,only:f_c_string_ptr
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)      :: start_gru
  integer(c_int), intent(in)      :: num_gru
  integer(c_int), intent(in)      :: file_gru
  integer(c_int), intent(out)     :: err
  type(c_ptr),    intent(out)     :: message_r
  ! Local Variables
  integer(i4b)                    :: ncID
  integer(i4b)                    :: iGRU
  character(len=256)              :: cmessage
  character(len=256)              :: attrFile
  character(len=256)              :: message

  err = 0; message = ""
  call f_c_string_ptr(trim(message), message_r)

  if(sum(gru_struc(1:num_gru)%nGlac) <= 0)then
    ! no glaciers anywhere in this run -- mirror read_dimension's no-grid baseline
    do iGRU = 1, num_gru
      gru_struc(iGRU)%nGrid = 0
      if(.not.allocated(gru_struc(iGRU)%gridInfo)) allocate(gru_struc(iGRU)%gridInfo(0))
    end do
    return
  endif

  attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)
  call nc_file_open(trim(attrFile),nf90_noWrite,ncID,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); call f_c_string_ptr(trim(message), message_r); return; end if

  call read_dimensionGrid(ncID, file_gru, num_gru, err, message)
  if(err/=0)then; call nc_file_close(ncID,err,cmessage); call f_c_string_ptr(trim(message), message_r); return; end if

  call nc_file_close(ncID,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); call f_c_string_ptr(trim(message), message_r); return; end if

end subroutine f_readGridDimension

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

subroutine f_getNumHru(num_hru) bind(C, name="f_getNumHru")
  USE globalData,only:gru_struc
  implicit none
  integer(c_int), intent(out)     :: num_hru
  num_hru = sum(gru_struc(:)%hruCount)
end subroutine f_getNumHru

subroutine f_readIcondNlayers(num_gru, err, message_r)&
    bind(C, name="f_readIcondNlayers")
  USE globalData,only:indx_meta                     ! metadata structures
  USE globalData,only:maxDOM                        ! maximum number of domains in any HRU (set here for def_output)

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
  integer(i4b)                    :: nDOM              ! max number of domains in any HRU (returned by read_icond_nlayers)

  err = 0
  message = ""
  nDOM = 0
  call f_c_string_ptr(trim(message), message_r)

  ! *****************************************************************************
  ! *** read the number of snow, lake, soil and glacier-ice layers per domain
  ! ***     also populates gru_struc(:)%hruInfo(:)%domInfo(:) and returns nDOM
  ! *****************************************************************************
  ! set restart filename and read the number of layers from the initial conditions (restart) file
  if(STATE_PATH == '') then
    restartFile = trim(SETTINGS_PATH)//trim(MODEL_INITCOND)
  else
    restartFile = trim(STATE_PATH)//trim(MODEL_INITCOND)
  endif
  call read_icond_nlayers(trim(restartFile),num_gru,nDOM,indx_meta,err,message)
  if(err/=0)then; call f_c_string_ptr(trim(message), message_r); return; endif

  ! make the maximum number of domains available globally (used when defining output files)
  maxDOM = nDOM

end subroutine f_readIcondNlayers

subroutine f_getNumHruPerGru(num_gru, num_hru_per_gru_array) &
    bind(C, name="f_getNumHruPerGru")
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
  
end subroutine f_getNumHruPerGru

subroutine f_deallocateGruStruc() bind(C, name="f_deallocateGruStruc")
    USE globalData,only:gru_struc           ! gru->hru mapping structure
    USE globalData,only:index_map
    implicit none
    if(allocated(gru_struc))then; deallocate(gru_struc);endif
    if(allocated(index_map))then; deallocate(index_map);endif
    if(allocated(gru_id))then; deallocate(gru_id);endif
    if(allocated(hru_id))then; deallocate(hru_id);endif
    if(allocated(hru2gru_id))then; deallocate(hru2gru_id);endif
    if(allocated(hru_ix))then; deallocate(hru_ix);endif
    if(allocated(nGlac_file))then; deallocate(nGlac_file);endif
    if(allocated(nWtld_file))then; deallocate(nWtld_file);endif
end subroutine

end module gru_struc_module