module gru_struc_module
  USE, intrinsic :: iso_c_binding
  USE globalData,only:integerMissing
  implicit none

  ! public::f_initGruStruc
  public::read_dimension_fortran
  public::read_icond_nlayers_fortran
  public::get_num_hru_per_gru_fortran
  public::deallocate_gru_struc_fortran
  contains


! subroutine f_initGruStruc() bind(C, name="f_initGruStruc")




! end subroutine f_initGruStruc

subroutine read_dimension_fortran(start_gru, num_gru, num_hru, file_gru, &
                                  file_hru, err, message_r) &
    bind(C, name="read_dimension_fortran")
  USE globalData,only:startGRU                               ! index of the GRU for a single GRU run
  USE globalData,only:checkHRU                               ! index of the HRU for a single HRU run
  USE globalData,only:iRunMode                               ! define the current running mode    
  USE globalData,only:iRunModeFull, iRunModeGRU, iRunModeHRU ! define the running modes
  
  USE summaFileManager,only:SETTINGS_PATH, LOCAL_ATTRIBUTES
  USE read_attrb_module,only:read_dimension
  
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
  character(len=256)              :: attrFile           ! attributes file name
  character(len=256)              :: message
  
  err = 0
  message = ""
  call f_c_string_ptr(trim(message), message_r)

  ! Set variables that were previosuly set by getCommandArguments()
  startGRU=start_gru
  iRunMode=iRunModeGRU
  checkHRU=integerMissing

  attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)
  if (iRunMode == iRunModeGRU) then
    call read_dimension(trim(attrFile),file_gru,file_hru,num_gru,num_hru,err,&
                        message,startGRU=start_gru)
  else
    err = 20
    message = "Error: iRunMode is not set to iRunModeGRU - other modes not supported yet."
  end if
  if (err /= 0) then; call f_c_string_ptr(trim(message), message_r); end if
end subroutine read_dimension_fortran

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