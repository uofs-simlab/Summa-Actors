module cppwrap_hru

USE,intrinsic :: iso_c_binding
USE nrtype
USE data_types
USE globalData

implicit none
public::Restart

contains



! **********************************************************************************************************
! public subroutine Restart: 
! **********************************************************************************************************
subroutine Restart(&
  indxGRU,            & !  index of GRU in gru_struc
  indxHRU,            & !  index of HRU in gru_struc
  ! primary data structures (variable length vectors)
  handle_indxStruct,  & !  model indices
  handle_mparStruct,  & !  model parameters
  handle_progStruct,  & !  model prognostic (state) variables
  handle_diagStruct,  & !  model diagnostic variables
  handle_fluxStruct,  & !  model fluxes
  ! basin-average structures
  handle_bvarStruct,  & !  basin-average variables
  dt_init,            &		
  err) bind(C,name='Restart')

  use summa_restart,only:summa_readRestart           

  implicit none
  ! calling variables
  integer(c_int), intent(in)        :: indxGRU           !  index of GRU in gru_struc
  integer(c_int), intent(in)        :: indxHRU           !  index of HRU in gru_struc  
  ! primary data structures (variable length vectors)
  type(c_ptr), intent(in), value    :: handle_indxStruct !  model indices
  type(c_ptr), intent(in), value    :: handle_mparStruct !  model parameters
  type(c_ptr), intent(in), value    :: handle_progStruct !  model prognostic (state) variables
  type(c_ptr), intent(in), value    :: handle_diagStruct !  model diagnostic variables
  type(c_ptr), intent(in), value    :: handle_fluxStruct !  model fluxes
  ! basin-average structures
  type(c_ptr), intent(in), value    :: handle_bvarStruct !  basin-average variables
  real(c_double), intent(inout)     :: dt_init
  integer(c_int), intent(inout)     :: err
  !---------------------------------------------------------------------------------------------------  
  ! local variables

  ! define the primary data structures (variable length vectors)
  type(var_ilength),pointer          :: indxStruct                 !  model indices
  type(var_dlength),pointer          :: mparStruct                 !  model parameters
  type(var_dlength),pointer          :: progStruct                 !  model prognostic (state) variables
  type(var_dlength),pointer          :: diagStruct                 !  model diagnostic variables
  type(var_dlength),pointer          :: fluxStruct                 !  model fluxes
  ! define the basin-average structures
  type(var_dlength),pointer          :: bvarStruct                 !  basin-average variables
  character(len=256)                 :: message


  ! getting data
  call c_f_pointer(handle_indxStruct, indxStruct)
  call c_f_pointer(handle_mparStruct, mparStruct)
  call c_f_pointer(handle_progStruct, progStruct)
  call c_f_pointer(handle_diagStruct, diagStruct)
  call c_f_pointer(handle_fluxStruct, fluxStruct)
  call c_f_pointer(handle_bvarStruct, bvarStruct)

  ! define global data (parameters, metadata)
  call summa_readRestart(&
    indxGRU,    & ! index of GRU in gru_struc
    indxHRU,    & ! index of HRU in gru_struc
      ! primary data structures (variable length vectors)
    indxStruct, & ! x%var(:)%dat -- model indices
    mparStruct, & ! x%var(:)%dat -- model parameters
    progStruct, & ! x%var(:)%dat -- model prognostic (state) variables
    diagStruct, & ! x%var(:)%dat -- model diagnostic variables
    fluxStruct, & ! x%var(:)%dat -- model fluxes
    ! basin-average structures
    bvarStruct, & ! x%var(:)%dat        -- basin-average variables
    dt_init,    & ! used to initialize the length of the sub-step for each HRU
    err, message)

  if(err/=0)then
  message=trim(message)
  print*, message
  endif

end subroutine Restart

end module cppwrap_hru