module cppwrap_hru

USE,intrinsic :: iso_c_binding
USE nrtype
USE data_types
USE globalData

implicit none
public::Restart
public::RunPhysics

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

  use summa4chm_restart,only:summa4chm_readRestart           

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
  call summa4chm_readRestart(&
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

! **********************************************************************************************************
! public subroutine RunPhysics: solving coupled energy-mass equations for one timestep
! **********************************************************************************************************
subroutine RunPhysics(&
  indxHRU,                &
  step_index,             &
  ! primary data structures (scalars)
  handle_timeStruct,      & ! x%var(:)     -- model time data
  handle_forcStruct,      & ! x%var(:)     -- model forcing data
  handle_attrStruct,      & ! x%var(:)     -- local attributes for each HRU
  handle_typeStruct,      & ! x%var(:)     -- local classification of soil veg etc. for each HRU
  ! primary data structures (variable length vectors)
  handle_indxStruct,      & ! x%var(:)%dat -- model indices
  handle_mparStruct,      & ! x%var(:)%dat -- model parameters
  handle_progStruct,      & ! x%var(:)%dat -- model prognostic (state) variables
  handle_diagStruct,      & ! x%var(:)%dat -- model diagnostic variables
  handle_fluxStruct,      & ! x%var(:)%dat -- model fluxes
  ! basin-average structures
  handle_bvarStruct,      & ! x%var(:)%dat        -- basin-average variables
  handle_lookupStruct,    & 
  fracJulDay,             &
  tmZoneOffsetFracDay,    &
  yearLength,             &
  ! run time variables
  computeVegFlux,         & ! flag to indicate if we are computing fluxes over vegetation
  dt_init,                & ! used to initialize the length of the sub-step for each HRU
  dt_init_factor,         & ! Used to adjust the length of the timestep in the event of a failure
  err) bind(C, name='RunPhysics')

  USE SummaActors_modelRun,only:SummaActors_runPhysics

  !======= Declarations =========
  implicit none
  ! calling variables  
  integer(c_int), intent(in)        :: indxHRU
  integer(c_int), intent(in)        :: step_index
  ! primary data structures (scalars)
  type(c_ptr), intent(in), value    :: handle_timeStruct ! model time data
  type(c_ptr), intent(in), value    :: handle_forcStruct ! model forcing data
  type(c_ptr), intent(in), value    :: handle_attrStruct ! local attributes for each HRU
  type(c_ptr), intent(in), value    :: handle_typeStruct ! local classification of soil veg etc. for each HRU
  ! primary data structures (variable length vectors)
  type(c_ptr), intent(in), value    :: handle_indxStruct ! model indices
  type(c_ptr), intent(in), value    :: handle_mparStruct ! model parameters
  type(c_ptr), intent(in), value    :: handle_progStruct ! model prognostic (state) variables
  type(c_ptr), intent(in), value    :: handle_diagStruct ! model diagnostic variables
  type(c_ptr), intent(in), value    :: handle_fluxStruct ! model fluxes
  ! basin-average structures
  type(c_ptr), intent(in), value    :: handle_bvarStruct ! basin-average variables
  type(c_ptr), intent(in), value    :: handle_lookupStruct ! lookup tables
  real(c_double),intent(inout)      :: fracJulDay
  real(c_double),intent(inout)      :: tmZoneOffsetFracDay 
  integer(c_int),intent(inout)      :: yearLength
  integer(c_int),intent(inout)      :: computeVegFlux    ! flag to indicate if we are computing fluxes over vegetation
  real(c_double),intent(inout)      :: dt_init
  integer(c_int),intent(in)         :: dt_init_factor    ! Used to adjust the length of the timestep in the event of a failure
  integer(c_int), intent(out)       :: err

  ! local variables
  ! primary data structures (scalars)
  type(var_i),pointer                :: timeStruct                 !  model time data
  type(var_d),pointer                :: forcStruct                 !  model forcing data
  type(var_d),pointer                :: attrStruct                 !  local attributes for each HRU
  type(var_i),pointer                :: typeStruct                 !  local classification of soil veg etc. for each HRU
  ! primary data structures (variable length vectors)
  type(var_ilength),pointer          :: indxStruct                 !  model indices
  type(var_dlength),pointer          :: mparStruct                 !  model parameters
  type(var_dlength),pointer          :: progStruct                 !  model prognostic (state) variables
  type(var_dlength),pointer          :: diagStruct                 !  model diagnostic variables
  type(var_dlength),pointer          :: fluxStruct                 !  model fluxes
  ! basin-average structures
  type(var_dlength),pointer          :: bvarStruct                 !  basin-average variables 
  type(zLookup), pointer             :: lookupStruct               ! lookup tables
  character(len=256)                 :: message

  call c_f_pointer(handle_timeStruct, timeStruct)
  call c_f_pointer(handle_forcStruct, forcStruct)
  call c_f_pointer(handle_attrStruct, attrStruct)
  call c_f_pointer(handle_typeStruct, typeStruct)
  call c_f_pointer(handle_indxStruct, indxStruct)
  call c_f_pointer(handle_mparStruct, mparStruct)
  call c_f_pointer(handle_progStruct, progStruct)
  call c_f_pointer(handle_diagStruct, diagStruct)
  call c_f_pointer(handle_fluxStruct, fluxStruct)
  call c_f_pointer(handle_bvarStruct, bvarStruct)
  call c_f_pointer(handle_lookupStruct, lookupStruct)


  call SummaActors_runPhysics(&
    indxHRU,                & 
    step_index,             &
    ! primary data structures (scalars)
    timeStruct,             & ! x%var(:)     -- model time data
    forcStruct,             & ! x%var(:)     -- model forcing data
    attrStruct,             & ! x%var(:)     -- local attributes for each HRU
    typeStruct,             & ! x%var(:)     -- local classification of soil veg etc. for each HRU
    ! primary data structures (variable length vectors)
    indxStruct,             & ! x%var(:)%dat -- model indices
    mparStruct,             & ! x%var(:)%dat -- model parameters
    progStruct,             & ! x%var(:)%dat -- model prognostic (state) variables
    diagStruct,             & ! x%var(:)%dat -- model diagnostic variables
    fluxStruct,             & ! x%var(:)%dat -- model fluxes
    ! basin-average structures
    bvarStruct,             & ! x%var(:)%dat        -- basin-average variables
    lookupStruct,           &
    fracJulDay,             &
    tmZoneOffsetFracDay,    &
    yearLength,             &
    ! run time variables
    computeVegFlux,         & ! flag to indicate if we are computing fluxes over vegetation
    dt_init,                & ! used to initialize the length of the sub-step for each HRU
    dt_init_factor,         & ! Used to adjust the length of the timestep in the event of a failure
    err, message)

  if(err/=0)then
    message=trim(message)
    print*, message
  endif

end subroutine RunPhysics


end module cppwrap_hru