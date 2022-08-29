module cppwrap_hru

USE,intrinsic :: iso_c_binding
USE nrtype
USE data_types
USE globalData

implicit none
! public::Initialize ! This is called Directly by the actors code
! The above is here just in case I look here again
! public::SetupParam ! This is now called directly by the actors code as setupHRUParam
public::Restart
public::Forcing
public::RunPhysics
public::DeallocateStructures
public::Write_Param_C

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
! public subroutine Forcing: 
! **********************************************************************************************************
subroutine Forcing(& 
  indxGRU,              & ! index of the GRU into gru_struc
  step_index,           &
  handle_timeStruct,    &
  handle_forcStruct,    &
  iFile,                &
  forcingStep,          & ! index of current time step in current forcing file
  fracJulDay,           &
  tmZoneOffsetFracDay,  &
  yearLength,           &
  err) bind(C,name='Forcing')

  USE summaActors_forcing,only:summaActors_readForcing


  implicit none
  ! calling variables

  ! primary data structures (variable length vectors)
  integer(c_int),intent(in)         :: indxGRU           ! Index of the GRU in gru_struc
  integer(c_int),intent(in)         :: step_index
  type(c_ptr),intent(in),value      :: handle_timeStruct ! model indices
  type(c_ptr),intent(in),value      :: handle_forcStruct ! model parameters
  integer(c_int),intent(inout)      :: iFile             ! index of current forcing file from forcing file list 
  integer(c_int),intent(inout)      :: forcingStep       ! index of current time step in current forcing file
  real(c_double),intent(inout)      :: fracJulDay
  real(c_double),intent(inout)      :: tmZoneOffsetFracDay
  integer(c_int),intent(inout)      :: yearLength
  integer(c_int),intent(out)        :: err
  !---------------------------------------------------------------------------------------------------  
  ! local variables
  ! define the primary data structures (variable length vectors)
  type(var_i),pointer               :: timeStruct                 !  model time data
  type(var_d),pointer               :: forcStruct                 !  model forcing data
  character(len=256)                :: message
  ! getting data
  call c_f_pointer(handle_timeStruct, timeStruct)
  call c_f_pointer(handle_forcStruct, forcStruct)
  err = 0
  ! define global data (parameters, metadata)
  call summaActors_readForcing(&
    indxGRU,                  &
    step_index,               &
    timeStruct,               &
    forcStruct,               &
    forcingStep,              & ! index of current time step in current forcing file
    iFile,                    &
    fracJulDay,               &
    tmZoneOffsetFracDay,      &
    yearLength,               &
    err, message)
  if(err/=0)then
    message=trim(message)
    print*, message
  endif

end subroutine Forcing

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

! **********************************************************************************************************
! public subroutine WriteOutput: Writes output to appropriate NetCDF output files
! **********************************************************************************************************
subroutine WriteOutput(&
  indxHRU,                &
  indxGRU,                &
  step_index,             &
  ! statistics structures
  handle_forcStat,        & !  model forcing data
  handle_progStat,        & !  model prognostic (state) variables
  handle_diagStat,        & !  model diagnostic variables
  handle_fluxStat,        & !  model fluxes
  handle_indxStat,        & !  model indices
  handle_bvarStat,        & !  basin-average variables
  handle_timeStruct,      &
  handle_forcStruct,      &
  handle_attrStruct,      &
  handle_typeStruct,      &
  ! primary data structures (variable length vectors)
  handle_indxStruct,      &
  handle_mparStruct,      &
  handle_progStruct,      &
  handle_diagStruct,      &
  handle_fluxStruct,      &
  ! basin-average structures
  handle_bparStruct,      &
  handle_bvarStruct,      &
  ! local HRU variables
  handle_statCounter,     & 
  handle_outputTimeStep,  &
  handle_resetStats,      &
  handle_finalizeStats,   &
  handle_finshTime,       & ! end time for the model simulation
  handle_oldTime,         & ! time for the previous model time step
  outputStep,             &
  ! run time variables
  err) bind(C, name='WriteOutput')

  use summaActors_writeOutputStruc,only:summaActors_writeToOutputStruc

  implicit none
  integer(c_int), intent(in)        :: indxHRU
  integer(c_int), intent(in)        :: indxGRU
  integer(c_int), intent(in)        :: step_index

  ! statistics variables
  type(c_ptr), intent(in), value    :: handle_forcStat !  model forcing data
  type(c_ptr), intent(in), value    :: handle_progStat !  model prognostic (state) variables
  type(c_ptr), intent(in), value    :: handle_diagStat !  model diagnostic variables
  type(c_ptr), intent(in), value    :: handle_fluxStat !  model fluxes
  type(c_ptr), intent(in), value    :: handle_indxStat !  model indices
  type(c_ptr), intent(in), value    :: handle_bvarStat !  basin-average variables
  ! primary data structures (scalars)
  type(c_ptr), intent(in), value    :: handle_timeStruct !  model time data
  type(c_ptr), intent(in), value    :: handle_forcStruct !  model forcing data
  type(c_ptr), intent(in), value    :: handle_attrStruct !  local attributes for each HRU
  type(c_ptr), intent(in), value    :: handle_typeStruct !  local classification of soil veg etc. for each HRU
  ! primary data structures (variable length vectors)
  type(c_ptr), intent(in), value    :: handle_indxStruct !  model indices
  type(c_ptr), intent(in), value    :: handle_mparStruct !  model parameters
  type(c_ptr), intent(in), value    :: handle_progStruct !  model prognostic (state) variables
  type(c_ptr), intent(in), value    :: handle_diagStruct !  model diagnostic variables
  type(c_ptr), intent(in), value    :: handle_fluxStruct !  model fluxes
  ! basin-average structures
  type(c_ptr), intent(in), value    :: handle_bparStruct !  basin-average parameters
  type(c_ptr), intent(in), value    :: handle_bvarStruct !  basin-average variables
  ! local HRU variables
  type(c_ptr), intent(in), value    :: handle_statCounter
  type(c_ptr), intent(in), value    :: handle_outputTimeStep
  type(c_ptr), intent(in), value    :: handle_resetStats
  type(c_ptr), intent(in), value    :: handle_finalizeStats
  type(c_ptr), intent(in), value    :: handle_finshTime    ! end time for the model simulation
  type(c_ptr), intent(in), value    :: handle_oldTime      ! time for the previous model time step
  integer(c_int), intent(in)        :: outputStep
  ! run time variables
  integer(c_int)                    :: err

  ! local variables
  type(var_dlength),pointer         :: forcStat          !  model forcing data
  type(var_dlength),pointer         :: progStat          !  model prognostic (state) variables
  type(var_dlength),pointer         :: diagStat          !  model diagnostic variables
  type(var_dlength),pointer         :: fluxStat          !  model fluxes
  type(var_dlength),pointer         :: indxStat          !  model indices
  type(var_dlength),pointer         :: bvarStat          !  basin-average variabl
  ! primary data structures (scalars)
  type(var_i),pointer               :: timeStruct        !  model time data
  type(var_d),pointer               :: forcStruct        !  model forcing data
  type(var_d),pointer               :: attrStruct        !  local attributes for each HRU
  type(var_i),pointer               :: typeStruct        !  local classification of soil veg etc. for each HRU
  ! primary data structures (variable length vectors)
  type(var_ilength),pointer         :: indxStruct        !  model indices
  type(var_dlength),pointer         :: mparStruct        !  model parameters
  type(var_dlength),pointer         :: progStruct        !  model prognostic (state) variables
  type(var_dlength),pointer         :: diagStruct        !  model diagnostic variables
  type(var_dlength),pointer         :: fluxStruct        !  model fluxes
  ! basin-average structures
  type(var_d),pointer               :: bparStruct        !  basin-average parameters
  type(var_dlength),pointer         :: bvarStruct        !  basin-average variables 
  ! local hru data
  type(var_i),pointer               :: statCounter
  type(var_i),pointer               :: outputTimeStep
  type(flagVec),pointer             :: resetStats
  type(flagVec),pointer             :: finalizeStats
  type(var_i),pointer               :: finshTime         ! end time for the model simulation
  type(var_i),pointer               :: oldTime           ! time for the previous model time step
  character(len=256)                :: message

  call c_f_pointer(handle_forcStat, forcStat)
  call c_f_pointer(handle_progStat, progStat)
  call c_f_pointer(handle_diagStat, diagStat)
  call c_f_pointer(handle_fluxStat, fluxStat)
  call c_f_pointer(handle_indxStat, indxStat)
  call c_f_pointer(handle_bvarStat, bvarStat)
  call c_f_pointer(handle_timeStruct, timeStruct)
  call c_f_pointer(handle_forcStruct, forcStruct)
  call c_f_pointer(handle_attrStruct, attrStruct)
  call c_f_pointer(handle_typeStruct, typeStruct)
  call c_f_pointer(handle_indxStruct, indxStruct)
  call c_f_pointer(handle_mparStruct, mparStruct)
  call c_f_pointer(handle_progStruct, progStruct)
  call c_f_pointer(handle_diagStruct, diagStruct)
  call c_f_pointer(handle_fluxStruct, fluxStruct)
  call c_f_pointer(handle_bparStruct, bparStruct)
  call c_f_pointer(handle_bvarStruct, bvarStruct)
  call c_f_pointer(handle_statCounter, statCounter)
  call c_f_pointer(handle_outputTimeStep, outputTimeStep)
  call c_f_pointer(handle_resetStats, resetStats)
  call c_f_pointer(handle_finalizeStats, finalizeStats)
  call c_f_pointer(handle_finshTime, finshTime);
  call c_f_pointer(handle_oldTime, oldTime)

  call summaActors_writeToOutputStruc(&
    indxHRU,            &
    indxGRU,            &
    step_index,         &
    ! statistics variables
    forcStat,           & ! model forcing data
    progStat,           & ! model prognostic (state) variables
    diagStat,           & ! model diagnostic variables
    fluxStat,           & ! model fluxes
    indxStat,           & ! model indices
    bvarStat,           & ! basin-average variables
    ! primary data structures (scalars)
    timeStruct,         & ! x%var(:)     -- model time data
    forcStruct,         & ! x%var(:)     -- model forcing data
    attrStruct,         & ! x%var(:)     -- local attributes for each HRU
    typeStruct,         & ! x%var(:)     -- local classification of soil veg etc. for each HRU
      ! primary data structures (variable length vectors)
    indxStruct,         & ! x%var(:)%dat -- model indices
    mparStruct,         & ! x%var(:)%dat -- model parameters
    progStruct,         & ! x%var(:)%dat -- model prognostic (state) variables
    diagStruct,         & ! x%var(:)%dat -- model diagnostic variables
    fluxStruct,         & ! x%var(:)%dat -- model fluxes
    ! basin-average structures
    bparStruct,         & ! x%var(:)     -- basin-average parameters
    bvarStruct,         & ! x%var(:)%dat -- basin-average variables
    ! local HRU data
    statCounter,        & ! x%var(:)
    outputTimeStep,     & ! x%var(:)
    resetStats,         & ! x%var(:)
    finalizeStats,      & ! x%var(:)
    finshTime,          & ! x%var(:)     -- end time for the model simulation 
    oldTime,            & ! x%var(:)     -- time for the previous model time step
    outputStep,         &
    ! run time variables
    err, message)

  if(err/=0)then
    message=trim(message)
    print*, message
  endif

end subroutine WriteOutput

subroutine DeallocateStructures(&
  handle_forcStat,      &
  handle_progStat,      &
  handle_diagStat,      &
  handle_fluxStat,      &
  handle_indxStat,      &
  handle_bvarStat,      &
  handle_timeStruct,    &
  handle_forcStruct,    &
  handle_attrStruct,    &
  handle_typeStruct,    &
  handle_idStruct,      &
  handle_indxStruct,    &
  handle_mparStruct,    &
  handle_progStruct,    &
  handle_diagStruct,    &
  handle_fluxStruct,    &
  handle_bparStruct,    &
  handle_bvarStruct,    &
  handle_dparStruct,    &
  handle_startTime,     &
  handle_finshTime,     &
  handle_refTime,       &
  handle_oldTime,       &
  handle_ncid,          &
  handle_statCounter,   &
  handle_outputTimeStep,&
  handle_resetStats,    &
  handle_finalizeStats, &                        
  err) bind(C, name='DeallocateStructures')
  USE globalData,only:time_meta,forc_meta,attr_meta,type_meta ! metadata structures
  USE globalData,only:prog_meta,diag_meta,flux_meta,id_meta   ! metadata structures
  USE globalData,only:mpar_meta,indx_meta                     ! metadata structures
  USE globalData,only:bpar_meta,bvar_meta                     ! metadata structures
  ! statistics metadata structures
  USE globalData,only:statForc_meta                           ! child metadata for stats
  USE globalData,only:statProg_meta                           ! child metadata for stats
  USE globalData,only:statDiag_meta                           ! child metadata for stats
  USE globalData,only:statFlux_meta                           ! child metadata for stats
  USE globalData,only:statIndx_meta                           ! child metadata for stats
  USE globalData,only:statBvar_meta                           ! child metadata for stats
  ! maxvarFreq 


         
  type(c_ptr), intent(in), value  :: handle_forcStat !  model forcing data
  type(c_ptr), intent(in), value  :: handle_progStat !  model prognostic (state) variables
  type(c_ptr), intent(in), value  :: handle_diagStat !  model diagnostic variables
  type(c_ptr), intent(in), value  :: handle_fluxStat !  model fluxes
  type(c_ptr), intent(in), value  :: handle_indxStat !  model indices
  type(c_ptr), intent(in), value  :: handle_bvarStat !  basin-average variables
  ! primary data structures (scalars)
  type(c_ptr), intent(in), value  :: handle_timeStruct !  model time data
  type(c_ptr), intent(in), value  :: handle_forcStruct !  model forcing data
  type(c_ptr), intent(in), value  :: handle_attrStruct !  local attributes for each HRU
  type(c_ptr), intent(in), value  :: handle_typeStruct !  local classification of soil veg etc. for each HRU
  type(c_ptr), intent(in), value  :: handle_idStruct ! 
  ! primary data structures (variable length vectors)
  type(c_ptr), intent(in), value  :: handle_indxStruct !  model indices
  type(c_ptr), intent(in), value  :: handle_mparStruct !  model parameters
  type(c_ptr), intent(in), value  :: handle_progStruct !  model prognostic (state) variables
  type(c_ptr), intent(in), value  :: handle_diagStruct !  model diagnostic variables
  type(c_ptr), intent(in), value  :: handle_fluxStruct !  model fluxes
  ! basin-average structures
  type(c_ptr), intent(in), value  :: handle_bparStruct !  basin-average parameters
  type(c_ptr), intent(in), value  :: handle_bvarStruct !  basin-average variables
  ! ancillary data structures
  type(c_ptr), intent(in), value  :: handle_dparStruct !  default model parameters
  ! local hru data structures
  type(c_ptr), intent(in), value  :: handle_startTime  ! start time for the model simulation
  type(c_ptr), intent(in), value  :: handle_finshTime ! end time for the model simulation
  type(c_ptr), intent(in), value  :: handle_refTime    ! reference time for the model simulation
  type(c_ptr), intent(in), value  :: handle_oldTime
  type(c_ptr), intent(in), value  :: handle_ncid
  type(c_ptr), intent(in), value  :: handle_statCounter
  type(c_ptr), intent(in), value  :: handle_outputTimeStep
  type(c_ptr), intent(in), value  :: handle_resetStats
  type(c_ptr), intent(in), value  :: handle_finalizeStats


  integer(c_int), intent(out)           :: err

  ! statistics structures
  type(var_dlength),pointer              :: forcStat                   !  model forcing data
  type(var_dlength),pointer              :: progStat                   !  model prognostic (state) variables
  type(var_dlength),pointer              :: diagStat                   !  model diagnostic variables
  type(var_dlength),pointer              :: fluxStat                   !  model fluxes
  type(var_dlength),pointer              :: indxStat                   !  model indices
  type(var_dlength),pointer              :: bvarStat                   !  basin-average variabl
  ! primary data structures (scalars)
  type(var_i),pointer                    :: timeStruct                 !  model time data
  type(var_d),pointer                    :: forcStruct                 !  model forcing data
  type(var_d),pointer                    :: attrStruct                 !  local attributes for each HRU
  type(var_i),pointer                    :: typeStruct                 !  local classification of soil veg etc. for each HRU
  type(var_i8),pointer                   :: idStruct                   ! 
  ! primary data structures (variable length vectors)
  type(var_ilength),pointer              :: indxStruct                 !  model indices
  type(var_dlength),pointer              :: mparStruct                 !  model parameters
  type(var_dlength),pointer              :: progStruct                 !  model prognostic (state) variables
  type(var_dlength),pointer              :: diagStruct                 !  model diagnostic variables
  type(var_dlength),pointer              :: fluxStruct                 !  model fluxes
  ! basin-average structures
  type(var_d),pointer                    :: bparStruct                 !  basin-average parameters
  type(var_dlength),pointer              :: bvarStruct                 !  basin-average variables
  ! ancillary data structures
  type(var_d),pointer                    :: dparStruct                 !  default model parameters
  ! local HRU data structures
  type(var_i),pointer                    :: startTime                  ! start time for the model simulation
  type(var_i),pointer                    :: finshTime                  ! end time for the model simulation
  type(var_i),pointer                    :: refTime                    ! reference time for the model simulation
  type(var_i),pointer                    :: oldTime                    ! time for the previous model time step 


  type(var_i),pointer                    :: ncid_c
  type(var_i),pointer                    :: statCounter
  type(var_i),pointer                    :: outputTimeStep
  type(flagVec),pointer                  :: finalizeStats
  type(flagVec),pointer                  :: resetStats  
  ! local Variables
  integer(i4b)                           :: iStruct

  call c_f_pointer(handle_forcStat, forcStat);             
  call c_f_pointer(handle_progStat, progStat);             
  call c_f_pointer(handle_diagStat, diagStat);             
  call c_f_pointer(handle_fluxStat, fluxStat);             
  call c_f_pointer(handle_indxStat, indxStat);             
  call c_f_pointer(handle_bvarStat, bvarStat);             
  call c_f_pointer(handle_timeStruct, timeStruct);         
  call c_f_pointer(handle_forcStruct, forcStruct);         
  call c_f_pointer(handle_attrStruct, attrStruct);         
  call c_f_pointer(handle_typeStruct, typeStruct);         
  call c_f_pointer(handle_idStruct, idStruct);             
  call c_f_pointer(handle_indxStruct, indxStruct);         
  call c_f_pointer(handle_mparStruct, mparStruct);         
  call c_f_pointer(handle_progStruct, progStruct);         
  call c_f_pointer(handle_diagStruct, diagStruct);         
  call c_f_pointer(handle_fluxStruct, fluxStruct);         
  call c_f_pointer(handle_bparStruct, bparStruct);         
  call c_f_pointer(handle_bvarStruct, bvarStruct);         
  call c_f_pointer(handle_dparStruct, dparStruct);         
  call c_f_pointer(handle_startTime, startTime);              
  call c_f_pointer(handle_finshTime, finshTime);           
  call c_f_pointer(handle_refTime, refTime);               
  call c_f_pointer(handle_oldTime, oldTime);               
  call c_f_pointer(handle_ncid, ncid_c);                   
  call c_f_pointer(handle_statCounter, statCounter);       
  call c_f_pointer(handle_outputTimeStep, outputTimeStep); 
  call c_f_pointer(handle_resetStats, resetStats);         
  call c_f_pointer(handle_finalizeStats, finalizeStats);   

  ! Deallocate time variables
  call DeallocateData(time_meta, startTime, err); deallocate(startTime)
  call DeallocateData(time_meta, finshTime, err); deallocate(finshTime)
  call DeallocateData(time_meta, refTime, err);   deallocate(refTime)
  call DeallocateData(time_meta, oldTime, err);   deallocate(oldTime)

  ! Deallocate variable structures
  do iStruct=1, size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
    case('time'); call DeallocateData(time_meta,timeStruct,err); deallocate(timeStruct);
    case('forc'); call DeallocateData(forc_meta,forcStruct,err); deallocate(forcStruct);
    case('attr'); call DeallocateData(attr_meta,attrStruct,err); deallocate(attrStruct);
    case('type'); call DeallocateData(type_meta,typeStruct,err); deallocate(typeStruct);
    case('id'  ); call DeallocateData(id_meta,idStruct,err);     deallocate(idStruct);
    case('mpar'); call DeallocateData(mpar_meta,mparStruct,err); deallocate(mparStruct);
    case('indx'); call DeallocateData(indx_meta,indxStruct,err); deallocate(indxStruct);
    case('prog'); call DeallocateData(prog_meta,progStruct,err); deallocate(progStruct);
    case('diag'); call DeallocateData(diag_meta,diagStruct,err); deallocate(diagStruct);
    case('flux'); call DeallocateData(flux_meta,fluxStruct,err); deallocate(fluxStruct);
    case('bpar'); call DeallocateData(bpar_meta,bparStruct,err); deallocate(bparStruct);
    case('bvar'); call DeallocateData(bvar_meta,bvarStruct,err); deallocate(bvarStruct);
    case('deriv'); cycle;
    case default; err=1;
    end select
    ! check errors
    if(err/=0)then; return; endif
  end do

  ! Deallocate dpar as it is not part of structInfo
  call DeallocateData(mpar_meta,dparStruct,err); deallocate(dparStruct);

  ! Deallocate statistics Structures
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
    case('forc'); call DeallocateData(statForc_meta(:)%var_info,forcStat,err); deallocate(forcStat);
    case('prog'); call DeallocateData(statProg_meta(:)%var_info,progStat,err); deallocate(progStat);
    case('diag'); call DeallocateData(statDiag_meta(:)%var_info,diagStat,err); deallocate(diagStat);
    case('flux'); call DeallocateData(statFlux_meta(:)%var_info,fluxStat,err); deallocate(fluxStat);
    case('indx'); call DeallocateData(statIndx_meta(:)%var_info,indxStat,err); deallocate(indxStat);
    case('bvar'); call DeallocateData(statBvar_meta(:)%var_info,bvarStat,err); deallocate(bvarStat);
    case default; cycle
    end select
    if(err/=0)then; return; end if
  end do


  ! Deallocate the rest
  call DeallocateData(time_meta, ncid_c, err); deallocate(ncid_c)
  call DeallocateData(time_meta, statCounter ,err); deallocate(statCounter)
  call DeallocateData(time_meta, outputTimeStep ,err); deallocate(outputTimeStep)
  deallocate(finalizeStats)
  deallocate(resetStats)



end subroutine DeallocateStructures

!********************************************************************************
! Private Subroutine: DeallocateData for deallocating the data portion of a
! allocated pointers
!********************************************************************************
subroutine DeallocateData(metaStruct, dataStruct, err)
  type(var_info),intent(in)   :: metaStruct(:) 
  class(*),intent(inout)      :: dataStruct
  integer(i4b)                :: err

  integer(i4b)                :: nVars
  integer(i4b)                :: iVar

  select type(dataStruct)
    class is (var_i)
      if(allocated(dataStruct%var)) then
        deallocate(dataStruct%var)
      else
        err=2 ! structure was not allocated
      endif

    class is (var_i8)
      if(allocated(dataStruct%var)) then
        deallocate(dataStruct%var) 
      else
        err=3 ! structure was not allocated
      endif

    class is (var_d)
      if(allocated(dataStruct%var)) then
        deallocate(dataStruct%var)
      else
        err=4 ! structure was not allocated
      endif

    class is (var_ilength)
      if(allocated(dataStruct%var)) then
        nVars = size(metaStruct)
        do iVar=1, nVars
          deallocate(dataStruct%var(iVar)%dat)
        end do
        deallocate(dataStruct%var)
      else
        err=5
      endif

    class is (var_dlength)
      if(allocated(dataStruct%var)) then
        nVars = size(metaStruct)
        do iVar=1, nVars
          deallocate(dataStruct%var(iVar)%dat)
        end do
          deallocate(dataStruct%var)
        else
          err=6
      endif
  end select

end subroutine DeallocateData

! **********************************************************************************************************
! public Subroutine write_param_c: called from C to call the fortran subroutine
! **********************************************************************************************************
subroutine Write_Param_C(&
      indxGRU,           &
      indxHRU,           &
      handle_attrStruct, &
      handle_typeStruct, &
      handle_mparStruct, &
      handle_bparStruct, &
      err) bind(C, name="Write_Param_C")

  USE outputStrucWrite_module,only:writeParm ! module to write model parameters
  USE globalData,only:attr_meta,type_meta,mpar_meta,bpar_meta ! meta structures needed for writeParam Call
  USE globalData,only:structInfo
  USE globalData,only:gru_struc
  implicit none

  ! Dummy Variables
  integer(c_int), intent(in)      :: indxGRU
  integer(c_int), intent(in)      :: indxHRU
  type(c_ptr), intent(in), value  :: handle_attrStruct
  type(c_ptr), intent(in), value  :: handle_typeStruct
  type(c_ptr), intent(in), value  :: handle_mparStruct
  type(c_ptr), intent(in), value  :: handle_bparStruct
  integer(c_int)                  :: err

  ! local variables
  type(var_d),      pointer       :: attrStruct
  type(var_i),      pointer       :: typeStruct
  type(var_dlength),pointer       :: mparStruct
  type(var_d),      pointer       :: bparStruct

  integer(i4b)                    :: iStruct
  character(len=256)              :: message
  character(len=256)              :: cmessage

  call c_f_pointer(handle_attrStruct, attrStruct)
  call c_f_pointer(handle_typeStruct, typeStruct)
  call c_f_pointer(handle_mparStruct, mparStruct)
  call c_f_pointer(handle_bparStruct, bparStruct)

  ! Error Control
  err=0; message="cppwrap_Write_Param_C"
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('attr'); call writeParm(indxGRU,indxHRU,gru_struc(indxGRU)%hruInfo(indxHRU)%hru_ix, &
                                  attrStruct,attr_meta,'attr',err,cmessage)
      case('type'); call writeParm(indxGRU,indxHRU,gru_struc(indxGRU)%hruInfo(indxHRU)%hru_ix, &
                                  typeStruct,type_meta,'type',err,cmessage)
      case('mpar'); call writeParm(indxGRU,indxHRU,gru_struc(indxGRU)%hruInfo(indxHRU)%hru_ix, &
                                  mparStruct,mpar_meta,'mpar',err,cmessage)
    end select
    if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
  end do  ! (looping through structures)

  ! write GRU parameters
  call writeParm(indxGRU,indxHRU,integerMissing,bparStruct,bpar_meta,'bpar',err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif 

  

end subroutine

end module cppwrap_hru