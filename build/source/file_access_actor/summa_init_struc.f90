module summa_init_struc
  USE iso_c_binding
  USE nrtype
  USE summa_type, only:summa1_type_dec                        ! master summa data type
  implicit none
  public :: f_allocate
  public :: f_paramSetup
  public :: f_readRestart
  public :: f_getInitTolerance 
  public :: f_deallocateInitStruc
  ! Used to get all the inital conditions for the model -- allows calling summa_setup.f90
  type(summa1_type_dec),allocatable,save,public :: init_struc 

  contains
subroutine f_allocate(num_gru, err, message_r) bind(C, name="f_allocate")
  USE globalData,only:structInfo            ! information on the data structures
  USE globalData,only:gru_struc                               ! gru-hru mapping structures
  USE globalData,only:time_meta, &                       
                      forc_meta, &
                      attr_meta, &
                      type_meta, &
                      prog_meta, &
                      diag_meta, &
                      flux_meta, &
                      id_meta,   &
                      mpar_meta, &
                      indx_meta, &
                      bpar_meta, &
                      bvar_meta
#ifdef V4_ACTIVE
  USE globalData,only:lookup_meta
#endif
  ! statistics metadata structures
  USE globalData,only:statForc_meta, &        ! child metadata for stats
                      statProg_meta, &        ! child metadata for stats
                      statDiag_meta, &        ! child metadata for stats
                      statFlux_meta, &        ! child metadata for stats
                      statIndx_meta, &        ! child metadata for stats
                      statBvar_meta           ! child metadata for stats
  USE allocspace_module,only:allocGlobal      ! module to allocate space for global data structures
  USE allocspace_module,only:allocLocal
  USE globalData,only:startTime,finshTime,refTime,oldTime
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  implicit none
  ! dummy variables
  integer(c_int),       intent(in)        :: num_gru
  integer(c_int),       intent(out)       :: err
  type(c_ptr),          intent(out)       :: message_r
  ! local variables
  integer(i4b)                            :: iStruct,iGRU      ! looping variables
  character(len=256)                      :: message           ! error message
  character(len=256)                      :: cmessage          ! error message

  ! Start of subroutine
  message = ""
  call f_c_string_ptr(trim(message), message_r)
  allocate(init_struc)
  summaVars: associate(&
#ifdef V4_ACTIVE  
    lookupStruct         =>init_struc%lookupStruct         , & ! x%gru(:)%hru(:)%z(:)%var(:)%lookup(:) -- lookup tables
#endif
    ! statistics structures
    forcStat             => init_struc%forcStat            , & ! x%gru(:)%hru(:)%var(:)%dat -- model forcing data
    progStat             => init_struc%progStat            , & ! x%gru(:)%hru(:)%var(:)%dat -- model prognostic (state) variables
    diagStat             => init_struc%diagStat            , & ! x%gru(:)%hru(:)%var(:)%dat -- model diagnostic variables
    fluxStat             => init_struc%fluxStat            , & ! x%gru(:)%hru(:)%var(:)%dat -- model fluxes
    indxStat             => init_struc%indxStat            , & ! x%gru(:)%hru(:)%var(:)%dat -- model indices
    bvarStat             => init_struc%bvarStat            , & ! x%gru(:)%var(:)%dat        -- basin-average variables

    ! primary data structures (scalars)
    timeStruct           => init_struc%timeStruct          , & ! x%var(:)                   -- model time data
    forcStruct           => init_struc%forcStruct          , & ! x%gru(:)%hru(:)%var(:)     -- model forcing data
    attrStruct           => init_struc%attrStruct          , & ! x%gru(:)%hru(:)%var(:)     -- local attributes for each HRU
    typeStruct           => init_struc%typeStruct          , & ! x%gru(:)%hru(:)%var(:)     -- local classification of soil veg etc. for each HRU
    idStruct             => init_struc%idStruct            , & ! x%gru(:)%hru(:)%var(:)     --

    ! primary data structures (variable length vectors)
    indxStruct           => init_struc%indxStruct          , & ! x%gru(:)%hru(:)%var(:)%dat -- model indices
    mparStruct           => init_struc%mparStruct          , & ! x%gru(:)%hru(:)%var(:)%dat -- model parameters
    progStruct           => init_struc%progStruct          , & ! x%gru(:)%hru(:)%var(:)%dat -- model prognostic (state) variables
    diagStruct           => init_struc%diagStruct          , & ! x%gru(:)%hru(:)%var(:)%dat -- model diagnostic variables
    fluxStruct           => init_struc%fluxStruct          , & ! x%gru(:)%hru(:)%var(:)%dat -- model fluxes

    ! basin-average structures
    bparStruct           => init_struc%bparStruct          , & ! x%gru(:)%var(:)            -- basin-average parameters
    bvarStruct           => init_struc%bvarStruct          , & ! x%gru(:)%var(:)%dat        -- basin-average variables

    ! ancillary data structures
    dparStruct           => init_struc%dparStruct          , &  ! x%gru(:)%hru(:)%var(:)     -- default model parameters

     ! run time variables
    computeVegFlux       => init_struc%computeVegFlux      , & ! flag to indicate if we are computing fluxes over vegetation (.false. means veg is buried with snow)
    dt_init              => init_struc%dt_init             , & ! used to initialize the length of the sub-step for each HRU
    upArea               => init_struc%upArea              , & ! area upslope of each HRU
    
    ! miscellaneous variables
    nGRU                 => init_struc%nGRU              , & ! number of grouped response units
    nHRU                 => init_struc%nHRU              , & ! number of global hydrologic response units
    hruCount             => init_struc%hruCount              & ! number of local hydrologic response units
  )

  ! allocate other data structures
  do iStruct=1,size(structInfo)
    ! allocate space
    select case(trim(structInfo(iStruct)%structName))
      case('time'); call allocGlobal(time_meta,  timeStruct,  err, cmessage)   ! model forcing data
      case('forc'); call allocGlobal(forc_meta,  forcStruct,  err, cmessage)   ! model forcing data
      case('attr'); call allocGlobal(attr_meta,  attrStruct,  err, cmessage)   ! local attributes for each HRU
      case('type'); call allocGlobal(type_meta,  typeStruct,  err, cmessage)   ! local classification of soil veg etc. for each HRU
      case('id'  ); call allocGlobal(id_meta,    idStruct,    err, message)    ! local values of hru and gru IDs
      case('mpar'); call allocGlobal(mpar_meta,  mparStruct,  err, cmessage)   ! model parameters
      case('indx'); call allocGlobal(indx_meta,  indxStruct,  err, cmessage)   ! model variables
      case('prog'); call allocGlobal(prog_meta,  progStruct,  err, cmessage)   ! model prognostic (state) variables
      case('diag'); call allocGlobal(diag_meta,  diagStruct,  err, cmessage)   ! model diagnostic variables
      case('flux'); call allocGlobal(flux_meta,  fluxStruct,  err, cmessage)   ! model fluxes
      case('bpar'); call allocGlobal(bpar_meta,  bparStruct,  err, cmessage)   ! basin-average parameters
      case('bvar'); call allocGlobal(bvar_meta,  bvarStruct,  err, cmessage)   ! basin-average variables
#ifdef V4_ACTIVE
      case('lookup'); call allocGlobal(lookup_meta, lookupStruct, err, cmessage) ! lookup tables
#endif      
      case('deriv'); cycle
      case default; err=20; message='unable to find structure name: '//trim(structInfo(iStruct)%structName)
    end select
    ! check errors
    if(err/=0)then
      message=trim(message)//trim(cmessage)//'[structure =  '//trim(structInfo(iStruct)%structName)//']'
      call f_c_string_ptr(trim(message), message_r)
      return
    endif
  end do  ! looping through data structures
  
  ! allocate space for default model parameters
  ! NOTE: This is done here, rather than in the loop above, because dpar is not one of the "standard" data structures
  call allocGlobal(mpar_meta,dparStruct,err,cmessage)   ! default model parameters
  if(err/=0)then
    message=trim(message)//trim(cmessage)//' [problem allocating dparStruct]'
    call f_c_string_ptr(trim(message), message_r)
    return
  endif

  ! allocate space for the time step and computeVegFlux flags (recycled for each GRU for subsequent model calls)
  allocate(dt_init%gru(num_gru),upArea%gru(num_gru),computeVegFlux%gru(num_gru),stat=err)
  if(err/=0)then
    message=trim(message)//'problem allocating space for dt_init, upArea, or computeVegFlux [GRU]'
    call f_c_string_ptr(trim(message), message_r)
    return
  endif

   ! allocate space for the HRUs
  do iGRU=1,num_gru
    hruCount = gru_struc(iGRU)%hruCount  ! gru_struc populated in "read_dimension"
    allocate(dt_init%gru(iGRU)%hru(hruCount),upArea%gru(iGRU)%hru(hruCount),&
             computeVegFlux%gru(iGRU)%hru(hruCount),stat=err)
    if(err/=0)then
      message='problem allocating space for dt_init, upArea, or computeVegFlux [HRU]'
      call f_c_string_ptr(trim(message), message_r)
      return
    endif
  end do

  nGRU = num_gru
  nHRU = sum(gru_struc%hruCount)
  
  end associate summaVars

  ! Allocate the time structures
  call allocLocal(time_meta, startTime, err=err, message=message)
  call allocLocal(time_meta, finshTime, err=err, message=message)
  call allocLocal(time_meta, refTime,   err=err, message=message)
  call allocLocal(time_meta, oldTime,   err=err, message=message)
  if(err/=0)then; call f_c_string_ptr(trim(message), message_r); return; endif

end subroutine f_allocate

subroutine f_paramSetup(err, message_r) bind(C, name="f_paramSetup")
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  USE summa_setup,only:summa_paramSetup
  implicit none
  ! dummy variables
  integer(c_int),       intent(out)       :: err
  type(c_ptr),          intent(out)       :: message_r
  ! local variables
  character(len=256)                      :: message           ! error message

  message = ''
  call f_c_string_ptr(trim(message), message_r)

  call summa_paramSetup(init_struc, err, message)
  call f_c_string_ptr(trim(message), message_r)

end subroutine f_paramSetup

subroutine f_readRestart(err, message_r) bind(C, name="f_readRestart")
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  USE summa_restart,only:summa_readRestart
  implicit none
  ! dummy variables
  integer(c_int),       intent(out)       :: err
  type(c_ptr),          intent(out)       :: message_r
  ! local variables
  character(len=256)                      :: message           ! error message

  message = ''
  call f_c_string_ptr(trim(message), message_r)
  call summa_readRestart(init_struc, err, message)
  call f_c_string_ptr(trim(message), message_r)

end subroutine f_readRestart

subroutine f_getInitTolerance(rtol, atol, rtol_temp_cas, rtol_temp_veg, rtol_wat_veg, &
            rtol_temp_soil_snow, rtol_wat_snow, rtol_matric, rtol_aquifr, atol_temp_cas, &
            atol_temp_veg, atol_wat_veg, atol_temp_soil_snow, atol_wat_snow, atol_matric, & 
            atol_aquifr, def_tol) &
    bind(C, name="f_getInitTolerance")
   USE globalData,only:model_decisions                         ! model decision structure
  USE var_lookup,only:iLookDECISIONS
  USE var_lookup,only:iLookPARAM
  implicit none
  ! dummy variables
  real(c_double),       intent(out)       :: rtol 
  real(c_double),       intent(out)       :: atol
  real(c_double),       intent(out)       :: rtol_temp_cas
  real(c_double),       intent(out)       :: rtol_temp_veg
  real(c_double),       intent(out)       :: rtol_wat_veg
  real(c_double),       intent(out)       :: rtol_temp_soil_snow
  real(c_double),       intent(out)       :: rtol_wat_snow
  real(c_double),       intent(out)       :: rtol_matric
  real(c_double),       intent(out)       :: rtol_aquifr
  real(c_double),       intent(out)       :: atol_temp_cas
  real(c_double),       intent(out)       :: atol_temp_veg
  real(c_double),       intent(out)       :: atol_wat_veg
  real(c_double),       intent(out)       :: atol_temp_soil_snow
  real(c_double),       intent(out)       :: atol_wat_snow
  real(c_double),       intent(out)       :: atol_matric
  real(c_double),       intent(out)       :: atol_aquifr
  logical(c_bool),       intent(out)       :: def_tol

  rtol = -9999
  atol = -9999
  rtol_temp_cas = -9999
  rtol_temp_veg = -9999
  rtol_temp_soil_snow = -9999
  rtol_wat_snow = -9999
  rtol_wat_veg = -9999
  rtol_matric = -9999
  rtol_aquifr = -9999
  atol_temp_cas = -9999
  atol_temp_veg = -9999
  atol_temp_soil_snow = -9999
  atol_wat_snow = -9999
  atol_wat_veg = -9999
  atol_matric = -9999
  atol_aquifr = -9999
  def_tol = true
#ifdef V4_ACTIVE
  if (model_decisions(iLookDECISIONS%num_method)%iDecision == 83) then
    rtol = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%rTol)%dat(1)
    atol = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%aTol)%dat(1)
    rtol_temp_cas = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%relTolTempCas)%dat(1)
    rtol_temp_veg = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%relTolTempveg)%dat(1)
    rtol_wat_snow = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%relTolWatSnow)%dat(1)
    rtol_temp_soil_snow = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%relTolTempSoilSnow)%dat(1)
    rtol_wat_veg = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%relTolWatVeg)%dat(1)
    rtol_matric = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%relTolMatric)%dat(1)
    rtol_aquifr = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%relTolAquifr)%dat(1)
    atol_temp_cas = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%absTolTempCas)%dat(1)
    atol_temp_veg = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%absTolTempVeg)%dat(1)
    atol_wat_snow = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%absTolWatSnow)%dat(1)
    atol_temp_soil_snow = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%absTolTempSoilSnow)%dat(1)
    atol_wat_veg = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%absTolWatVeg)%dat(1)
    atol_matric = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%absTolMatric)%dat(1)
    atol_aquifr = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%absTolAquifr)%dat(1)
    def_tol = init_struc%mparStruct%gru(1)%hru(1)%var(iLookPARAM%defaultTol)%dat(1) == 1

#endif

end subroutine f_getInitTolerance

subroutine f_deallocateInitStruc() bind(C, name="f_deallocateInitStruc")
  USE globalData,only:startTime,finshTime,refTime,oldTime
  implicit none
  if(allocated(init_struc)) then; deallocate(init_struc); endif
  if(allocated(startTime%var)) then; deallocate(startTime%var); endif
  if(allocated(finshTime%var)) then; deallocate(finshTime%var); endif
  if(allocated(refTime%var)) then; deallocate(refTime%var); endif
  if(allocated(oldTime%var)) then; deallocate(oldTime%var); endif
  
end subroutine f_deallocateInitStruc

end module summa_init_struc