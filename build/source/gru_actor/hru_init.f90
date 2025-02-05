module INIT_HRU_ACTOR
! used to declare and allocate summa data structures and initialize model state to known values
USE,intrinsic :: iso_c_binding
USE nrtype          ! variable types, etc.
USE data_types,only:&
                    ! no spatial dimension
                    var_i,               & ! x%var(:)            (i4b)
                    var_i8,              & ! x%var(:)            (i8b)
                    var_d,               & ! x%var(:)            (dp)
                    var_ilength,         & ! x%var(:)%dat        (i4b)
                    var_dlength            ! x%var(:)%dat        (dp)
#ifdef V4_ACTIVE
USE data_types,only:zlookup               ! x%z(:)%var(:)%lookup(:) -- lookup tables
#endif
USE actor_data_types,only:hru_type             ! hru_type
                    
! access missing values
USE globalData,only:integerMissing   ! missing integer
USE globalData,only:realMissing      ! missing double precision number
! named variables for run time options
USE globalData,only:iRunModeFull,iRunModeGRU,iRunModeHRU
! metadata structures
USE globalData,only:time_meta,forc_meta,attr_meta,type_meta ! metadata structures
USE globalData,only:prog_meta,diag_meta,flux_meta,id_meta   ! metadata structures
USE globalData,only:mpar_meta,indx_meta                     ! metadata structures
USE globalData,only:bpar_meta,bvar_meta                     ! metadata structures
USE globalData,only:averageFlux_meta                        ! metadata for time-step average fluxes
#ifdef V4_ACTIVE
USE globalData,only:lookup_meta 
#endif
! statistics metadata structures
USE globalData,only:statForc_meta                           ! child metadata for stats
USE globalData,only:statProg_meta                           ! child metadata for stats
USE globalData,only:statDiag_meta                           ! child metadata for stats
USE globalData,only:statFlux_meta                           ! child metadata for stats
USE globalData,only:statIndx_meta                           ! child metadata for stats
USE globalData,only:statBvar_meta                           ! child metadata for stats
! maxvarFreq 
USE var_lookup,only:maxVarFreq                               ! # of available output frequencies
! named variables
USE var_lookup,only:iLookATTR                               ! look-up values for local attributes
USE var_lookup,only:iLookTYPE                               ! look-up values for classification of veg, soils etc.
USE var_lookup,only:iLookPARAM                              ! look-up values for local column model parameters
USE var_lookup,only:iLookID                                   ! look-up values for local column model parameters

USE var_lookup,only:iLookPROG                               ! look-up values for local column model prognostic (state) variables
USE var_lookup,only:iLookDIAG                               ! look-up values for local column model diagnostic variables
USE var_lookup,only:iLookFLUX                               ! look-up values for local column model fluxes
USE globalData,only:urbanVegCategory                        ! vegetation category for urban areas

! named variables to define LAI decisions
USE mDecisions_module,only:&
 monthlyTable,& ! LAI/SAI taken directly from a monthly table for different vegetation classes
 specified      ! LAI/SAI computed from green vegetation fraction and winterSAI and summerLAI parameters

! safety: set private unless specified otherwise
implicit none
private
public::initHRU
public::setupHRU
public::readHRURestart
contains
! **************************************************************************************************
! public subroutine initHRU: ! used to declare and allocate summa data structures and initialize model state to known values
! **************************************************************************************************
subroutine initHRU(indx_gru, indx_hru, hru_data, err, message)
  ! ---------------------------------------------------------------------------------------
  ! * desired modules
  ! ---------------------------------------------------------------------------------------
  ! data types
  USE nrtype                                                  ! variable types, etc.
  ! subroutines and functions: allocate space
  USE allocspace_module,only:allocLocal
  ! timing variables
  USE globalData,only:startInit,endInit                       ! date/time for the start and end of the initialization
  USE globalData,only:elapsedRead                             ! elapsed time for the data read
  USE globalData,only:elapsedWrite                            ! elapsed time for the stats/write
  USE globalData,only:elapsedPhysics                          ! elapsed time for the physics
  ! miscellaneous global data
  USE globalData,only:gru_struc                               ! gru-hru mapping structures
  USE globalData,only:structInfo                              ! information on the data structures
  USE globalData,only:startTime,finshTime,refTime,oldTime

  USE var_lookup,only:maxvarFreq                              ! maximum number of output files
  USE var_lookup,only:iLookFreq                               ! output frequency lookup table
  implicit none
  ! Dummy Variables
  integer(c_int),intent(in)                  :: indx_gru      ! indx of the parent GRU
  integer(c_int),intent(in)                  :: indx_hru      ! indx of the HRU
  type(hru_type),intent(out)                 :: hru_data      ! hru data structure (hru_type
  integer(c_int),intent(out)                 :: err  
  character(len=256),intent(out)             :: message       ! error message
  ! Local Variables
  character(LEN=256)                         :: cmessage      ! error message of downwind routine
  integer(i4b)                               :: iStruct       ! looping variables
  ! ---------------------------------------------------------------------------------------
  ! initialize error control
  err=0; message='hru_init/'

  ! initialize the start of the initialization
  call date_and_time(values=startInit)

  ! initialize the elapsed time for cumulative quantities
  elapsedRead=0._dp
  elapsedWrite=0._dp
  elapsedPhysics=0._dp

  ! *****************************************************************************
  ! *** allocate space for data structures
  ! ****************************************************************************
  ! allocate time structures
  do iStruct=1,4
  select case(iStruct)
    case(1); call allocLocal(time_meta, hru_data%startTime_hru, err=err, message=cmessage)  ! start time for the model simulation
    case(2); call allocLocal(time_meta, hru_data%finishTime_hru, err=err, message=cmessage)  ! end time for the model simulation
    case(3); call allocLocal(time_meta, hru_data%refTime_hru,   err=err, message=cmessage)  ! reference time for the model simulation
    case(4); call allocLocal(time_meta, hru_data%oldTime_hru,   err=err, message=cmessage)  ! time from the previous step
  end select
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  end do  ! looping through time structures

  ! copy the time variables set up by the job_actor
  hru_data%startTime_hru%var(:) = startTime%var(:)
  hru_data%finishTime_hru%var(:) = finshTime%var(:)
  hru_data%refTime_hru%var(:) = refTime%var(:)
  hru_data%oldTime_hru%var(:) = oldTime%var(:)


  ! get the number of snow and soil layers
  associate(&
  nSnow => gru_struc(indx_gru)%hruInfo(indx_hru)%nSnow, & ! number of snow layers for each HRU
  nSoil => gru_struc(indx_gru)%hruInfo(indx_hru)%nSoil  ) ! number of soil layers for each HRU

  ! allocate other data structures
  do iStruct=1,size(structInfo)
  ! allocate space  
  select case(trim(structInfo(iStruct)%structName))    
    case('time'); call allocLocal(time_meta,hru_data%timeStruct,err=err,message=cmessage)     ! model time data
    case('forc'); call allocLocal(forc_meta,hru_data%forcStruct,nSnow,nSoil,err,cmessage);    ! model forcing data
    case('attr'); call allocLocal(attr_meta,hru_data%attrStruct,nSnow,nSoil,err,cmessage);    ! model attribute data
    case('type'); call allocLocal(type_meta,hru_data%typeStruct,nSnow,nSoil,err,cmessage);    ! model type data
    case('id'  ); call allocLocal(id_meta,hru_data%idStruct,nSnow,nSoil,err,cmessage);        ! model id data
    case('mpar'); call allocLocal(mpar_meta,hru_data%mparStruct,nSnow,nSoil,err,cmessage);    ! model parameters  
    case('indx'); call allocLocal(indx_meta,hru_data%indxStruct,nSnow,nSoil,err,cmessage);    ! model variables
    case('prog'); call allocLocal(prog_meta,hru_data%progStruct,nSnow,nSoil,err,cmessage);    ! model prognostic (state) variables
    case('diag'); call allocLocal(diag_meta,hru_data%diagStruct,nSnow,nSoil,err,cmessage);    ! model diagnostic variables
    case('flux'); call allocLocal(flux_meta,hru_data%fluxStruct,nSnow,nSoil,err,cmessage);    ! model fluxes
    case('bpar'); call allocLocal(bpar_meta,hru_data%bparStruct,nSnow=0,nSoil=0,err=err,message=cmessage);  ! basin-average variables
    case('bvar'); call allocLocal(bvar_meta,hru_data%bvarStruct,nSnow=0,nSoil=0,err=err,message=cmessage);  ! basin-average variables
    case('lookup'); cycle ! allocated in enthaplpyTemp.f90
    case('deriv'); cycle
    case default; err=20; message='unable to find structure name: '//trim(structInfo(iStruct)%structName)
  end select
  ! check errors
  if(err/=0)then
    message=trim(message)//trim(cmessage)//'[structure =  '//trim(structInfo(iStruct)%structName)//']'
    print*, message
    return
  endif
  end do  ! looping through data structures

  ! allocate space for default model parameters
	! NOTE: This is done here, rather than in the loop above, because dpar is not one of the "standard" data structures
	call allocLocal(mpar_meta,hru_data%dparStruct,nSnow,nSoil,err,cmessage);    ! default model parameters
	if(err/=0)then; message=trim(message)//trim(cmessage)//' [problem allocating dparStruct]'; print*,message;return;endif
	 


  ! *****************************************************************************
  ! *** allocate space for output statistics data structures
  ! *****************************************************************************
  ! loop through data structures
  do iStruct=1,size(structInfo)
    ! allocate space
    select case(trim(structInfo(iStruct)%structName))
      case('forc'); call allocLocal(statForc_meta(:)%var_info,hru_data%forcStat,nSnow,nSoil,err,cmessage);    ! model forcing data
      case('prog'); call allocLocal(statProg_meta(:)%var_info,hru_data%progStat,nSnow,nSoil,err,cmessage);    ! model prognostic 
      case('diag'); call allocLocal(statDiag_meta(:)%var_info,hru_data%diagStat,nSnow,nSoil,err,cmessage);    ! model diagnostic
      case('flux'); call allocLocal(statFlux_meta(:)%var_info,hru_data%fluxStat,nSnow,nSoil,err,cmessage);    ! model fluxes
      case('indx'); call allocLocal(statIndx_meta(:)%var_info,hru_data%indxStat,nSnow,nSoil,err,cmessage);    ! index vars
      case('bvar'); call allocLocal(statBvar_meta(:)%var_info,hru_data%bvarStat,nSnow=0,nSoil=0,err=err,message=cmessage);  ! basin-average variables
      case default; cycle
    end select
    ! check errors
    if(err/=0)then
      message=trim(message)//trim(cmessage)//'[statistics for =  '//trim(structInfo(iStruct)%structName)//']'
      print*, message
      return
    endif
  end do ! iStruct


  ! Intilaize the statistics data structures
  allocate(hru_data%statCounter%var(maxVarFreq), stat=err)
  allocate(hru_data%outputTimeStep%var(maxVarFreq), stat=err)
  allocate(hru_data%resetStats%dat(maxVarFreq), stat=err)
  allocate(hru_data%finalizeStats%dat(maxVarFreq), stat=err)
  hru_data%statCounter%var(1:maxVarFreq) = 1
  hru_data%outputTimeStep%var(1:maxVarFreq) = 1
  ! initialize flags to reset/finalize statistics
  hru_data%resetStats%dat(:)    = .true.   ! start by resetting statistics
  hru_data%finalizeStats%dat(:) = .false.  ! do not finalize stats on the first time step
  ! set stats flag for the timestep-level output
  hru_data%finalizeStats%dat(iLookFreq%timestep)=.true.

  ! identify the end of the initialization
  call date_and_time(values=endInit)

  ! end association to info in data structures
  end associate

end subroutine initHRU


! **************************************************************************************************
! public subroutine setupHRUParam: initializes parameter data structures (e.g. vegetation and soil parameters).
! **************************************************************************************************
subroutine setupHRU(indxGRU, indxHRU, hru_data, err, message)
  ! ---------------------------------------------------------------------------------------
  ! * desired modules
  ! ---------------------------------------------------------------------------------------
  USE nrtype                                                  ! variable types, etc.
  USE summa_init_struc,only:init_struc
  ! subroutines and functions
  use time_utils_module,only:elapsedSec                       ! calculate the elapsed time
  USE mDecisions_module,only:mDecisions                       ! module to read model decisions
  USE paramCheck_module,only:paramCheck                       ! module to check consistency of model parameters
  USE pOverwrite_module,only:pOverwrite                       ! module to overwrite default parameter values with info from the Noah tables
  USE var_derive_module,only:fracFuture                       ! module to calculate the fraction of runoff in future time steps (time delay histogram)
  USE module_sf_noahmplsm,only:read_mp_veg_parameters         ! module to read NOAH vegetation tables
  ! global data structures
  USE globalData,only:gru_struc                               ! gru-hru mapping structures
  USE globalData,only:localParFallback                        ! local column default parameters
  USE globalData,only:model_decisions                         ! model decision structure
  USE globalData,only:greenVegFrac_monthly                    ! fraction of green vegetation in each month (0-1)
  ! output constraints
  USE globalData,only:maxLayers                               ! maximum number of layers
  USE globalData,only:maxSnowLayers                           ! maximum number of snow layers
  ! timing variables
  USE globalData,only:startSetup,endSetup                     ! date/time for the start and end of the parameter setup
  USE globalData,only:elapsedSetup                            ! elapsed time for the parameter setup
  ! Noah-MP parameters
  USE NOAHMP_VEG_PARAMETERS,only:SAIM,LAIM                    ! 2-d tables for stem area index and leaf area index (vegType,month)
  USE NOAHMP_VEG_PARAMETERS,only:HVT,HVB                      ! height at the top and bottom of vegetation (vegType)

  ! ---------------------------------------------------------------------------------------
  ! * variables
  ! ---------------------------------------------------------------------------------------
  implicit none
  ! dummy variables
  ! calling variables
  integer(c_int),intent(in)                :: indxGRU              ! Index of the parent GRU of the HRU
  integer(c_int),intent(in)                :: indxHRU              ! ID to locate correct HRU from netcdf file 
  type(hru_type),intent(out)               :: hru_data             ! local hru data structure
  integer(c_int),intent(inout)             :: err
  character(len=256),intent(out)           :: message

  ! local variables

  integer(i4b)                             :: ivar                 ! loop counter
  integer(i4b)                             :: i_z                  ! loop counter
  character(len=256)                       :: cmessage             ! error message of downwind routine

  ! ---------------------------------------------------------------------------------------
  ! initialize error control
  err=0; message='setupHRU'

  hru_data%oldTime_hru%var(:) = hru_data%startTime_hru%var(:)
  hru_data%attrStruct%var(:) = init_struc%attrStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  hru_data%typeStruct%var(:) = init_struc%typeStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  hru_data%idStruct%var(:) = init_struc%idStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  hru_data%mparStruct%var(:) = init_struc%mparStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  hru_data%bparStruct%var(:) = init_struc%bparStruct%gru(indxGRU)%var(:)
  hru_data%dparStruct%var(:) = init_struc%dparStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  do ivar=1, size(init_struc%bvarStruct%gru(indxGRU)%var(:))
    hru_data%bvarStruct%var(ivar)%dat(:) = init_struc%bvarStruct%gru(indxGRU)%var(ivar)%dat(:)
  enddo
#ifdef V4_ACTIVE
  if (allocated(init_struc%lookupStruct%gru(indxGRU)%hru(indxHRU)%z)) then
    if (.not. allocated(hru_data%lookupStruct%z)) then
      allocate(hru_data%lookupStruct%z(size(init_struc%lookupStruct%gru(indxGRU)%hru(indxHRU)%z)))
    end if
    do i_z = 1, size(init_struc%lookupStruct%gru(indxGRU)%hru(indxHRU)%z(:))
      if (.not. allocated(hru_data%lookupStruct%z(i_z)%var)) then
        allocate(hru_data%lookupStruct%z(i_z)%var(size(init_struc%lookupStruct%gru(indxGRU)%hru(indxHRU)%z(i_z)%var)))
      end if
      do ivar = 1, size(init_struc%lookupStruct%gru(indxGRU)%hru(indxHRU)%z(i_z)%var(:))
        if (.not. allocated(hru_data%lookupStruct%z(i_z)%var(ivar)%lookup)) then
          allocate(hru_data%lookupStruct%z(i_z)%var(ivar)%lookup(size(init_struc%lookupStruct%gru(indxGRU)%hru(indxHRU)%z(i_z)%var(ivar)%lookup)))
        end if
        hru_data%lookupStruct%z(i_z)%var(ivar)%lookup(:) = init_struc%lookupStruct%gru(indxGRU)%hru(indxHRU)%z(i_z)%var(ivar)%lookup(:)
      end do
    end do
  endif
#endif
  do ivar=1, size(init_struc%progStruct%gru(indxGRU)%hru(indxHRU)%var(:))
    hru_data%progStruct%var(ivar)%dat(:) = init_struc%progStruct%gru(indxGRU)%hru(indxHRU)%var(ivar)%dat(:)
  enddo
  do ivar=1, size(init_struc%indxStruct%gru(indxGRU)%hru(indxHRU)%var(:))
    hru_data%indxStruct%var(ivar)%dat(:) = init_struc%indxStruct%gru(indxGRU)%hru(indxHRU)%var(ivar)%dat(:)
  enddo
  do ivar=1, size(diag_struc%indxStruct%gru(indxGRU)%hru(indxHRU)%var(:))
    hru_data%diagStruct%var(ivar)%dat(:) = init_struc%diagStruct%gru(indxGRU)%hru(indxHRU)%var(ivar)%dat(:)
  enddo
end subroutine setupHRU


! **************************************************************************************************
! public subroutine summa_readRestart: read restart data and reset the model state
! **************************************************************************************************
subroutine readHRURestart(indxGRU, indxHRU, hru_data, err, message)
  USE nrtype                                                  ! variable types, etc.
  ! functions and subroutines
  USE time_utils_module,only:elapsedSec                       ! calculate the elapsed time
  USE var_derive_module,only:calcHeight                       ! module to calculate height at layer interfaces and layer mid-point
  USE var_derive_module,only:v_shortcut                       ! module to calculate "short-cut" variables
  USE var_derive_module,only:rootDensty                       ! module to calculate the vertical distribution of roots
  USE var_derive_module,only:satHydCond                       ! module to calculate the saturated hydraulic conductivity in each soil layer
  ! global data structures
  USE globalData,only:model_decisions                         ! model decision structure
  ! timing variables
  USE globalData,only:startRestart,endRestart                 ! date/time for the start and end of reading model restart files
  USE globalData,only:elapsedRestart                          ! elapsed time to read model restart files
  ! Lookup values
  USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions
  USE var_lookup,only:iLookBVAR                               ! look-up values for basin-average model variables
  ! model decisions
  USE mDecisions_module,only:&                                ! look-up values for the choice of method for the spatial representation of groundwater
  localColumn, & ! separate groundwater representation in each local soil column
  singleBasin    ! single groundwater store over the entire basin
#ifdef V4_ACTIVE
  USE mDecisions_module,only:&
  fullStart,      & ! start with full aquifer
  emptyStart        ! start with empty aquifer
  ! look-up values for the choice of variable in energy equations (BE residual or IDA state variable)
  USE mDecisions_module,only:&
  closedForm,     & ! use temperature with closed form heat capacity
  enthalpyFormLU, & ! use enthalpy with soil temperature-enthalpy lookup tables
  enthalpyForm      ! use enthalpy with soil temperature-enthalpy analytical solution
#endif
  implicit none
  ! Dummy variables
  integer(c_int),intent(in)               :: indxGRU            !  index of GRU in gru_struc
  integer(c_int),intent(in)               :: indxHRU            !  index of HRU in gru_struc
  type(hru_type),intent(out)              :: hru_data
  integer(c_int), intent(out)             :: err
  character(len=256),intent(out)          :: message
  ! local variables
  integer(i4b)                            :: ivar               ! index of variable
  character(LEN=256)                      :: cmessage           ! error message of downwind routine
  character(LEN=256)                      :: restartFile        ! restart file name
  integer(i4b)                            :: nGRU
  real(double)                            :: aquifer_start      ! initial aquifer storage
  logical(lgt)                            :: checkEnthalpy      ! flag if checking enthalpy for consistency
  logical(lgt)                            :: use_lookup         ! flag to use the lookup table for soil enthalpy, otherwise use analytical solution
 
  ! ---------------------------------------------------------------------------------------
  ! initialize error control
  err=0; message='hru_actor_readRestart/'

! check initial conditions
  checkEnthalpy = .false.
  use_lookup    = .false.
#ifdef V4_ACTIVE
  if(model_decisions(iLookDECISIONS%num_method)%iDecision .ne. closedForm) checkEnthalpy = .true. ! check enthalpy either for mixed form energy equation or enthalpy state variable
  if(model_decisions(iLookDECISIONS%num_method)%iDecision==enthalpyFormLU) use_lookup = .true.    ! use lookup tables for soil temperature-enthalpy instead of analytical solution
  call check_icond(1,                         & ! intent(in):    number of response units
                   hru_data%progStruct,                   & ! intent(inout): model prognostic variables
                   hru_data%diagStruct,                   & ! intent(inout): model diagnostic variables
                   hru_data%mparStruct,                   & ! intent(in):    model parameters
                   hru_data%indxStruct,                   & ! intent(in):    layer indexes
                   hru_data%lookupStruct,                 & ! intent(in):    lookup tables
                   checkEnthalpy,                & ! intent(in):    flag if need to start with consistent enthalpy
                   .false.,                      & ! intent(in):    flag that enthalpy not in initial conditions
                   use_lookup,                   & ! intent(in):    flag to use the lookup table for soil enthalpy
                   err,cmessage)                   ! intent(out):   error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
#endif
 

  ! *****************************************************************************
  ! *** compute ancillary variables
  ! *****************************************************************************

  ! re-calculate height of each layer
  call calcHeight(hru_data%indxStruct,   & ! layer type
      hru_data%progStruct,   & ! model prognostic (state) variables for a local HRU
      err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! calculate vertical distribution of root density
  call rootDensty(hru_data%mparStruct,   & ! vector of model parameters
      hru_data%indxStruct,   & ! data structure of model indices
      hru_data%progStruct,   & ! data structure of model prognostic (state) variables
      hru_data%diagStruct,   & ! data structure of model diagnostic variables
      err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! calculate saturated hydraulic conductivity in each soil layer
  call satHydCond(hru_data%mparStruct,   & ! vector of model parameters
      hru_data%indxStruct,   & ! data structure of model indices
      hru_data%progStruct,   & ! data structure of model prognostic (state) variables
      hru_data%fluxStruct,   & ! data structure of model fluxes
      err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! calculate "short-cut" variables such as volumetric heat capacity
  call v_shortcut(hru_data%mparStruct,   & ! vector of model parameters
      hru_data%diagStruct,   & ! data structure of model diagnostic variables
      err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! initialize canopy drip
  ! NOTE: canopy drip from the previous time step is used to compute throughfall for the current time step
  hru_data%fluxStruct%var(iLookFLUX%scalarCanopyLiqDrainage)%dat(1) = 0._dp  ! not used

  ! *****************************************************************************
  ! *** initialize aquifer storage
  ! *****************************************************************************

  ! initialize aquifer storage
  ! NOTE: this is ugly: need to add capabilities to initialize basin-wide state variables

  ! There are two options for groundwater:
  !  (1) where groundwater is included in the local column (i.e., the HRUs); and
  !  (2) where groundwater is included for the single basin (i.e., the GRUS, where multiple HRUS drain into a GRU).

  ! For water balance calculations it is important to ensure that the local aquifer storage is zero if groundwater is treated as a basin-average state variable (singleBasin);
  !  and ensure that basin-average aquifer storage is zero when groundwater is included in the local columns (localColumn).

  aquifer_start  = 1._dp
#ifdef V4_ACTIVE
  ! select aquifer option
  select case(model_decisions(iLookDECISIONS%aquiferIni)%iDecision)
   case(fullStart)
    aquifer_start  = 1._dp ! Start with full aquifer, since easier to spin up by draining than filling (filling we need to wait for precipitation) 
   case(emptyStart)
    aquifer_start  = 0._dp ! Start with empty aquifer ! If want to compare model method outputs, empty start leads to quicker equilibrium
   case default
    message=trim(message)//'unable to identify decision for initial aquifer storage'
   return
  end select  ! aquifer option
#endif

  ! select groundwater option
  select case(model_decisions(iLookDECISIONS%spatial_gw)%iDecision)

  ! the basin-average aquifer storage is not used if the groundwater is included in the local column
  case(localColumn)
   hru_data%bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = 0._dp ! set to zero to be clear that there is no basin-average aquifer storage in this configuration
#ifdef V4_ACTIVE
   if(model_decisions(iLookDECISIONS%aquiferIni)%iDecision==emptyStart) &
     hru_data%progStruct%var(iLookPROG%scalarAquiferStorage)%dat(1) = aquifer_start ! leave at initialized values if fullStart
   end do
#endif

  ! the local column aquifer storage is not used if the groundwater is basin-average
  ! (i.e., where multiple HRUs drain to a basin-average aquifer)
  case(singleBasin)
   hru_data%bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = aquifer_start
   hru_data%progStruct%var(iLookPROG%scalarAquiferStorage)%dat(1) = 0._dp  ! set to zero to be clear that there is no local aquifer storage in this configuration

  ! error check
  case default
  message=trim(message)//'unable to identify decision for regional representation of groundwater'
  return

  end select  ! groundwater option

  ! *****************************************************************************
  ! *** initialize time step
  ! *****************************************************************************

  ! initialize time step length
  hru_data%dt_init = hru_data%progStruct%var(iLookPROG%dt_init)%dat(1) ! seconds

end subroutine readHRURestart

! Set the HRU's relative and absolute tolerances
subroutine setBEStepsIDATol(handle_hru_data,    &
                            be_steps,           &
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
                            absTolAquifr) bind(C, name="setBEStepsIDATol")
  USE data_types,only:var_dlength
  USE var_lookup,only:iLookPARAM

  implicit none

  type(c_ptr), intent(in), value          :: handle_hru_data    !  model time data
  integer(c_int),intent(in)               :: be_steps
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

#ifdef V4_ACTIVE
  hru_data%mparStruct%var(iLookPARAM%be_steps)%dat(1)            = REAL(be_steps)
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
#endif
end subroutine setBEStepsIDATol
end module INIT_HRU_ACTOR
