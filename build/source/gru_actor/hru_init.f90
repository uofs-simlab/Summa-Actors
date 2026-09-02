module INIT_HRU_ACTOR
! used to declare and allocate summa data structures and initialize model state to known values
USE,intrinsic :: iso_c_binding
USE nr_type          ! variable types, etc.
USE actor_data_types,only:hru_type             ! hru_type
                    
! metadata structures
USE globalData,only:time_meta,forc_meta,attr_meta,type_meta ! metadata structures
USE globalData,only:prog_meta,diag_meta,flux_meta,id_meta   ! metadata structures
USE globalData,only:mpar_meta,indx_meta                     ! metadata structures
USE globalData,only:bpar_meta,bvar_meta                     ! metadata structures
USE globalData,only:averageFlux_meta                        ! metadata for time-step average fluxes
USE globalData,only:lookup_meta 
! statistics metadata structures
USE globalData,only:statForc_meta                           ! child metadata for stats
USE globalData,only:statProg_meta                           ! child metadata for stats
USE globalData,only:statDiag_meta                           ! child metadata for stats
USE globalData,only:statFlux_meta                           ! child metadata for stats
USE globalData,only:statIndx_meta                           ! child metadata for stats
USE globalData,only:statBvar_meta                           ! child metadata for stats
! maxvarFreq 
USE var_lookup,only:maxvarFreq                               ! # of available output frequencies
! named variables
USE var_lookup,only:iLookATTR                               ! look-up values for local attributes
USE var_lookup,only:iLookTYPE                               ! look-up values for classification of veg, soils etc.
USE var_lookup,only:iLookPARAM                              ! look-up values for local column model parameters
USE var_lookup,only:iLookID                                   ! look-up values for local column model parameters

USE var_lookup,only:iLookPROG                               ! look-up values for local column model prognostic (state) variables
USE var_lookup,only:iLookDIAG                               ! look-up values for local column model diagnostic variables
USE var_lookup,only:iLookFLUX                               ! look-up values for local column model fluxes

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
  USE nr_type                                                 ! variable types, etc.
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
  USE var_lookup,only:iLookFreq                               ! output frequency lookup table
  implicit none
  ! Dummy Variables
  integer(c_int),intent(in)                  :: indx_gru      ! indx of the parent GRU
  integer(c_int),intent(in)                  :: indx_hru      ! indx of the HRU
  type(hru_type),intent(out)                 :: hru_data      ! hru data structure (hru_type)
  integer(c_int),intent(out)                 :: err  
  character(len=256),intent(out)             :: message       ! error message
  ! Local Variables
  character(LEN=256)                         :: cmessage      ! error message of downwind routine
  integer(i4b)                               :: iStruct       ! looping variables
  integer(i4b)                               :: iDOM          ! domain loop counter
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
    case(2); call allocLocal(time_meta, hru_data%finishTime_hru, err=err, message=cmessage) ! end time for the model simulation
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

  ! get the number of domains in this HRU
  associate(domCount => gru_struc(indx_gru)%hruInfo(indx_hru)%domCount)

  ! allocate the per-domain containers (contents allocated in the domain loop below)
  allocate(hru_data%dt_init%dom(domCount))
  allocate(hru_data%mparStruct%dom(domCount), hru_data%indxStruct%dom(domCount),   &
           hru_data%progStruct%dom(domCount), hru_data%diagStruct%dom(domCount),   &
           hru_data%fluxStruct%dom(domCount),                                      &
           hru_data%progStat%dom(domCount),   hru_data%diagStat%dom(domCount),     &
           hru_data%fluxStat%dom(domCount),   hru_data%indxStat%dom(domCount),     &
           hru_data%lookupStruct%dom(domCount), stat=err)
  if(err/=0)then; message=trim(message)//'problem allocating per-domain containers'; print*,message; return; endif

  ! allocate the HRU-level (non-domain) data structures
  do iStruct=1,size(structInfo)
  select case(trim(structInfo(iStruct)%structName))
    case('time'); call allocLocal(time_meta,hru_data%timeStruct,err=err,message=cmessage)                            ! model time data
    case('forc'); call allocLocal(forc_meta,hru_data%forcStruct,0,0,0,0,0,err,cmessage)                              ! model forcing data (HRU level)
    case('attr'); call allocLocal(attr_meta,hru_data%attrStruct,0,0,0,0,0,err,cmessage)                              ! model attribute data
    case('type'); call allocLocal(type_meta,hru_data%typeStruct,0,0,0,0,0,err,cmessage)                              ! model type data
    case('id'  ); call allocLocal(id_meta,hru_data%idStruct,0,0,0,0,0,err,cmessage)                                  ! model id data
    case('bpar'); call allocLocal(bpar_meta,hru_data%bparStruct,nSnow=0,nLake=0,nSoil=0,nGlce=0,nGlac=0,err=err,message=cmessage)  ! basin-average parameters
    case('bvar'); call allocLocal(bvar_meta,hru_data%bvarStruct,nSnow=0,nLake=0,nSoil=0,nGlce=0,nGlac=gru_struc(indx_gru)%nGlac,err=err,message=cmessage)  ! basin-average variables
    case default; cycle
  end select
  if(err/=0)then
    message=trim(message)//trim(cmessage)//'[structure =  '//trim(structInfo(iStruct)%structName)//']'
    print*, message
    return
  endif
  end do  ! looping through HRU-level data structures

  ! default model parameters (HRU level; not one of the "standard" data structures)
  call allocLocal(mpar_meta,hru_data%dparStruct,0,0,0,0,0,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage)//'[dparStruct]'; print*,message; return; endif

  ! allocate HRU-level statistics structures
  call allocLocal(statForc_meta(:)%var_info,hru_data%forcStat,0,0,0,0,0,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage)//'[forcStat]'; print*,message; return; endif
  call allocLocal(statBvar_meta(:)%var_info,hru_data%bvarStat,nSnow=0,nLake=0,nSoil=0,nGlce=0,nGlac=0,err=err,message=cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage)//'[bvarStat]'; print*,message; return; endif

  ! *****************************************************************************
  ! *** allocate the per-domain data structures (prognostic + parameters + stats)
  ! *****************************************************************************
  do iDOM = 1, domCount
    associate(nSnow => gru_struc(indx_gru)%hruInfo(indx_hru)%domInfo(iDOM)%nSnow, &
              nLake => gru_struc(indx_gru)%hruInfo(indx_hru)%domInfo(iDOM)%nLake, &
              nSoil => gru_struc(indx_gru)%hruInfo(indx_hru)%domInfo(iDOM)%nSoil, &
              nGlce => gru_struc(indx_gru)%hruInfo(indx_hru)%domInfo(iDOM)%nGlce)
      call allocLocal(mpar_meta,hru_data%mparStruct%dom(iDOM),nSnow,nLake,nSoil,nGlce,0,err,cmessage);                     if(err/=0)goto 900
      call allocLocal(indx_meta,hru_data%indxStruct%dom(iDOM),nSnow,nLake,nSoil,nGlce,0,err,cmessage);                     if(err/=0)goto 900
      call allocLocal(prog_meta,hru_data%progStruct%dom(iDOM),nSnow,nLake,nSoil,nGlce,0,err,cmessage);                     if(err/=0)goto 900
      call allocLocal(diag_meta,hru_data%diagStruct%dom(iDOM),nSnow,nLake,nSoil,nGlce,0,err,cmessage);                     if(err/=0)goto 900
      call allocLocal(flux_meta,hru_data%fluxStruct%dom(iDOM),nSnow,nLake,nSoil,nGlce,0,err,cmessage);                     if(err/=0)goto 900
      call allocLocal(statProg_meta(:)%var_info,hru_data%progStat%dom(iDOM),nSnow,nLake,nSoil,nGlce,0,err,cmessage);       if(err/=0)goto 900
      call allocLocal(statDiag_meta(:)%var_info,hru_data%diagStat%dom(iDOM),nSnow,nLake,nSoil,nGlce,0,err,cmessage);       if(err/=0)goto 900
      call allocLocal(statFlux_meta(:)%var_info,hru_data%fluxStat%dom(iDOM),nSnow,nLake,nSoil,nGlce,0,err,cmessage);       if(err/=0)goto 900
      call allocLocal(statIndx_meta(:)%var_info,hru_data%indxStat%dom(iDOM),nSnow,nLake,nSoil,nGlce,0,err,cmessage);       if(err/=0)goto 900
    end associate
  end do
  goto 910
  900 continue
    message=trim(message)//trim(cmessage)//'[per-domain allocation]'
    print*, message
    return
  910 continue

  ! Intilaize the statistics data structures
  allocate(hru_data%statCounter%var(maxvarFreq), stat=err)
  allocate(hru_data%outputTimeStep%var(maxvarFreq), stat=err)
  allocate(hru_data%resetStats%dat(maxvarFreq), stat=err)
  allocate(hru_data%finalizeStats%dat(maxvarFreq), stat=err)
  hru_data%statCounter%var(1:maxvarFreq) = 1
  hru_data%outputTimeStep%var(1:maxvarFreq) = 1
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
  USE nr_type                                                  ! variable types, etc.
  USE summa_init_struc,only:init_struc
  USE globalData,only:gru_struc                                ! gru-hru-dom mapping structures
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

  integer(i4b)                             :: iVar                 ! loop counter
  integer(i4b)                             :: i_z                  ! loop counter
  integer(i4b)                             :: iDOM                 ! domain loop counter
  character(len=256)                       :: cmessage             ! error message of downwind routine

  ! ---------------------------------------------------------------------------------------
  ! initialize error control
  err=0; message='setupHRU'

  ! update the HRU-level (non-domain) structures
  hru_data%oldTime_hru%var(:) = hru_data%startTime_hru%var(:)
  hru_data%attrStruct%var(:) = init_struc%attrStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  hru_data%typeStruct%var(:) = init_struc%typeStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  hru_data%idStruct%var(:) = init_struc%idStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  hru_data%bparStruct%var(:) = init_struc%bparStruct%gru(indxGRU)%var(:)
  hru_data%dparStruct%var(:) = init_struc%dparStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  do iVar=1, size(init_struc%bvarStruct%gru(indxGRU)%var(:))
    hru_data%bvarStruct%var(iVar)%dat(:) = init_struc%bvarStruct%gru(indxGRU)%var(iVar)%dat(:)
  enddo

  ! update the per-domain structures
  do iDOM = 1, gru_struc(indxGRU)%hruInfo(indxHRU)%domCount
    associate(inLookup => init_struc%lookupStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM), &
              hrLookup => hru_data%lookupStruct%dom(iDOM))
    hru_data%mparStruct%dom(iDOM)%var(:) = init_struc%mparStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM)%var(:)
    if (allocated(inLookup%z)) then
      if (.not. allocated(hrLookup%z)) allocate(hrLookup%z(size(inLookup%z)))
      do i_z = 1, size(inLookup%z(:))
        if (.not. allocated(hrLookup%z(i_z)%var)) allocate(hrLookup%z(i_z)%var(size(inLookup%z(i_z)%var)))
        do iVar = 1, size(inLookup%z(i_z)%var(:))
          if (.not. allocated(hrLookup%z(i_z)%var(iVar)%lookup)) &
            allocate(hrLookup%z(i_z)%var(iVar)%lookup(size(inLookup%z(i_z)%var(iVar)%lookup)))
          hrLookup%z(i_z)%var(iVar)%lookup(:) = inLookup%z(i_z)%var(iVar)%lookup(:)
        end do
      end do
    endif
    end associate
    do iVar=1, size(init_struc%progStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM)%var(:))
      hru_data%progStruct%dom(iDOM)%var(iVar)%dat(:) = init_struc%progStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM)%var(iVar)%dat(:)
    enddo
    do iVar=1, size(init_struc%indxStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM)%var(:))
      hru_data%indxStruct%dom(iDOM)%var(iVar)%dat(:) = init_struc%indxStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM)%var(iVar)%dat(:)
    enddo
    do iVar=1, size(init_struc%diagStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM)%var(:))
      hru_data%diagStruct%dom(iDOM)%var(iVar)%dat(:) = init_struc%diagStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM)%var(iVar)%dat(:)
    enddo
    do iVar=1, size(init_struc%fluxStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM)%var(:))
      hru_data%fluxStruct%dom(iDOM)%var(iVar)%dat(:) = init_struc%fluxStruct%gru(indxGRU)%hru(indxHRU)%dom(iDOM)%var(iVar)%dat(:)
    enddo
  enddo
end subroutine setupHRU


! **************************************************************************************************
! public subroutine summa_readRestart: read restart data and reset the model state
! **************************************************************************************************
subroutine readHRURestart(indxGRU, indxHRU, hru_data, err, message)
  USE nr_type                                                 ! variable types, etc.
  ! functions and subroutines
  USE var_derive_module,only:calcHeight                       ! module to calculate height at layer interfaces and layer mid-point
  USE var_derive_module,only:v_shortcut                       ! module to calculate "short-cut" variables
  USE var_derive_module,only:rootDensty                       ! module to calculate the vertical distribution of roots
  USE var_derive_module,only:satHydCond                       ! module to calculate the saturated hydraulic conductivity in each soil layer
  ! global data structures
  USE globalData,only:model_decisions                         ! model decision structure
  USE globalData,only:gru_struc                               ! gru-hru-dom mapping structure
  ! Lookup values
  USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions
  USE var_lookup,only:iLookBVAR                               ! look-up values for basin-average model variables
  USE var_lookup,only:iLookINDEX                              ! look-up values for local column index variables
  ! model decisions
  USE mDecisions_module,only:&                                ! look-up values for the choice of method for the spatial representation of groundwater
  localColumn, & ! separate groundwater representation in each local soil column
  singleBasin    ! single groundwater store over the entire basin
  USE mDecisions_module,only:&
  fullStart,      & ! start with full aquifer
  emptyStart        ! start with empty aquifer
  implicit none
  ! Dummy variables
  integer(c_int),intent(in)               :: indxGRU            !  index of GRU in gru_struc
  integer(c_int),intent(in)               :: indxHRU            !  index of HRU in gru_struc
  type(hru_type),intent(out)              :: hru_data
  integer(c_int), intent(out)             :: err
  character(len=256),intent(out)          :: message
  ! local variables
  integer(i4b)                            :: iVar               ! index of variable
  integer(i4b)                            :: iDOM               ! domain loop counter
  character(LEN=256)                      :: cmessage           ! error message of downwind routine
  character(LEN=256)                      :: restartFile        ! restart file name
  integer(i4b)                            :: nGRU
  real(dp)                                :: aquifer_start      ! initial aquifer storage
  ! ---------------------------------------------------------------------------------------
  ! initialize error control
  err=0; message='hru_actor_readRestart/'

  ! *****************************************************************************
  ! *** compute ancillary variables (per domain)
  ! *****************************************************************************
  do iDOM = 1, gru_struc(indxGRU)%hruInfo(indxHRU)%domCount

    ! re-calculate height of each layer
    call calcHeight(hru_data%indxStruct%dom(iDOM), hru_data%progStruct%dom(iDOM), err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

    if (hru_data%indxStruct%dom(iDOM)%var(iLookINDEX%nSoil)%dat(1) > 0) then
      ! calculate vertical distribution of root density
      call rootDensty(hru_data%mparStruct%dom(iDOM), hru_data%indxStruct%dom(iDOM), &
                      hru_data%progStruct%dom(iDOM), hru_data%diagStruct%dom(iDOM), err,cmessage)
      if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

      ! calculate saturated hydraulic conductivity in each soil layer
      call satHydCond(hru_data%mparStruct%dom(iDOM), hru_data%indxStruct%dom(iDOM), &
                      hru_data%progStruct%dom(iDOM), hru_data%fluxStruct%dom(iDOM), err,cmessage)
      if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

      ! calculate "short-cut" variables such as volumetric heat capacity
      call v_shortcut(hru_data%mparStruct%dom(iDOM), hru_data%diagStruct%dom(iDOM), err,cmessage)
      if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
    end if

    ! initialize canopy drip (used to compute throughfall on the current time step)
    hru_data%fluxStruct%dom(iDOM)%var(iLookFLUX%scalarCanopyLiqDrainage)%dat(1) = 0._dp  ! not used
  end do

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

  ! select groundwater option
  select case(model_decisions(iLookDECISIONS%spatial_gw)%iDecision)

  ! the basin-average aquifer storage is not used if the groundwater is included in the local column
  case(localColumn)
   hru_data%bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = 0._dp ! set to zero to be clear that there is no basin-average aquifer storage in this configuration
   if(model_decisions(iLookDECISIONS%aquiferIni)%iDecision==emptyStart)then
     do iDOM = 1, gru_struc(indxGRU)%hruInfo(indxHRU)%domCount
       hru_data%progStruct%dom(iDOM)%var(iLookPROG%scalarAquiferStorage)%dat(1) = aquifer_start ! leave at initialized values if fullStart
     end do
   endif

  ! the local column aquifer storage is not used if the groundwater is basin-average
  ! (i.e., where multiple HRUs drain to a basin-average aquifer)
  case(singleBasin)
   hru_data%bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = aquifer_start
   do iDOM = 1, gru_struc(indxGRU)%hruInfo(indxHRU)%domCount
     hru_data%progStruct%dom(iDOM)%var(iLookPROG%scalarAquiferStorage)%dat(1) = 0._dp  ! set to zero to be clear that there is no local aquifer storage in this configuration
   end do

  ! error check
  case default
  message=trim(message)//'unable to identify decision for regional representation of groundwater'
  return

  end select  ! groundwater option

  ! *****************************************************************************
  ! *** initialize time step length (per domain)
  ! *****************************************************************************
  do iDOM = 1, gru_struc(indxGRU)%hruInfo(indxHRU)%domCount
    hru_data%dt_init%dom(iDOM) = hru_data%progStruct%dom(iDOM)%var(iLookPROG%dt_init)%dat(1) ! seconds
  end do

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
  integer(i4b)                            :: iDOM

  call c_f_pointer(handle_hru_data, hru_data)

  do iDOM = 1, size(hru_data%mparStruct%dom)
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%be_steps)%dat(1)            = REAL(be_steps)
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%relTolTempCas)%dat(1)       = relTolTempCas
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%absTolTempCas)%dat(1)       = absTolTempCas
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%relTolTempVeg)%dat(1)       = relTolTempVeg
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%absTolTempVeg)%dat(1)       = absTolTempVeg
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%relTolWatVeg)%dat(1)        = relTolWatVeg
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%absTolWatVeg)%dat(1)        = absTolWatVeg
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%relTolTempSoilSnow)%dat(1)  = relTolTempSoilSnow
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%absTolTempSoilSnow)%dat(1)  = absTolTempSoilSnow
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%relTolWatSnow)%dat(1)       = relTolWatSnow
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%absTolWatSnow)%dat(1)       = absTolWatSnow
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%relTolMatric)%dat(1)        = relTolMatric
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%absTolMatric)%dat(1)        = absTolMatric
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%relTolAquifr)%dat(1)        = relTolAquifr
    hru_data%mparStruct%dom(iDOM)%var(iLookPARAM%absTolAquifr)%dat(1)        = absTolAquifr
  end do

end subroutine setBEStepsIDATol
end module INIT_HRU_ACTOR
