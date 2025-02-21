module gru_actor
USE,intrinsic :: iso_c_binding
USE nrtype
USE globalData,only:integerMissing
USE globalData,only:realMissing

implicit none
public::f_getNumHruInGru
public::f_initGru
public::f_setGruTolerances
public::setupGRU_fortran
public::readGRURestart_fortran
public::setTimeZoneOffsetGRU_fortran
public::readGRUForcing_fortran
public::runGRU_fortran
public::writeGRUOutput_fortran
private::setupGRU
private::allocateOutputBuffer
private::alloc_outputStruc
private::allocateDat_rkind
private::allocateDat_int
private::is_var_desired

contains

subroutine f_getNumHruInGru(indx_gru, num_hru) bind(C, name="f_getNumHruInGru")
  USE globalData,only:gru_struc
  implicit none
  integer(c_int), intent(in)  :: indx_gru
  integer(c_int), intent(out) :: num_hru

  num_hru = gru_struc(indx_gru)%hruCount
end subroutine f_getNumHruInGru

subroutine f_setGruTolerances(handle_gru_data, be_steps, rel_tol, abs_tolWat, abs_tolNrg) bind(C, name="f_setGruTolerances")
  USE actor_data_types,only:gru_type
  USE var_lookup,only: iLookPARAM

  implicit none
  type(c_ptr), intent(in),value :: handle_gru_data
  integer(c_int), intent(in)    :: be_steps
  real(c_double), intent(in)    :: rel_tol
  real(c_double), intent(in)    :: abs_tolWat
  real(c_double), intent(in)    :: abs_tolNrg
  ! Local Varaibles
  integer(i4b)                  :: iHRU

  type(gru_type),pointer :: gru_data
  call c_f_pointer(handle_gru_data, gru_data)

  do iHRU = 1, size(gru_data%hru)
    ! Reset only if the values are valid
    if (rel_tol > 0.0 .and. abs_tolWat > 0.0 .and. abs_tolNrg > 0.0 ) then
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%relTolTempCas)%dat(1) = rel_tol
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%relTolTempVeg)%dat(1) = rel_tol
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%relTolWatVeg)%dat(1) = rel_tol
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%relTolTempSoilSnow)%dat(1) = rel_tol
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%relTolWatSnow)%dat(1) = rel_tol
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%relTolMatric)%dat(1) = rel_tol
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%relTolAquifr)%dat(1) = rel_tol

      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%absTolTempCas)%dat(1) = abs_tolNrg
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%absTolTempVeg)%dat(1) = abs_tolNrg 
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%absTolWatVeg)%dat(1) = abs_tolWat 
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%absTolTempSoilSnow)%dat(1) = abs_tolNrg 
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%absTolWatSnow)%dat(1) = abs_tolWat 
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%absTolMatric)%dat(1) = abs_tolWat 
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%absTolAquifr)%dat(1) = abs_tolWat 
    end if
    if (be_steps>0) then
      gru_data%hru(iHRU)%mparStruct%var(iLookPARAM%be_steps)%dat(1) = be_steps
    end if
  end do


end subroutine f_setGruTolerances

subroutine setupGRU(iGRU, err, message)
  USE summa_init_struc,only:init_struc
  USE globalData,only:gru_struc
  USE globalData,only:model_decisions                         ! model decision structure
  USE globalData,only:greenVegFrac_monthly                    ! fraction of green vegetation in each month (0-1)
  
  USE var_lookup,only:iLookTYPE
  USE var_lookup,only:iLookID
  USE var_lookup,only:iLookDECISIONS
  USE var_lookup,only:iLookPARAM
  USE var_lookup,only:iLookATTR
  USE var_lookup,only:iLookBVAR
  
  USE NOAHMP_VEG_PARAMETERS,only:HVT,HVB                      ! height at the top and bottom of vegetation (vegType)
  USE NOAHMP_VEG_PARAMETERS,only:SAIM,LAIM                    ! 2-d tables for stem area index and leaf area index (vegType,month)
  
  USE paramCheck_module,only:paramCheck                       ! module to check consistency of model parameters
#ifdef V4_ACTIVE
  ! look-up values for the choice of variable in energy equations (BE residual or IDA state variable)
  USE mDecisions_module,only:&
    closedForm,    &                      ! use temperature with closed form heat capacity
    enthalpyFormLU,&                      ! use enthalpy with soil temperature-enthalpy lookup tables
    enthalpyForm                          ! use enthalpy with soil temperature-enthalpy analytical solution
  USE enthalpyTemp_module,only:T2H_lookup_snWat               ! module to calculate a look-up table for the snow temperature-enthalpy conversion
  USE enthalpyTemp_module,only:T2L_lookup_soil                ! module to calculate a look-up table for the soil temperature-enthalpy conversion

#else
  
  USE ConvE2Temp_module,only:E2T_lookup                       ! module to calculate a look-up table for the temperature-enthalpy conversion

#endif
  USE var_derive_module,only:fracFuture                       ! module to calculate the fraction of runoff in future time steps (time delay histogram)
  
  ! named variables to define LAI decisions
  USE mDecisions_module,only:&
      monthlyTable,& ! LAI/SAI taken directly from a monthly table for different vegetation classes
      specified      ! LAI/SAI computed from green vegetation fraction and winterSAI and summerLAI parameters
  implicit none
  ! Dum
  integer(c_int), intent(in)      :: iGRU
  integer(c_int), intent(out)     :: err
  character(len=256), intent(out) :: message

  ! Local Variables
  character(len=256) :: cmessage
  integer(i4b)        :: iHRU, jHRU, kHRU
  logical             :: needLookup_soil = .false.

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

#ifdef V4_ACTIVE
  if(model_decisions(iLookDECISIONS%nrgConserv)%iDecision == enthalpyFormLU) needLookup_soil = .true. 
  ! *****************************************************************************
  ! *** compute derived model variables that are pretty much constant for the basin as a whole
  ! *****************************************************************************
  ! calculate the fraction of runoff in future time steps
  call fracFuture(bparStruct%gru(iGRU)%var,    &  ! vector of basin-average model parameters
                  bvarStruct%gru(iGRU),        &  ! data structure of basin-average variables
                  err,cmessage)                   ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! loop through local HRUs
  do iHRU=1,gru_struc(iGRU)%hruCount

    kHRU=0
    ! check the network topology (only expect there to be one downslope HRU)
    do jHRU=1,gru_struc(iGRU)%hruCount
      if(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%downHRUindex) == idStruct%gru(iGRU)%hru(jHRU)%var(iLookID%hruId))then
      if(kHRU==0)then  ! check there is a unique match
        kHRU=jHRU
      else
        message=trim(message)//'only expect there to be one downslope HRU'; return
      end if  ! (check there is a unique match)
      end if  ! (if identified a downslope HRU)
    end do

    ! check that the parameters are consistent
    call paramCheck(mparStruct%gru(iGRU)%hru(iHRU),err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

    ! calculate a look-up table for the temperature-enthalpy conversion of snow for future snow layer merging
    ! NOTE1: might be able to make this more efficient by only doing this for the HRUs that have snow
    ! NOTE2: H is the mixture enthalpy of snow liquid and ice
    call T2H_lookup_snWat(mparStruct%gru(iGRU)%hru(iHRU),err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

    ! calculate a lookup table for the temperature-enthalpy conversion of soil 
    ! NOTE: L is the integral of soil Clapeyron equation liquid water matric potential from temperature
    !       multiply by Cp_liq*iden_water to get temperature component of enthalpy
    if(needLookup_soil)then
      call T2L_lookup_soil(gru_struc(iGRU)%hruInfo(iHRU)%nSoil,   &   ! intent(in):    number of soil layers
                            mparStruct%gru(iGRU)%hru(iHRU),        &   ! intent(in):    parameter data structure
                            lookupStruct%gru(iGRU)%hru(iHRU),      &   ! intent(inout): lookup table data structure
                            err,cmessage)                              ! intent(out):   error control
      if(err/=0)then; message=trim(message)//trim(cmessage); return; endif  
    endif

    ! overwrite the vegetation height
    HVT(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex)) = mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%heightCanopyTop)%dat(1)
    HVB(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex)) = mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%heightCanopyBottom)%dat(1)

    ! overwrite the tables for LAI and SAI
    if(model_decisions(iLookDECISIONS%LAI_method)%iDecision == specified)then
      SAIM(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex),:) = mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%winterSAI)%dat(1)
      LAIM(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex),:) = mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%summerLAI)%dat(1)*greenVegFrac_monthly
    endif

  end do ! HRU

  ! compute total area of the upstream HRUS that flow into each HRU
  do iHRU=1,gru_struc(iGRU)%hruCount
    upArea%gru(iGRU)%hru(iHRU) = 0._rkind
    do jHRU=1,gru_struc(iGRU)%hruCount
      ! check if jHRU flows into iHRU; assume no exchange between GRUs
      if(typeStruct%gru(iGRU)%hru(jHRU)%var(iLookTYPE%downHRUindex)==typeStruct%gru(iGRU)%hru(iHRU)%var(iLookID%hruId))then
      upArea%gru(iGRU)%hru(iHRU) = upArea%gru(iGRU)%hru(iHRU) + attrStruct%gru(iGRU)%hru(jHRU)%var(iLookATTR%HRUarea)
      endif   ! (if jHRU is an upstream HRU)
    end do  ! jHRU
  end do  ! iHRU

  ! identify the total basin area for a GRU (m2)
  associate(totalArea => bvarStruct%gru(iGRU)%var(iLookBVAR%basin__totalArea)%dat(1) )
    totalArea = 0._rkind
    do iHRU=1,gru_struc(iGRU)%hruCount
      totalArea = totalArea + attrStruct%gru(iGRU)%hru(iHRU)%var(iLookATTR%HRUarea)
    end do
  end associate

! ****************************end V4***********************************
#else
! ****************************start V3****************************************


    ! calculate the fraction of runoff in future time steps
  call fracFuture(bparStruct%gru(iGRU)%var,    &  ! vector of basin-average model parameters
                  bvarStruct%gru(iGRU),        &  ! data structure of basin-average variables
                  err,cmessage)                   ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); endif

  ! loop through local HRUs
  do iHRU=1,gru_struc(iGRU)%hruCount

    kHRU=0
    ! check the network topology (only expect there to be one downslope HRU)
    do jHRU=1,gru_struc(iGRU)%hruCount
      if(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%downHRUindex) == idStruct%gru(iGRU)%hru(jHRU)%var(iLookID%hruId))then
        if(kHRU==0)then  ! check there is a unique match
          kHRU=jHRU
        else
          message=trim(message)//'only expect there to be one downslope HRU';
        end if  ! (check there is a unique match)
      end if  ! (if identified a downslope HRU)
    end do

    ! check that the parameters are consistent
    call paramCheck(mparStruct%gru(iGRU)%hru(iHRU),err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); endif

    ! calculate a look-up table for the temperature-enthalpy conversion
    call E2T_lookup(mparStruct%gru(iGRU)%hru(iHRU),err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); endif

    ! overwrite the vegetation height
    HVT(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex)) = mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%heightCanopyTop)%dat(1)
    HVB(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex)) = mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%heightCanopyBottom)%dat(1)

    ! overwrite the tables for LAI and SAI
    if(model_decisions(iLookDECISIONS%LAI_method)%iDecision == specified)then
      SAIM(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex),:) = mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%winterSAI)%dat(1)
      LAIM(typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex),:) = mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%summerLAI)%dat(1)*greenVegFrac_monthly
    endif

  end do ! HRU

  ! compute total area of the upstream HRUS that flow into each HRU
  do iHRU=1,gru_struc(iGRU)%hruCount
    upArea%gru(iGRU)%hru(iHRU) = 0._rkind
    do jHRU=1,gru_struc(iGRU)%hruCount
      ! check if jHRU flows into iHRU; assume no exchange between GRUs
      if(typeStruct%gru(iGRU)%hru(jHRU)%var(iLookTYPE%downHRUindex)==typeStruct%gru(iGRU)%hru(iHRU)%var(iLookID%hruId))then
        upArea%gru(iGRU)%hru(iHRU) = upArea%gru(iGRU)%hru(iHRU) + attrStruct%gru(iGRU)%hru(jHRU)%var(iLookATTR%HRUarea)
      endif   ! (if jHRU is an upstream HRU)
    end do  ! jHRU
  end do  ! iHRU

  ! identify the total basin area for a GRU (m2)
  bvarStruct%gru(iGRU)%var(iLookBVAR%basin__totalArea)%dat(1) = 0._rkind
  do iHRU=1,gru_struc(iGRU)%hruCount
    bvarStruct%gru(iGRU)%var(iLookBVAR%basin__totalArea)%dat(1) = bvarStruct%gru(iGRU)%var(iLookBVAR%basin__totalArea)%dat(1) + attrStruct%gru(iGRU)%hru(iHRU)%var(iLookATTR%HRUarea)
  end do

#endif

end associate summaVars

end subroutine setupGRU



subroutine f_initGru(indx_gru, handle_gru_data, output_buffer_steps, &
    err, message_r) bind(C, name="f_initGru")
  USE actor_data_types,only:gru_type             
  USE data_types,only:var_dlength
  USE globalData,only:statBvar_meta                           ! child metadata for stats
  USE globalData,only:bvar_meta                     ! metadata structures
  USE allocspace_module,only:allocLocal
  USE INIT_HRU_ACTOR,only:initHRU
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  implicit none
  ! Dummy variables
  integer(c_int), intent(in)          :: indx_gru
  type(c_ptr),    intent(in),value    :: handle_gru_data
  integer(c_int), intent(in)          :: output_buffer_steps
  integer(c_int), intent(out)         :: err
  type(c_ptr),   intent(out)          :: message_r

  ! local variables
  type(gru_type),pointer              :: gru_data
  integer(i4b)                        :: iHRU
  character(len=256)                  :: message = ""
  character(len=256)                  :: cmessage

  err = 0; message = "f_initGru/"
  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_gru_data, gru_data)

  ! ****************************************************************************
  ! Initialize our section of the output buffer
  ! ****************************************************************************
  call allocateOutputBuffer(indx_gru, size(gru_data%hru), output_buffer_steps, &
                            err, message)
  if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return;end if

  ! Setup the GRU
  call setupGRU(indx_gru, err, message)
  if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return;end if


  ! ****************************************************************************
  ! Allocate the basin variables
  ! ****************************************************************************
  call allocLocal(bvar_meta,gru_data%bvarStruct,nSnow=0,nSoil=0,err=err,message=cmessage);
  if(err /= 0) then; message=trim(message)//cmessage; call f_c_string_ptr(trim(message), message_r);return;end if 
  call allocLocal(statBvar_meta(:)%var_info,gru_data%bvarStat,nSnow=0,nSoil=0,err=err,message=cmessage);
  if(err /= 0) then; message=trim(message)//cmessage; call f_c_string_ptr(trim(message), message_r);return;end if 

  ! ****************************************************************************
  ! Initialize the HRUs
  ! ****************************************************************************
  do iHRU = 1, size(gru_data%hru)
    call initHRU(indx_gru, iHRU, gru_data%hru(iHRU), err, message)
    if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return; end if
  end do
end subroutine f_initGru

subroutine setupGRU_fortran(indx_gru, handle_gru_data, err, message_r) & 
    bind(C, name="setupGRU_fortran")
  USE summa_init_struc,only:init_struc
  USE actor_data_types,only:gru_type
  USE INIT_HRU_ACTOR,only:setupHRU
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  type(c_ptr),    intent(in),value :: handle_gru_data
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  integer(i4b)                     :: iHRU
  integer(i4b)                     :: iVar
  type(gru_type),pointer           :: gru_data
  character(len=256)               :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_gru_data, gru_data)

  do iHRU = 1, size(gru_data%hru)
    call setupHRU(indx_gru, iHRU, gru_data%hru(iHRU), err, message)
    if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return; end if
  end do

  do ivar=1, size(init_struc%bvarStruct%gru(indx_gru)%var(:))
    gru_data%bvarStruct%var(ivar)%dat(:) = init_struc%bvarStruct%gru(indx_gru)%var(ivar)%dat(:)
  enddo
end subroutine setupGRU_fortran

subroutine readGRURestart_fortran(indx_gru, handle_gru_data, err, message_r) &
    bind(C, name="readGRURestart_fortran")
  USE actor_data_types,only:gru_type
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  USE INIT_HRU_ACTOR,only:readHRURestart

  USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions
  USE var_lookup,only:iLookBVAR                               ! look-up values for basin-average model variables
  USE globalData,only:model_decisions                         ! model decision structure
  USE mDecisions_module,only:localColumn, & ! separate groundwater representation in each local soil column
                             singleBasin    ! single groundwater store over the entire basin
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  type(c_ptr),    intent(in),value :: handle_gru_data
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  integer(i4b)                     :: iHRU
  type(gru_type),pointer           :: gru_data
  character(len=256)               :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_gru_data, gru_data)

  do iHRU = 1, size(gru_data%hru)
    call readHRURestart(indx_gru, iHRU, gru_data%hru(iHRU), err, message)
    if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return; end if
  end do

  ! Set the basin variables that pertain to the GRU
  select case(model_decisions(iLookDECISIONS%spatial_gw)%iDecision)
    case(localColumn) 
      gru_data%bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = 0._dp
    case(singleBasin)
      gru_data%bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = 1._dp
    case default
      message=trim(message)//'unable to identify decision for regional representation of groundwater'
      call f_c_string_ptr(trim(message), message_r)
      err = 1
      return
  end select

end subroutine readGRURestart_fortran

subroutine setTimeZoneOffsetGRU_fortran(iFile, handle_gru_data, err, message_r) & 
    bind(C, name="setTimeZoneOffsetGRU_fortran")
  USE actor_data_types,only:gru_type
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  USE hru_read,only:setTimeZoneOffset
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)       :: iFile
  type(c_ptr),    intent(in),value :: handle_gru_data
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  integer(i4b)                     :: iHRU
  type(gru_type),pointer           :: gru_data
  character(len=256)               :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_gru_data, gru_data)

  do iHRU = 1, size(gru_data%hru)
    call setTimeZoneOffset(iFile, gru_data%hru(iHRU), err, message)
    if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return; end if
  end do

end subroutine setTimeZoneOffsetGRU_fortran

subroutine readGRUForcing_fortran(indx_gru, iStep, iRead, iFile, & 
    handle_gru_data, err, message_r) bind(C, name="readGRUForcing_fortran")
  USE actor_data_types,only:gru_type
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  USE hru_read,only:readHRUForcing
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  integer(c_int), intent(in)       :: iStep
  integer(c_int), intent(inout)    :: iRead
  integer(c_int), intent(in)       :: iFile
  type(c_ptr),    intent(in),value :: handle_gru_data
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  integer(i4b)                     :: iHRU
  type(gru_type),pointer           :: gru_data
  character(len=256)               :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_gru_data, gru_data)

  do iHRU = 1, size(gru_data%hru)
    call readHRUForcing(indx_gru, iHRU, iStep, iRead, iFile, &
                        gru_data%hru(iHRU), err, message)
    if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return; end if
  end do

end subroutine readGRUForcing_fortran

subroutine runGRU_fortran(indx_gru, modelTimeStep, handle_gru_data, &
    dt_init_factor, err, message_r) bind(C, name="runGRU_fortran")
  USE actor_data_types,only:gru_type
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  USE summa_modelRun,only:runPhysics
  
  USE globalData,only:model_decisions          ! model decision structure
  USE globalData,only:gru_struc
  USE qTimeDelay_module,only:qOverland         ! module to route water through an "unresolved" river network
  
  USE mDecisions_module,only:&               ! look-up values for LAI decisions
      monthlyTable,& ! LAI/SAI taken directly from a monthly table for different vegetation classes
      specified,&    ! LAI/SAI computed from green vegetation fraction and winterSAI and summerLAI parameters   
      localColumn, & ! separate groundwater representation in each local soil column
      singleBasin, & ! single groundwater store over the entire basin
      bigBucket

  USE var_lookup,only:iLookBVAR              ! look-up values for basin-average model variables
  USE var_lookup,only:iLookFLUX              ! look-up values for local column model fluxes
  USE var_lookup,only:iLookATTR              ! look-up values for local attributes
  USE var_lookup,only:iLookDECISIONS         ! look-up values for model decisions
  USE var_lookup,only:iLookTYPE              ! look-up values for HRU types
  USE var_lookup,only:iLookID                ! look-up values for HRU IDs
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  integer(c_int), intent(in)       :: modelTimeStep
  type(c_ptr),    intent(in),value :: handle_gru_data
  integer(c_int), intent(in)       :: dt_init_factor
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  integer(i4b)                     :: iHRU, kHRU, jHRU
  integer(i4b)                     :: iVar
  type(gru_type),pointer           :: gru_data
  character(len=256)               :: message = ""
  character(len=256)               :: cmessage
  real(rkind)                      :: fracHRU                ! fractional area of a given HRU (-)

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_gru_data, gru_data)

  ! ----- basin initialization --------------------------------------------------------------------------------------------
  ! initialize runoff variables
  gru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1)    = 0._dp  ! surface runoff (m s-1)
  gru_data%bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)     = 0._dp 
  gru_data%bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)    = 0._dp  ! outflow from all "outlet" HRUs (those with no downstream HRU)
  gru_data%bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1)      = 0._dp 

  ! initialize baseflow variables
  gru_data%bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)  = 0._dp ! recharge to the aquifer (m s-1)
  gru_data%bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  = 0._dp ! baseflow from the aquifer (m s-1)
  gru_data%bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1) = 0._dp ! transpiration loss from the aquifer (m s-1)

  do iHRU = 1, size(gru_data%hru) 
    gru_data%hru(iHRU)%fluxStruct%var(iLookFLUX%mLayerColumnInflow)%dat(:) = 0._rkind
  end do


  do iHRU = 1, size(gru_data%hru)
    ! Give the HRU the up to date basin variables
    do iVar=1, size(gru_data%bvarStruct%var(:))
      gru_data%hru(iHRU)%bvarStruct%var(iVar)%dat(:) = gru_data%bvarStruct%var(iVar)%dat(:)
    end do
    
    call runPhysics(indx_gru, iHRU, modelTimeStep, gru_data%hru(iHRU), &
                    dt_init_factor, err, message)
    if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return; end if

    fracHRU = gru_data%hru(iHRU)%attrStruct%var(iLookATTR%HRUarea) / &
              gru_data%hru(iHRU)%bvarStruct%var(iLookBVAR%basin__totalArea)%dat(1)

    ! Compute Fluxes Across HRUs
    ! identify lateral connectivity
    ! (Note:  for efficiency, this could this be done as a setup task, not every timestep)
    kHRU = 0
    ! identify the downslope HRU
    dsHRU: do jHRU=1,gru_struc(indx_gru)%hruCount
      if(gru_data%hru(iHRU)%typeStruct%var(iLookTYPE%downHRUindex) == gru_data%hru(jHRU)%idStruct%var(iLookID%hruId))then
        if(kHRU==0)then  ! check there is a unique match
          kHRU=jHRU
          exit dsHRU
        end if  ! (check there is a unique match)
      end if  ! (if identified a downslope HRU)
    end do dsHRU

    ! if lateral flows are active, add inflow to the downslope HRU
    if(kHRU > 0)then  ! if there is a downslope HRU
      gru_data%hru(kHRU)%fluxStruct%var(iLookFLUX%mLayerColumnInflow)%dat(:) = &
          gru_data%hru(kHRU)%fluxStruct%var(iLookFLUX%mLayerColumnInflow)%dat(:) + &
          gru_data%hru(iHRU)%fluxStruct%var(iLookFLUX%mLayerColumnOutflow)%dat(:)

    ! otherwise just increment basin (GRU) column outflow (m3 s-1) with the hru fraction
    else
      gru_data%bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1) = & 
          gru_data%bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1) + &
          sum( gru_data%hru(iHRU)%fluxStruct%var(iLookFLUX%mLayerColumnOutflow)%dat(:))
    end if


    ! ----- calculate weighted basin (GRU) fluxes --------------------------------------------------------------------------------------
    
    ! increment basin surface runoff (m s-1)
    gru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) = &
        gru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + &
        gru_data%hru(iHRU)%fluxStruct%var(iLookFLUX%scalarSurfaceRunoff)%dat(1) * &
        fracHRU
    
    !increment basin soil drainage (m s-1)
    gru_data%bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1) = &
        gru_data%bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1) + & 
        gru_data%hru(iHRU)%fluxStruct%var(iLookFLUX%scalarSoilDrainage)%dat(1) * &
        fracHRU
    
    ! increment aquifer variables -- ONLY if aquifer baseflow is computed individually for each HRU and aquifer is run
    ! NOTE: groundwater computed later for singleBasin
    if(model_decisions(iLookDECISIONS%spatial_gw)%iDecision == localColumn .and. &
       model_decisions(iLookDECISIONS%groundwatr)%iDecision == bigBucket) then

      gru_data%bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)  = &
          gru_data%bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1) + &
          gru_data%hru(iHRU)%fluxStruct%var(iLookFLUX%scalarSoilDrainage)%dat(1) * &
          fracHRU
      gru_data%bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1) = &
          gru_data%bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1) +& 
          gru_data%hru(iHRU)%fluxStruct%var(iLookFLUX%scalarAquiferTranspire)%dat(1) * &
          fracHRU
      gru_data%bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1) = & 
          gru_data%bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1) + &
          gru_data%hru(iHRU)%fluxStruct%var(iLookFLUX%scalarAquiferBaseflow)%dat(1) &
          * fracHRU
    end if
  end do
  ! ***********************************************************************************************************************
  ! ********** END LOOP THROUGH HRUS **************************************************************************************
  ! ***********************************************************************************************************************
  ! perform the routing
  associate(totalArea => gru_data%bvarStruct%var(iLookBVAR%basin__totalArea)%dat(1) )

  ! compute water balance for the basin aquifer
  if(model_decisions(iLookDECISIONS%spatial_gw)%iDecision == singleBasin)then
    message=trim(message)//'multi_driver/bigBucket groundwater code not transferred from old code base yet'
    err=20; call f_c_string_ptr(trim(message), message_r); return
  end if

  ! calculate total runoff depending on whether aquifer is connected
  if(model_decisions(iLookDECISIONS%groundwatr)%iDecision == bigBucket) then
    ! aquifer
    gru_data%bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1) = &
        gru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + &
        gru_data%bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)/totalArea + &
        gru_data%bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)
  else
    ! no aquifer
    gru_data%bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1) = &
        gru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + &
        gru_data%bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)/totalArea + &
        gru_data%bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)
  endif

  call qOverland(&! input
      model_decisions(iLookDECISIONS%subRouting)%iDecision,            &  ! intent(in): index for routing method
      gru_data%bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1),    &  ! intent(in): total runoff to the channel from all active components (m s-1)
      gru_data%bvarStruct%var(iLookBVAR%routingFractionFuture)%dat,    &  ! intent(in): fraction of runoff in future time steps (m s-1)
      gru_data%bvarStruct%var(iLookBVAR%routingRunoffFuture)%dat,      &  ! intent(in): runoff in future time steps (m s-1)
      ! output
      gru_data%bvarStruct%var(iLookBVAR%averageInstantRunoff)%dat(1),  &  ! intent(out): instantaneous runoff (m s-1)
      gru_data%bvarStruct%var(iLookBVAR%averageRoutedRunoff)%dat(1),   &  ! intent(out): routed runoff (m s-1)
      err,message)                                                        ! intent(out): error control
  if(err/=0)then; err=20; message=trim(message)//trim(cmessage); print*, message; return; endif;
  end associate

  ! update hru's bvarStruct with the basin's bvarStruct
  do iHRU = 1, size(gru_data%hru)
    do iVar=1, size(gru_data%bvarStruct%var(:))
      gru_data%hru(iHRU)%bvarStruct%var(iVar)%dat(:) = gru_data%bvarStruct%var(iVar)%dat(:)
    end do
  end do

end subroutine runGRU_fortran

subroutine writeGRUOutput_fortran(indx_gru, timestep, outputstep, &
    handle_gru_data, err, message_r) bind(C, name="writeGRUOutput_fortran")
  USE actor_data_types,only:gru_type
  USE HRUwriteoOutput_module,only:writeHRUOutput
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  integer(c_int), intent(in)       :: timestep
  integer(c_int), intent(in)       :: outputstep
  type(c_ptr),    intent(in),value :: handle_gru_data
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  integer(i4b)                     :: iHRU
  type(gru_type),pointer           :: gru_data
  character(len=256)               :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_gru_data, gru_data)

  do iHRU = 1, size(gru_data%hru)
    call writeHRUOutput(indx_gru, iHRU, timestep, outputstep, gru_data%hru(iHRU), & 
                        err, message)
    if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return; end if
  end do

end subroutine writeGRUOutput_fortran

! Local Subroutines
subroutine allocateOutputBuffer(indx_gru, num_hru, output_buffer_steps, &
    err, message)
  USE output_buffer,only:summa_struct
  USE globalData,only:structInfo                                ! information on the data structures
  USE allocspace_module,only:allocLocal                         ! module to allocate space for global data structures
  USE globalData,only:gru_struc                                 ! information on the GRUs
  
  USE globalData,only:time_meta,forc_meta,attr_meta,type_meta   ! metadata structures
  USE globalData,only:prog_meta,diag_meta,flux_meta,id_meta     ! metadata structures
  USE globalData,only:mpar_meta,indx_meta                       ! metadata structures
  USE globalData,only:bpar_meta,bvar_meta                       ! metadata structures

  USE globalData,only:statForc_meta,statProg_meta,statDiag_meta ! child metadata for stats
  USE globalData,only:statFlux_meta,statIndx_meta,statBvar_meta ! child metadata for stats

#ifdef V4_ACTIVE
  USE globalData,only:lookup_meta                             ! child metadata for stats
#endif
  USE globalData,only:maxSnowLayers
  USE var_lookup,only:maxvarFreq             ! allocation dimension (output frequency)
  

  implicit none
  ! Dummy Variables
  integer(c_int), intent(in)        :: indx_gru
  integer(c_int), intent(in)        :: num_hru
  integer(c_int), intent(in)        :: output_buffer_steps
  integer(c_int), intent(out)       :: err 
  character(len=256), intent(out)   :: message
  ! Local Variables
  integer(i4b)                      :: iHRU
  integer(i4b)                      :: iStep
  integer(i4b)                      :: iStruct
  integer(i4b)                      :: iDat

  if (allocated(summa_struct(1)%timeStruct%gru(indx_gru)%hru)) then 
    return 
  endif

  allocate(summa_struct(1)%forcStat%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%progStat%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%diagStat%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%fluxStat%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%indxStat%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%bvarStat%gru(indx_gru)%hru(num_hru))
  ! Primary Data Structures (scalars)
  allocate(summa_struct(1)%timeStruct%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%forcStruct%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%attrStruct%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%typeStruct%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%idStruct%gru(indx_gru)%hru(num_hru))
  ! Primary Data Structures (variable length vectors)
  allocate(summa_struct(1)%indxStruct%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%mparStruct%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%progStruct%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%diagStruct%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%fluxStruct%gru(indx_gru)%hru(num_hru))
  ! Basin-Average structures
  allocate(summa_struct(1)%bvarStruct%gru(indx_gru)%hru(num_hru))
  ! Finalize Stats for writing
  allocate(summa_struct(1)%finalizeStats%gru(indx_gru)%hru(num_hru))
  ! TODO: IS this needed - upArea?
  allocate(summa_struct(1)%upArea%gru(indx_gru)%hru(num_hru))
  allocate(summa_struct(1)%dparStruct%gru(indx_gru)%hru(num_hru))


  call allocLocal(bpar_meta,summa_struct(1)%bparStruct%gru(indx_gru), &
                  nSnow=0,nSoil=0,err=err,message=message);
  do iHRU=1,num_hru
   ! get the number of snow and soil layers
    associate(&
    nSnow => gru_struc(indx_gru)%hruInfo(iHRU)%nSnow, & ! number of snow layers for each HRU
    nSoil => gru_struc(indx_gru)%hruInfo(iHRU)%nSoil  ) ! number of soil layers for each HRU

      ! Allocate variables that do not require time
      do iStruct=1,size(structInfo)
        select case(trim(structInfo(iStruct)%structName))
        case('time')
          call alloc_outputStruc(time_meta,summa_struct(1)%timeStruct%gru(indx_gru)%hru(iHRU), &
                                      nSteps=output_buffer_steps,err=err,message=message) 
        case('forc')
          call alloc_outputStruc(forc_meta,summa_struct(1)%forcStruct%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,message=message)
          call alloc_outputStruc(statForc_meta(:)%var_info,summa_struct(1)%forcStat%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,message=message); 
        case('attr')
          call allocLocal(attr_meta,summa_struct(1)%attrStruct%gru(indx_gru)%hru(iHRU),nSnow,nSoil,err,message)
        case('type')
          call allocLocal(type_meta,summa_struct(1)%typeStruct%gru(indx_gru)%hru(iHRU),nSnow,nSoil,err,message)
        case('id'  )
          call allocLocal(id_meta,  summa_struct(1)%idStruct%gru(indx_gru)%hru(iHRU),nSnow,nSoil,err,message)
        case('mpar')
          call allocLocal(mpar_meta,summa_struct(1)%mparStruct%gru(indx_gru)%hru(iHRU),nSnow,nSoil,err,message)
        case('indx')
          call alloc_outputStruc(indx_meta,summa_struct(1)%indxStruct%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,str_name='indx',message=message);
          call alloc_outputStruc(statIndx_meta(:)%var_info,summa_struct(1)%indxStat%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,str_name='indx',message=message);
        case('prog')
          call alloc_outputStruc(prog_meta,summa_struct(1)%progStruct%gru(indx_gru)%hru(iHRU), &
                                  nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,str_name='prog',message=message);
          call alloc_outputStruc(statProg_meta(:)%var_info,summa_struct(1)%progStat%gru(indx_gru)%hru(iHRU), &
                                  nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,str_name='prog',message=message);
        case('diag')
          call alloc_outputStruc(diag_meta,summa_struct(1)%diagStruct%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,message=message);
          call alloc_outputStruc(statDiag_meta(:)%var_info,summa_struct(1)%diagStat%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,message=message);
        case('flux')
          call alloc_outputStruc(flux_meta,summa_struct(1)%fluxStruct%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,message=message);    ! model fluxes
          call alloc_outputStruc(statFlux_meta(:)%var_info,summa_struct(1)%fluxStat%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=maxSnowLayers,nSoil=nSoil,err=err,message=message);
        case('bpar'); cycle;
        case('bvar')
          call alloc_outputStruc(bvar_meta,summa_struct(1)%bvarStruct%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=0,nSoil=0,err=err,str_name='bvar',message=message);  ! basin-average variables
          call alloc_outputStruc(statBvar_meta(:)%var_info,summa_struct(1)%bvarStat%gru(indx_gru)%hru(iHRU), &
                                 nSteps=output_buffer_steps,nSnow=0,nSoil=0,err=err,str_name='bvar',message=message);  ! basin-average variables
        case('deriv'); cycle;
#ifdef V4_ACTIVE     
        case('lookup'); call allocLocal(lookup_meta,summa_struct(1)%lookupStruct,nSnow,nSoil,err,message);
#endif
        end select
      end do

      ! allocate space for default model parameters
	    ! NOTE: This is done here, rather than in the loop above, because dpar is not one of the "standard" data structures
      call allocLocal(mpar_meta,summa_struct(1)%dparStruct%gru(indx_gru)%hru(iHRU),nSnow,nSoil,err,message)

      ! Finalize Stats Structre
      ! NOTE: This is done here, rather than in the loop above, because finalizeStats is not one of the "standard" data structures
      allocate(summa_struct(1)%finalizeStats%gru(indx_gru)%hru(iHRU)%tim(output_buffer_steps))
      do iStep = 1, output_buffer_steps
        allocate(summa_struct(1)%finalizeStats%gru(indx_gru)%hru(iHRU)%tim(iStep)%dat(1:maxVarFreq))
        summa_struct(1)%finalizeStats%gru(indx_gru)%hru(iHRU)%tim(iStep)%dat(:) = .false.
      end do ! timeSteps
    end associate
  end do
end subroutine allocateOutputBuffer

subroutine alloc_outputStruc(metaStruct,dataStruct,nSteps,nSnow,nSoil,str_name,err,message)
  USE data_types
  USE actor_data_types
  USE var_lookup,only:iLookINDEX
  USE var_lookup,only:maxvarFreq             ! allocation dimension (output frequency)
  implicit none
  type(var_info),intent(in)            :: metaStruct(:)
  class(*),intent(inout)               :: dataStruct
  ! optional input
  integer(i4b),intent(in),optional     :: nSteps
  integer(i4b),intent(in),optional     :: nSnow          ! number of snow layers
  integer(i4b),intent(in),optional     :: nSoil          ! number of soil layers
  character(len=*),intent(in),optional :: str_name    ! name of the structure to allocate
  ! output
  integer(i4b),intent(inout)           :: err            ! error code
  character(*),intent(out)             :: message        ! error message
  ! local
  logical(lgt)                         :: check          ! .true. if the variables are allocated
  logical(lgt)                         :: allocAllFlag   ! .true. if struct is to have all timesteps allocated
  integer(i4b)                         :: nVars          ! number of variables in the metadata structure
  integer(i4b)                         :: nLayers        ! total number of layers
  integer(i4b)                         :: iVar
  integer(i4b)                         :: iStat          ! checks if we want this variable
  character(len=256)                   :: cmessage       ! error message of the downwind routine
  ! initalize error control
  message='alloc_outputStruc'

  allocAllFlag = .false.
  if (present(str_name)) then
    allocAllFlag = .true.
  end if

  nVars = size(metaStruct)
  if(present(nSnow) .or. present(nSoil))then
    ! check both are present
    if(.not.present(nSoil))then; err=20; message=trim(message)//'expect nSoil to be present when nSnow is present'; print*,message; return; end if
    if(.not.present(nSnow))then; err=20; message=trim(message)//'expect nSnow to be present when nSoil is present'; print*,message; return; end if
    nLayers = nSnow+nSoil
    ! It is possible that nSnow and nSoil are actually needed here, so we return an error if the optional arguments are missing when needed
  else
    select type(dataStruct)
      class is (var_time_ilength); err=20
      class is (var_time_dlength); err=20
    end select
    if(err/=0)then; message=trim(message)//'expect nSnow and nSoil to be present for variable-length data structures'; print*,message; return; end if
  end if

  check=.false.
  ! allocate the space for the variables and thier time steps in the output structure
  select type(dataStruct)
    ! ****************************************************
    class is (var_time_i)
      if(allocated(dataStruct%var))then
        check=.true.
      else 
        allocate(dataStruct%var(nVars),stat=err)
      end if
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar) .or. allocAllFlag)then
          allocate(dataStruct%var(iVar)%tim(nSteps))
        end if
      end do
      return
    ! ****************************************************
    class is (var_time_i8)
      if(allocated(dataStruct%var))then 
        check=.true.
      else 
        allocate(dataStruct%var(nVars),stat=err) 
      end if 
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar) .or. allocAllFlag)then
          allocate(dataStruct%var(iVar)%tim(nSteps))
        end if
      end do
      return
    ! ****************************************************
    class is (var_time_d)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars),stat=err)
      end if
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar) .or. allocAllFlag)then
          allocate(dataStruct%var(iVar)%tim(nSteps))
        end if
      end do
      return
    ! ****************************************************   
    class is (var_d)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars),stat=err)
      end if
      return
    ! ****************************************************
    class is (var_i)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars),stat=err)
      end if
      return
    ! ****************************************************    
    class is (var_i8)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars), stat=err)
      end if
      return
    ! ****************************************************    
    class is (var_dlength)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars),stat=err)
        call allocateDat_rkind(metaStruct,dataStruct,nSnow,nSoil,err,cmessage)
      end if
    ! ****************************************************
    class is (var_time_ilength)
      if(allocated(dataStruct%var))then
        check=.true. 
      else 
        allocate(dataStruct%var(nVars),stat=err) 
      end if
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar) .or. allocAllFlag .or. (present(str_name) .and. &
         ((iVar == iLookINDEX%nLayers) .or. (iVar == iLookINDEX%nSnow) .or. (iVar == iLookINDEX%nSoil)) ))then
        allocate(dataStruct%var(iVar)%tim(nSteps))
          call allocateDat_int(metaStruct,dataStruct,nSnow,nSoil,nSteps,iVar,err,cmessage)
        end if
      end do
    ! ****************************************************
    class is (var_time_dlength)
      if(allocated(dataStruct%var))then
        check=.true.
      else 
        allocate(dataStruct%var(nVars),stat=err)
      end if
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar) .or. allocAllFlag)then
          if (allocated(dataStruct%var(iVar)%tim)) then
            print*, "Already Allocated"; return;
          end if
          allocate(dataStruct%var(iVar)%tim(nSteps), stat=err)
          call allocateDat_rkind_nSteps(metaStruct,dataStruct,nSnow,nSoil,nSteps,iVar,err,cmessage)
        end if
      end do
    ! ****************************************************
    class default; err=20; message=trim(message)//'unable to identify derived data type for the variable dimension'; print*,message;return
  end select
  ! check errors
  if(check) then; err=20; message=trim(message)//'structure was unexpectedly allocated already'; print*,message; return; end if
  if(err/=0)then; err=20; message=trim(message)//'problem allocating'; print*,message; return; end if

  ! check errors
  if(err/=0)then; message=trim(message)//trim(cmessage); print*, message; return; end if
end subroutine

logical function is_var_desired(metaStruct, iVar)
  USE data_types
  USE var_lookup,only:maxvarFreq             ! allocation dimension (output frequency)
  implicit none
  type(var_info),intent(in) :: metaStruct(:)
  integer(i4b),intent(in)   :: iVar
  ! local
  integer(i4b)              :: iFreq
  ! initalize error control
  is_var_desired=.false.
  do iFreq=1,maxvarFreq
    if(metaStruct(iVar)%statIndex(iFreq) /= integerMissing)then
      is_var_desired=.true.
      exit
    end if
  end do

end function is_var_desired

subroutine allocateDat_rkind_nSteps(metadata,varData,nSnow, nSoil, &
  nSteps,iVar,err,message)
  USE data_types
  USE actor_data_types
  USE var_lookup,only:iLookVarType           ! look up structure for variable typed

  USE globalData,only:nTimeDelay            ! number of timesteps in the time delay histogram
  USE globalData,only:nBand                 ! number of spectral bands
  USE var_lookup,only:maxvarFreq             ! allocation dimension (output frequency)
  USE get_ixName_module,only:get_varTypeName       ! to access type strings for error messages

  implicit none
  type(var_info),intent(in)            :: metadata(:)
  ! output variables
  type(var_time_dlength),intent(inout) :: varData     ! model variables for a local HRU
  integer(i4b),intent(in)              :: nSnow
  integer(i4b),intent(in)              :: nSoil
  integer(i4b),intent(in)              :: nSteps
  integer(i4b),intent(in)              :: iVar
  integer(i4b),intent(inout)           :: err         ! error code
  character(*),intent(inout)           :: message     ! error message

  ! local variables
  integer(i4b)                         :: iStep 
  integer(i4b)                         :: nLayers
  message='allocateDat_rkindAccessActor'

  nLayers = nSnow+nSoil
  do iStep=1, nSteps
    select case(metadata(iVar)%vartype)
      case(iLookVarType%scalarv); allocate(varData%var(iVar)%tim(iStep)%dat(1),stat=err)
      case(iLookVarType%wLength); allocate(varData%var(iVar)%tim(iStep)%dat(nBand),stat=err)
      case(iLookVarType%midSnow); allocate(varData%var(iVar)%tim(iStep)%dat(nSnow),stat=err)
      case(iLookVarType%midSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
      case(iLookVarType%midToto); allocate(varData%var(iVar)%tim(iStep)%dat(nLayers),stat=err)
      case(iLookVarType%ifcSnow); allocate(varData%var(iVar)%tim(iStep)%dat((nLayers-nSoil)+1),stat=err)
      case(iLookVarType%ifcSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil+1),stat=err)
      case(iLookVarType%ifcToto); allocate(varData%var(iVar)%tim(iStep)%dat(nLayers+1),stat=err)
      case(iLookVarType%parSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
      case(iLookVarType%routing); allocate(varData%var(iVar)%tim(iStep)%dat(nTimeDelay),stat=err)
      case(iLookVarType%outstat); allocate(varData%var(iVar)%tim(iStep)%dat(maxvarfreq*2),stat=err)
      case(iLookVarType%unknown); allocate(varData%var(iVar)%tim(iStep)%dat(0),stat=err)
      case default
      err=40; message=trim(message)//"1. unknownVariableType[name='"//trim(metadata(iVar)%varname)//"'; type='"//trim(get_varTypeName(metadata(iVar)%vartype))//"']"
      return
    end select
  end do ! (iStep)

end subroutine allocateDat_rkind_nSteps

subroutine allocateDat_rkind(metadata,varData,nSnow,nSoil,err,message)
  USE get_ixName_module,only:get_varTypeName       ! to access type strings for error messages
  USE data_types
  USE var_lookup,only:iLookVarType           ! look up structure for variable typed
  USE var_lookup,only:maxvarFreq             ! allocation dimension (output frequency)
  USE globalData,only:nBand                 ! number of spectral bands
  USE globalData,only:nTimeDelay            ! number of timesteps in the time delay histogram
  implicit none
  type(var_info),intent(in)         :: metadata(:)
  ! output variables
  type(var_dlength),intent(inout)   :: varData     ! model variables for a local HRU
  integer(i4b),intent(in)           :: nSnow
  integer(i4b),intent(in)           :: nSoil
  
  integer(i4b),intent(inout)        :: err         ! error code
  character(*),intent(inout)        :: message     ! error message
  
  ! local variables
  integer(i4b)                      :: nVars
  integer(i4b)                      :: iVar
  integer(i4b)                      :: nLayers
  message='allocateDat_rkindAccessActor'

  nVars = size(metaData)
  nLayers = nSnow+nSoil
  do iVar=1, nVars
    select case(metadata(iVar)%vartype)
    case(iLookVarType%scalarv); allocate(varData%var(iVar)%dat(1),stat=err)
    case(iLookVarType%wLength); allocate(varData%var(iVar)%dat(nBand),stat=err)
    case(iLookVarType%midSnow); allocate(varData%var(iVar)%dat(nSnow),stat=err)
    case(iLookVarType%midSoil); allocate(varData%var(iVar)%dat(nSoil),stat=err)
    case(iLookVarType%midToto); allocate(varData%var(iVar)%dat(nLayers),stat=err)
    case(iLookVarType%ifcSnow); allocate(varData%var(iVar)%dat((nLayers-nSoil)+1),stat=err)
    case(iLookVarType%ifcSoil); allocate(varData%var(iVar)%dat(nSoil+1),stat=err)
    case(iLookVarType%ifcToto); allocate(varData%var(iVar)%dat(nLayers+1),stat=err)
    case(iLookVarType%parSoil); allocate(varData%var(iVar)%dat(nSoil),stat=err)
    case(iLookVarType%routing); allocate(varData%var(iVar)%dat(nTimeDelay),stat=err)
    case(iLookVarType%outstat); allocate(varData%var(iVar)%dat(maxvarfreq*2),stat=err)
    case(iLookVarType%unknown); allocate(varData%var(iVar)%dat(0),stat=err)
    case default
        err=40; message=trim(message)//"1. unknownVariableType[name='"//trim(metadata(iVar)%varname)//"'; type='"//trim(get_varTypeName(metadata(iVar)%vartype))//"']"
        return
    end select
  end do

end subroutine allocateDat_rkind

subroutine allocateDat_int(metadata,varData,nSnow, nSoil, &
                           nSteps,iVar,err,message)
  USE get_ixName_module,only:get_varTypeName       ! to access type strings for error messages
  USE data_types
  USE actor_data_types
  USE var_lookup,only:iLookVarType           ! look up structure for variable typed
  USE var_lookup,only:maxvarFreq             ! allocation dimension (output frequency)
  USE globalData,only:nBand                 ! number of spectral bands
  USE globalData,only:nTimeDelay            ! number of timesteps in the time delay histogram
  implicit none
  type(var_info),intent(in)            :: metadata(:)
  ! output variables
  type(var_time_ilength),intent(inout) :: varData     ! model variables for a local HRU
  integer(i4b),intent(in)              :: nSnow
  integer(i4b),intent(in)              :: nSoil
  integer(i4b),intent(in)              :: nSteps
  integer(i4b),intent(in)              :: iVar  
  integer(i4b),intent(inout)           :: err         ! error code
  character(*),intent(inout)           :: message     ! error message
  ! local variables
  integer(i4b)                         :: iStep 
  integer(i4b)                         :: nLayers
  message='allocateDat_rkindAccessActor'

  nLayers = nSnow+nSoil
  do iStep=1, nSteps
    select case(metadata(iVar)%vartype)
      case(iLookVarType%scalarv); allocate(varData%var(iVar)%tim(iStep)%dat(1),stat=err)
      case(iLookVarType%wLength); allocate(varData%var(iVar)%tim(iStep)%dat(nBand),stat=err)
      case(iLookVarType%midSnow); allocate(varData%var(iVar)%tim(iStep)%dat(nSnow),stat=err)
      case(iLookVarType%midSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
      case(iLookVarType%midToto); allocate(varData%var(iVar)%tim(iStep)%dat(nLayers),stat=err)
      case(iLookVarType%ifcSnow); allocate(varData%var(iVar)%tim(iStep)%dat((nLayers-nSoil)+1),stat=err)
      case(iLookVarType%ifcSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil+1),stat=err)
      case(iLookVarType%ifcToto); allocate(varData%var(iVar)%tim(iStep)%dat(nLayers+1),stat=err)
      case(iLookVarType%parSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
      case(iLookVarType%routing); allocate(varData%var(iVar)%tim(iStep)%dat(nTimeDelay),stat=err)
      case(iLookVarType%outstat); allocate(varData%var(iVar)%tim(iStep)%dat(maxvarfreq*2),stat=err)
      case(iLookVarType%unknown); allocate(varData%var(iVar)%tim(iStep)%dat(0),stat=err)
      case default
      err=40; message=trim(message)//"1. unknownVariableType[name='"//trim(metadata(iVar)%varname)//"'; type='"//trim(get_varTypeName(metadata(iVar)%vartype))//"']"
      return
    end select
  end do ! loop through time steps
end subroutine allocateDat_int


end module gru_actor
