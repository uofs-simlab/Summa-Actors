module hru_actor
    implicit none


    public::run_hru_for_timestep()

    contains
    subroutine run_hru_for_timestep()

    
    ! local variables:
    integer(i4b)            :: nSnow
    integer(i4b)            :: nSoil
    integer(i4b)            :: nLayers

    ! *******************************************************************************************
    ! *** initialize computeVegFlux (flag to indicate if we are computing fluxes over vegetation)
    ! *******************************************************************************************

    ! if computeVegFlux changes, then the number of state variables changes, and we need to reoranize the data structures
    if(modelTimeStep==1)then
        ! get vegetation phenology
        ! (compute the exposed LAI and SAI and whether veg is buried by snow)
        call vegPhenlgy(&
                    ! input/output: data structures
                    model_decisions,                & ! intent(in):    model decisions
                    typeStruct,                     & ! intent(in):    type of vegetation and soil
                    attrStruct,                     & ! intent(in):    spatial attributes
                    mparStruct,                     & ! intent(in):    model parameters
                    progStruct,                     & ! intent(in):    model prognostic variables for a local HRU
                    diagStruct,                     & ! intent(inout): model diagnostic variables for a local HRU
                    ! output
                    computeVegFluxFlag,             & ! intent(out): flag to indicate if we are computing fluxes over vegetation (.false. means veg is buried with snow)
                    notUsed_canopyDepth,            & ! intent(out): NOT USED: canopy depth (m)
                    notUsed_exposedVAI,             & ! intent(out): NOT USED: exposed vegetation area index (m2 m-2)
                    fracJulDay,                     &
                    yearLength,                     &
                    err,cmessage)                     ! intent(out): error control
        if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  
        ! save the flag for computing the vegetation fluxes
        if(computeVegFluxFlag)      computeVegFlux = yes
        if(.not.computeVegFluxFlag) computeVegFlux = no
        ! define the green vegetation fraction of the grid box (used to compute LAI)
        diagStruct%var(iLookDIAG%scalarGreenVegFraction)%dat(1) = greenVegFrac_monthly(timeStruct%var(iLookTIME%im))
    end if ! if first timestep

    ! initialize total inflow for each layer in a soil column
    fluxStruct%var(iLookFLUX%mLayerColumnInflow)%dat(:) = 0._dp

    ! convienence variables
    nSnow   = indxStruct%var(iLookINDEX%nSnow)%dat(1)    ! number of snow layers
    nSoil   = indxStruct%var(iLookINDEX%nSoil)%dat(1)    ! number of soil layers
    nLayers = indxStruct%var(iLookINDEX%nLayers)%dat(1)  ! total number of layers
    
    computeVegFluxFlag = (ComputeVegFlux == yes)
    
    ! water pixel: do nothing
    if (typeStruct%var(iLookTYPE%vegTypeIndex) == isWater) return

    ! get height at bottom of each soil layer, negative downwards (used in Noah MP)
    allocate(zSoilReverseSign(nSoil),stat=err)
    if(err/=0)then
        message=trim(message)//'problem allocating space for zSoilReverseSign'
        err=20; return
    endif
    zSoilReverseSign(:) = -progStruct%var(iLookPROG%iLayerHeight)%dat(nSnow+1:nLayers)
    
    ! populate parameters in Noah-MP modules
    ! Passing a maxSoilLayer in order to pass the check for NROOT, that is done to avoid making any changes to Noah-MP code.
    !  --> NROOT from Noah-MP veg tables (as read here) is not used in SUMMA
    call REDPRM(typeStruct%var(iLookTYPE%vegTypeIndex),      & ! vegetation type index
                typeStruct%var(iLookTYPE%soilTypeIndex),     & ! soil type
                typeStruct%var(iLookTYPE%slopeTypeIndex),    & ! slope type index
                zSoilReverseSign,                            & ! * not used: height at bottom of each layer [NOTE: negative] (m)
                maxSoilLayers,                               & ! number of soil layers
                urbanVegCategory)                              ! vegetation category for urban areas

    ! deallocate height at bottom of each soil layer(used in Noah MP)
    deallocate(zSoilReverseSign,stat=err)
    if(err/=0)then
        message=trim(message)//'problem deallocating space for zSoilReverseSign'
        err=20; return
    endif

    ! overwrite the minimum resistance
    if(overwriteRSMIN) RSMIN = mparStruct%var(iLookPARAM%minStomatalResistance)%dat(1)
    
    ! overwrite the vegetation height
    HVT(typeStruct%var(iLookTYPE%vegTypeIndex)) = mparStruct%var(iLookPARAM%heightCanopyTop)%dat(1)
    HVB(typeStruct%var(iLookTYPE%vegTypeIndex)) = mparStruct%var(iLookPARAM%heightCanopyBottom)%dat(1)
   
    ! overwrite the tables for LAI and SAI
    if(model_decisions(iLookDECISIONS%LAI_method)%iDecision == specified)then
     SAIM(typeStruct%var(iLookTYPE%vegTypeIndex),:) = mparStruct%var(iLookPARAM%winterSAI)%dat(1)
     LAIM(typeStruct%var(iLookTYPE%vegTypeIndex),:) = mparStruct%var(iLookPARAM%summerLAI)%dat(1)*greenVegFrac_monthly
    end if

    ! compute derived forcing variables
    call derivforce(&
            timeStruct%var,     & ! vector of time information
            forcStruct%var,     & ! vector of model forcing data
            attrStruct%var,     & ! vector of model attributes
            mparStruct,         & ! data structure of model parameters
            progStruct,         & ! data structure of model prognostic variables
            diagStruct,         & ! data structure of model diagnostic variables
            fluxStruct,         & ! data structure of model fluxes
            tmZoneOffsetFracDay,& 
            err,cmessage)       ! error control
    if(err/=0)then; err=20; message=trim(message)//trim(cmessage); return; endif

    ! initialize the number of flux calls
    diagStruct%var(iLookDIAG%numFluxCalls)%dat(1) = 0._dp

    ! run the model for a single HRU
    call coupled_em(&
        ! model control
        indxHRU,            & ! intent(in):    hruID
        dt_init,            & ! intent(inout): initial time step
        dt_init_factor,     & ! Used to adjust the length of the timestep in the event of a failure
        computeVegFluxFlag, & ! intent(inout): flag to indicate if we are computing fluxes over vegetation
        ! data structures (input)
        typeStruct,         & ! intent(in):    local classification of soil veg etc. for each HRU
        attrStruct,         & ! intent(in):    local attributes for each HRU
        forcStruct,         & ! intent(in):    model forcing data
        mparStruct,         & ! intent(in):    model parameters
        bvarStruct,         & ! intent(in):    basin-average model variables
        ! data structures (input-output)
        indxStruct,         & ! intent(inout): model indices
        progStruct,         & ! intent(inout): model prognostic variables for a local HRU
        diagStruct,         & ! intent(inout): model diagnostic variables for a local HRU
        fluxStruct,         & ! intent(inout): model fluxes for a local HRU
        fracJulDay,         &
        yearLength,         &
        ! error control
        err,cmessage)       ! intent(out): error control
    if(err/=0)then; err=20; message=trim(message)//trim(cmessage); return; endif 

    ! save the flag for computing the vegetation fluxes
    if(computeVegFluxFlag)      ComputeVegFlux = yes
    if(.not.computeVegFluxFlag) ComputeVegFlux = no

    ! The gru_actor will need the following information
    fracHRU = attrStruct%var(iLookATTR%HRUarea) / bvarStruct%var(iLookBVAR%basin__totalArea)%dat(1)
    fluxStruct%var(iLookFLUX%scalarSurfaceRunoff)%dat(1)
    fluxStruct%var(iLookFLUX%scalarSoilDrainage)%dat(1)
    fluxStruct%var(iLookFLUX%scalarAquiferTranspire)%dat(1)
    fluxStruct%var(iLookFLUX%scalarAquiferBaseflow)%dat(1)
    end subroutine

end module hru_actor