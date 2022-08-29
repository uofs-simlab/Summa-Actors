! SUMMA - Structure for Unifying Multiple Modeling Alternatives
! Copyright (C) 2014-2020 NCAR/RAL; University of Saskatchewan; University of Washington
!
! This file is part of SUMMA
!
! For more information see: http://www.ral.ucar.edu/projects/summa
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module SummaActors_modelRun
! calls the model physics

USE data_types,only:&
                    ! no spatial dimension
                    var_i,               & ! x%var(:)            (i4b)
                    var_i8,              & ! x%var(:)            (i8b)
                    var_d,               & ! x%var(:)            (dp)
                    var_ilength,         & ! x%var(:)%dat        (i4b)
                    var_dlength,         & ! x%var(:)%dat        (dp)
                    var_dlength_array      ! x%struc(:)%dat       (dp)
! access missing values
USE globalData,only:integerMissing         ! missing integer
USE globalData,only:realMissing            ! missing double precision number

! provide access to Noah-MP constants
USE module_sf_noahmplsm,only:isWater       ! parameter for water land cover type

! named variables
USE globalData,only:yes,no                 ! .true. and .false.
USE globalData,only:overwriteRSMIN         ! flag to overwrite RSMIN
USE globalData,only:maxSoilLayers          ! Maximum Number of Soil Layers
! urban vegetation category (could be local)
USE globalData,only:urbanVegCategory       ! vegetation category for urban areas
USE globalData,only:greenVegFrac_monthly   ! fraction of green vegetation in each month (0-1)
! provide access to the named variables that describe elements of parameter structures
USE var_lookup,only:iLookTYPE              ! look-up values for classification of veg, soils etc.
USE var_lookup,only:iLookID                ! look-up values for hru and gru IDs
USE var_lookup,only:iLookATTR              ! look-up values for local attributes
USE var_lookup,only:iLookFLUX              ! look-up values for local column model fluxes
USE var_lookup,only:iLookBVAR              ! look-up values for basin-average model variables
USE var_lookup,only:iLookTIME              ! named variables for time data structure
USE var_lookup,only:iLookDIAG              ! look-up values for local column model diagnostic variables
USE var_lookup,only:iLookINDEX             ! look-up values for local column index variables
USE var_lookup,only:iLookPROG              ! look-up values for local column model prognostic (state) variables
USE var_lookup,only:iLookPARAM             ! look-up values for local column model parameters
USE var_lookup,only:iLookDECISIONS         ! look-up values for model decisions
USE summa4chm_util,only:handle_err

! Noah-MP parameters
USE NOAHMP_VEG_PARAMETERS,only:SAIM,LAIM   ! 2-d tables for stem area index and leaf area index (vegType,month)
USE NOAHMP_VEG_PARAMETERS,only:HVT,HVB     ! height at the top and bottom of vegetation (vegType)
USE noahmp_globals,only:RSMIN

! provide access to the named variables that describe model decisions
USE mDecisions_module,only:&               ! look-up values for LAI decisions
 monthlyTable,& ! LAI/SAI taken directly from a monthly table for different vegetation classes
 specified,&    ! LAI/SAI computed from green vegetation fraction and winterSAI and summerLAI parameters   
 localColumn, & ! separate groundwater representation in each local soil column
 singleBasin, & ! single groundwater store over the entire basin
 bigBucket


! safety: set private unless specified otherwise
implicit none
private
public::SummaActors_runPhysics
contains

 ! calls the model physics
 subroutine SummaActors_runPhysics(&
                indxHRU,            &
                modelTimeStep,      &
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
                bvarStruct,         & ! x%var(:)%dat        -- basin-average variables
                fracJulDay,         &
                tmZoneOffsetFracDay,& 
                yearLength,         &
                ! run time variables
                computeVegFlux,     & ! flag to indicate if we are computing fluxes over vegetation
                dt_init,            & ! used to initialize the length of the sub-step for each HRU
                dt_init_factor,     & ! Used to adjust the length of the timestep in the event of a failure
                err, message)
 ! ---------------------------------------------------------------------------------------
 ! * desired modules
 ! ---------------------------------------------------------------------------------------
 ! data types
 USE nrtype                                   ! variable types, etc.
 ! subroutines and functions
 USE nr_utility_module,only:indexx            ! sort vectors in ascending order
 USE vegPhenlgy_module,only:vegPhenlgy        ! module to compute vegetation phenology
 USE time_utils_module,only:elapsedSec        ! calculate the elapsed time
 USE module_sf_noahmplsm,only:redprm          ! module to assign more Noah-MP parameters
 USE derivforce_module,only:derivforce        ! module to compute derived forcing data
 USE coupled_em_module,only:coupled_em        ! module to run the coupled energy and mass model
 USE qTimeDelay_module,only:qOverland         ! module to route water through an "unresolved" river network
 ! global data
 USE globalData,only:model_decisions          ! model decision structure
 USE globalData,only:startPhysics,endPhysics  ! date/time for the start and end of the initialization
 USE globalData,only:elapsedPhysics           ! elapsed time for the initialization
 
 ! ---------------------------------------------------------------------------------------
 ! * variables
 ! ---------------------------------------------------------------------------------------
 implicit none
 ! dummy variables
 integer(i4b),intent(in)                  :: indxHRU                ! id of HRU                   
 integer(i4b),intent(in)                  :: modelTimeStep          ! time step index
 ! primary data structures (scalars)
 type(var_i),intent(inout)                :: timeStruct             ! model time data
 type(var_d),intent(inout)                :: forcStruct             ! model forcing data
 type(var_d),intent(inout)                :: attrStruct             ! local attributes for each HRU
 type(var_i),intent(inout)                :: typeStruct             ! local classification of soil veg etc. for each HRU
 ! primary data structures (variable length vectors)
 type(var_ilength),intent(inout)          :: indxStruct             ! model indices
 type(var_dlength),intent(in)             :: mparStruct             ! model parameters
 type(var_dlength),intent(inout)          :: progStruct             ! model prognostic (state) variables
 type(var_dlength),intent(inout)          :: diagStruct             ! model diagnostic variables
 type(var_dlength),intent(inout)          :: fluxStruct             ! model fluxes
 ! basin-average structures
 type(var_dlength),intent(inout)          :: bvarStruct             ! basin-average variables
 real(dp),intent(inout)                   :: fracJulDay
 real(dp),intent(inout)                   :: tmZoneOffsetFracDay 
 integer(i4b),intent(inout)               :: yearLength
 integer(i4b),intent(inout)               :: computeVegFlux         ! flag to indicate if we are computing fluxes over vegetation
 real(dp),intent(inout)                   :: dt_init
 integer(i4b),intent(in)                  :: dt_init_factor         ! Used to adjust the length of the timestep in the event of a failure
 integer(i4b),intent(inout)               :: err                    ! error code
 character(*),intent(out)                 :: message                ! error message
 ! ---------------------------------------------------------------------------------------
 ! local variables: general
 real(dp)                                 :: fracHRU                ! fractional area of a given HRU (-)
 character(LEN=256)                       :: cmessage               ! error message of downwind routine
 ! local variables: veg phenology
 logical(lgt)                             :: computeVegFluxFlag     ! flag to indicate if we are computing fluxes over vegetation (.false. means veg is buried with snow)
 real(dp)                                 :: notUsed_canopyDepth    ! NOT USED: canopy depth (m)
 real(dp)                                 :: notUsed_exposedVAI     ! NOT USED: exposed vegetation area index (m2 m-2)
 integer(i4b)                             :: nSnow                  ! number of snow layers
 integer(i4b)                             :: nSoil                  ! number of soil layers
 integer(i4b)                             :: nLayers                ! total number of layers
 real(dp), ALLOCATABLE                    :: zSoilReverseSign(:)    ! height at bottom of each soil layer, negative downwards (m)
 ! ---------------------------------------------------------------------------------------
 ! initialize error control
 err=0; !message='SummaActors_runPhysics/'
 
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
    if(err/=0)then
      message=trim(message)//trim(cmessage)
      print*, message
      return
    endif
  
    ! save the flag for computing the vegetation fluxes
    if(computeVegFluxFlag)      computeVegFlux = yes
    if(.not.computeVegFluxFlag) computeVegFlux = no
     
    ! define the green vegetation fraction of the grid box (used to compute LAI)
    diagStruct%var(iLookDIAG%scalarGreenVegFraction)%dat(1) = greenVegFrac_monthly(timeStruct%var(iLookTIME%im))
 end if  ! if the first time step
 

 ! ****************************************************************************
 ! *** model simulation
 ! ****************************************************************************

 ! initialize the start of the physics
 call date_and_time(values=startPhysics)
 
 !****************************************************************************** 
 !****************************** From run_oneGRU *******************************
 !******************************************************************************
 ! ----- basin initialization --------------------------------------------------------------------------------------------

 
 ! initialize runoff variables
  bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1)    = 0._dp  ! surface runoff (m s-1)
  bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)     = 0._dp 
  bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)    = 0._dp  ! outflow from all "outlet" HRUs (those with no downstream HRU)
  bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1)      = 0._dp 

 ! initialize baseflow variables
  bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)  = 0._dp ! recharge to the aquifer (m s-1)
  bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  = 0._dp ! baseflow from the aquifer (m s-1)
  bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1) = 0._dp ! transpiration loss from the aquifer (m s-1)

  ! initialize total inflow for each layer in a soil column
  if (modelTimeStep == 0 .and. indxHRU == 1)then
    fluxStruct%var(iLookFLUX%mLayerColumnInflow)%dat(:) = 0._dp
  end if
 
 ! update the number of layers
 nSnow   = indxStruct%var(iLookINDEX%nSnow)%dat(1)    ! number of snow layers
 nSoil   = indxStruct%var(iLookINDEX%nSoil)%dat(1)    ! number of soil layers
 nLayers = indxStruct%var(iLookINDEX%nLayers)%dat(1)  ! total number of layers
 
 computeVegFluxFlag = (ComputeVegFlux == yes)

 !******************************************************************************
 !****************************** From run_oneHRU *******************************
 !******************************************************************************
 ! water pixel: do nothing
 if (typeStruct%var(iLookTYPE%vegTypeIndex) == isWater) return

 ! get height at bottom of each soil layer, negative downwards (used in Noah MP)
 allocate(zSoilReverseSign(nSoil),stat=err)
 if(err/=0)then
  message=trim(message)//'problem allocating space for zSoilReverseSign'
  print*, message
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
  print*, message
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
 if(err/=0)then
  err=20
  message=trim(message)//trim(cmessage)
  print*, message
  return 
endif
 
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


 !************************************* End of run_oneHRU *****************************************
 ! save the flag for computing the vegetation fluxes
 if(computeVegFluxFlag)      ComputeVegFlux = yes
 if(.not.computeVegFluxFlag) ComputeVegFlux = no

 fracHRU = attrStruct%var(iLookATTR%HRUarea) / bvarStruct%var(iLookBVAR%basin__totalArea)%dat(1)

 ! ----- calculate weighted basin (GRU) fluxes --------------------------------------------------------------------------------------
 
 ! increment basin surface runoff (m s-1)
 bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) = bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + fluxStruct%var(iLookFLUX%scalarSurfaceRunoff)%dat(1) * fracHRU
 
 !increment basin soil drainage (m s-1)
 bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)   = bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)  + fluxStruct%var(iLookFLUX%scalarSoilDrainage)%dat(1)  * fracHRU
 
 ! increment aquifer variables -- ONLY if aquifer baseflow is computed individually for each HRU and aquifer is run
 ! NOTE: groundwater computed later for singleBasin
 if(model_decisions(iLookDECISIONS%spatial_gw)%iDecision == localColumn .and. model_decisions(iLookDECISIONS%groundwatr)%iDecision == bigBucket) then

   bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)  = bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)   + fluxStruct%var(iLookFLUX%scalarSoilDrainage)%dat(1)     * fracHRU
   bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1) = bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1)  + fluxStruct%var(iLookFLUX%scalarAquiferTranspire)%dat(1) * fracHRU
   bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  =  bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  &
           +  fluxStruct%var(iLookFLUX%scalarAquiferBaseflow)%dat(1) * fracHRU
  end if

  ! perform the routing
  associate(totalArea => bvarStruct%var(iLookBVAR%basin__totalArea)%dat(1) )

 ! compute water balance for the basin aquifer
 if(model_decisions(iLookDECISIONS%spatial_gw)%iDecision == singleBasin)then
  message=trim(message)//'multi_driver/bigBucket groundwater code not transferred from old code base yet'
  err=20; return
 end if

 ! calculate total runoff depending on whether aquifer is connected
 if(model_decisions(iLookDECISIONS%groundwatr)%iDecision == bigBucket) then
  ! aquifer
  bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1) = bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)/totalArea + bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)
 else
  ! no aquifer
  bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1) = bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)/totalArea + bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)
 endif

 call qOverland(&
                ! input
                model_decisions(iLookDECISIONS%subRouting)%iDecision,            &  ! intent(in): index for routing method
                bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1),            &  ! intent(in): total runoff to the channel from all active components (m s-1)
                bvarStruct%var(iLookBVAR%routingFractionFuture)%dat,             &  ! intent(in): fraction of runoff in future time steps (m s-1)
                bvarStruct%var(iLookBVAR%routingRunoffFuture)%dat,               &  ! intent(in): runoff in future time steps (m s-1)
                ! output
                bvarStruct%var(iLookBVAR%averageInstantRunoff)%dat(1),           &  ! intent(out): instantaneous runoff (m s-1)
                bvarStruct%var(iLookBVAR%averageRoutedRunoff)%dat(1),            &  ! intent(out): routed runoff (m s-1)
                err,message)                                                                  ! intent(out): error control
 if(err/=0)then; err=20; message=trim(message)//trim(cmessage); return; endif
 end associate
 
 !************************************* End of run_oneGRU *****************************************
 
  ! check errors
  call handle_err(err, cmessage)

 ! identify the end of the physics
 call date_and_time(values=endPhysics)

 ! aggregate the elapsed time for the physics
 elapsedPhysics = elapsedPhysics + elapsedSec(startPhysics, endPhysics)


 end subroutine SummaActors_runPhysics

end module SummaActors_modelRun
