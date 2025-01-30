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

module summa_modelRun
! calls the model physics
USE,intrinsic :: iso_c_binding

USE actor_data_types,only:hru_type
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
public::runPhysics
#ifdef V4_ACTIVE
public::get_steps_tolerances
public::set_steps_tolerances
#endif
contains

! Runs the model physics for an HRU
subroutine runPhysics(indxGRU, indxHRU, modelTimeStep, hru_data, &
    dt_init_factor, err, message)
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
  USE globalData,only:gru_struc
  USE globalData,only:model_decisions          ! model decision structure
  USE globalData,only:startPhysics,endPhysics  ! date/time for the start and end of the initialization
  USE globalData,only:elapsedPhysics           ! elapsed time for the initialization
  implicit none
  ! Dummy Variables
  integer(c_int),intent(in)                 :: indxGRU                ! id of GRU
  integer(c_int),intent(in)                 :: indxHRU                ! id of HRU                   
  integer(c_int), intent(in)                :: modelTimeStep          ! time step index
  type(hru_type), intent(inout)             :: hru_data               ! c_ptr to -- hru data
  integer(c_int), intent(in)                :: dt_init_factor         ! used to adjust the length of the timestep in the event of a failure
  integer(c_int), intent(inout)             :: err                    ! error code
  character(len=256), intent(out)           :: message                ! error message
  ! local variables: general
  integer(8)                                :: hruId                  ! hruId
  character(LEN=256)                        :: cmessage               ! error message of downwind routine
  ! local variables: veg phenology
  logical(lgt)                              :: computeVegFluxFlag     ! flag to indicate if we are computing fluxes over vegetation (.false. means veg is buried with snow)
  real(dp)                                  :: notUsed_canopyDepth    ! NOT USED: canopy depth (m)
  real(dp)                                  :: notUsed_exposedVAI     ! NOT USED: exposed vegetation area index (m2 m-2)
  integer(i4b)                              :: nSnow                  ! number of snow layers
  integer(i4b)                              :: nSoil                  ! number of soil layers
  integer(i4b)                              :: nLayers                ! total number of layers
  real(dp), allocatable                     :: zSoilReverseSign(:)    ! height at bottom of each soil layer, negative downwards (m)
  ! ---------------------------------------------------------------------------------------
  hruId = gru_struc(indxGRU)%hruInfo(indxHRU)%hru_id

  ! ---------------------------------------------------------------------------------------
  ! initialize error control
  err=0; message='runPhysics/'
  
  ! *******************************************************************************************
  ! *** initialize computeVegFlux (flag to indicate if we are computing fluxes over vegetation)
  ! *******************************************************************************************
  ! if computeVegFlux changes, then the number of state variables changes, and we need to reoranize the data structures
  if(modelTimeStep==1)then
      ! get vegetation phenology
      ! (compute the exposed LAI and SAI and whether veg is buried by snow)
      call vegPhenlgy(&
                      ! model control
                      model_decisions,        & ! intent(in):    model decisions
                      hru_data%fracJulDay,    & ! intent(in):    fractional julian days since the start of year
                      hru_data%yearLength,    & ! intent(in):    number of days in the current year
                      ! input/output: data structures
                      hru_data%typeStruct,    & ! intent(in):    type of vegetation and soil
                      hru_data%attrStruct,    & ! intent(in):    spatial attributes
                      hru_data%mparStruct,    & ! intent(in):    model parameters
                      hru_data%progStruct,    & ! intent(in):    model prognostic variables for a local HRU
                      hru_data%diagStruct,    & ! intent(inout): model diagnostic variables for a local HRU
                      ! output
                      computeVegFluxFlag,     & ! intent(out): flag to indicate if we are computing fluxes over vegetation (.false. means veg is buried with snow)
                      notUsed_canopyDepth,    & ! intent(out): NOT USED: canopy depth (m)
                      notUsed_exposedVAI,     & ! intent(out): NOT USED: exposed vegetation area index (m2 m-2)
                      err,cmessage)                     ! intent(out): error control
      if(err/=0)then;message=trim(message)//trim(cmessage); return; endif

    
      ! save the flag for computing the vegetation fluxes
      if(computeVegFluxFlag)      hru_data%computeVegFlux = yes
      if(.not.computeVegFluxFlag) hru_data%computeVegFlux = no
      
      ! define the green vegetation fraction of the grid box (used to compute LAI)
      hru_data%diagStruct%var(iLookDIAG%scalarGreenVegFraction)%dat(1) = greenVegFrac_monthly(hru_data%timeStruct%var(iLookTIME%im))
  end if  ! if the first time step
 

  ! ****************************************************************************
  ! *** model simulation
  ! ****************************************************************************
  ! update the number of layers
  nSnow   = hru_data%indxStruct%var(iLookINDEX%nSnow)%dat(1)    ! number of snow layers
  nSoil   = hru_data%indxStruct%var(iLookINDEX%nSoil)%dat(1)    ! number of soil layers
  nLayers = hru_data%indxStruct%var(iLookINDEX%nLayers)%dat(1)  ! total number of layers
  
  computeVegFluxFlag = (hru_data%ComputeVegFlux == yes)

  !******************************************************************************
  !****************************** From run_oneHRU *******************************
  !******************************************************************************
  ! water pixel: do nothing
  if (hru_data%typeStruct%var(iLookTYPE%vegTypeIndex) == isWater) then
      ! Set wall_clock time to zero so it does not get a random value
    hru_data%diagStruct%var(iLookDIAG%wallClockTime)%dat(1) = 0._dp 
    return
  endif

  ! get height at bottom of each soil layer, negative downwards (used in Noah MP)
  allocate(zSoilReverseSign(nSoil),stat=err)
  if(err/=0)then; message=trim(message)//'problem allocating space for zSoilReverseSign'; return; endif

  zSoilReverseSign(:) = -hru_data%progStruct%var(iLookPROG%iLayerHeight)%dat(nSnow+1:nLayers)
 
  ! populate parameters in Noah-MP modules
  ! Passing a maxSoilLayer in order to pass the check for NROOT, that is done to avoid making any changes to Noah-MP code.
  !  --> NROOT from Noah-MP veg tables (as read here) is not used in SUMMA
  call REDPRM(hru_data%typeStruct%var(iLookTYPE%vegTypeIndex),      & ! vegetation type index
              hru_data%typeStruct%var(iLookTYPE%soilTypeIndex),     & ! soil type
              hru_data%typeStruct%var(iLookTYPE%slopeTypeIndex),    & ! slope type index
              zSoilReverseSign,                            & ! * not used: height at bottom of each layer [NOTE: negative] (m)
              maxSoilLayers,                               & ! number of soil layers
              urbanVegCategory)                              ! vegetation category for urban areas

  ! deallocate height at bottom of each soil layer(used in Noah MP)
  deallocate(zSoilReverseSign,stat=err)
  if(err/=0)then;message=trim(message)//'problem deallocating space for zSoilReverseSign'; return; endif
 

  ! overwrite the minimum resistance
  if(overwriteRSMIN) RSMIN = hru_data%mparStruct%var(iLookPARAM%minStomatalResistance)%dat(1)
  
  ! overwrite the vegetation height
  HVT(hru_data%typeStruct%var(iLookTYPE%vegTypeIndex)) = hru_data%mparStruct%var(iLookPARAM%heightCanopyTop)%dat(1)
  HVB(hru_data%typeStruct%var(iLookTYPE%vegTypeIndex)) = hru_data%mparStruct%var(iLookPARAM%heightCanopyBottom)%dat(1)

  ! overwrite the tables for LAI and SAI
  if(model_decisions(iLookDECISIONS%LAI_method)%iDecision == specified)then
    SAIM(hru_data%typeStruct%var(iLookTYPE%vegTypeIndex),:) = hru_data%mparStruct%var(iLookPARAM%winterSAI)%dat(1)
    LAIM(hru_data%typeStruct%var(iLookTYPE%vegTypeIndex),:) = hru_data%mparStruct%var(iLookPARAM%summerLAI)%dat(1)*greenVegFrac_monthly
  end if
 
  ! compute derived forcing variables
  call derivforce(&
        hru_data%timeStruct%var,     & ! vector of time information
        hru_data%forcStruct%var,     & ! vector of model forcing data
        hru_data%attrStruct%var,     & ! vector of model attributes
        hru_data%mparStruct,         & ! data structure of model parameters
        hru_data%progStruct,         & ! data structure of model prognostic variables
        hru_data%diagStruct,         & ! data structure of model diagnostic variables
        hru_data%fluxStruct,         & ! data structure of model fluxes
        hru_data%tmZoneOffsetFracDay,         & ! time zone offset in fractional days
        err,cmessage)                  ! error control
  if(err/=0)then;err=20; message=trim(message)//cmessage; return; endif
 
  ! initialize the number of flux calls
  hru_data%diagStruct%var(iLookDIAG%numFluxCalls)%dat(1) = 0._dp

  ! run the model for a single HRU
  call coupled_em(&
                  ! model control
                  hruId,                       & ! intent(in):    hruID
                  hru_data%dt_init,            & ! intent(inout): initial time step
                  dt_init_factor,              & ! Used to adjust the length of the timestep in the event of a failure
                  computeVegFluxFlag,          & ! intent(inout): flag to indicate if we are computing fluxes over vegetation
                  hru_data%fracJulDay,         & ! intent(in):    fractional julian days since the start of year
                  hru_data%yearLength,         & ! intent(in):    number of days in the current year
                  ! data structures (input)
                  hru_data%typeStruct,         & ! intent(in):    local classification of soil veg etc. for each HRU
                  hru_data%attrStruct,         & ! intent(in):    local attributes for each HRU
                  hru_data%forcStruct,         & ! intent(in):    model forcing data
                  hru_data%mparStruct,         & ! intent(in):    model parameters
                  hru_data%bvarStruct,         & ! intent(in):    basin-average model variables
#ifdef V4_ACTIVE                  
                  hru_data%lookupStruct,       &
#endif
                  ! data structures (input-output)
                  hru_data%indxStruct,         & ! intent(inout): model indices
                  hru_data%progStruct,         & ! intent(inout): model prognostic variables for a local HRU
                  hru_data%diagStruct,         & ! intent(inout): model diagnostic variables for a local HRU
                  hru_data%fluxStruct,         & ! intent(inout): model fluxes for a local HRU
                  ! error control
                  err,cmessage)       ! intent(out): error control
  if(err/=0)then; err=20; message=trim(message)//trim(cmessage); return; endif;


  !************************************* End of run_oneHRU *****************************************
  ! save the flag for computing the vegetation fluxes
  if(computeVegFluxFlag)      hru_data%ComputeVegFlux = yes
  if(.not.computeVegFluxFlag) hru_data%ComputeVegFlux = no

end subroutine runPhysics

! *******************************************************************************************
! *** get_steps_tolerances
! *******************************************************************************************
#ifdef V4_ACTIVE
subroutine get_steps_tolerances(handle_hru_data, beSteps, rtol, atolWat, atolNrg) bind(C, name='get_steps_tolerances')
  USE globalData,only:model_decisions                         ! model decision structure
  USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions
  USE var_lookup,only:iLookPARAM                              ! look-up values for local column model parameters
  implicit none

  ! dummy variables
  type(c_ptr),    intent(in), value         :: handle_hru_data        ! c_ptr to -- hru data
  integer(c_int), intent(out)               :: beSteps                ! number of backward Euler steps in data window
  real(c_double), intent(out)               :: rtol                   ! relative tolerance
  real(c_double), intent(out)               :: atolWat                ! absolute tolerance for water states
  real(c_double), intent(out)               :: atolNrg                ! absolute tolerance for energy states
  ! local variables
  type(hru_type),pointer                    :: hru_data               ! hru data
  call c_f_pointer(handle_hru_data, hru_data)

  if (trim(model_decisions(iLookDECISIONS%num_method)%cDecision)=='ida') then
    beSteps = 1 ! IDA should have full step size (value isn't used anyhow)
    ! IDA tolerances, which are set in the model decision file
    rtol = (hru_data%mparStruct%var(iLookPARAM%relTolTempCas)%dat(1) &
          + hru_data%mparStruct%var(iLookPARAM%relTolWatVeg)%dat(1) &
          + hru_data%mparStruct%var(iLookPARAM%relTolTempVeg)%dat(1) &
          + hru_data%mparStruct%var(iLookPARAM%relTolWatSnow)%dat(1) &
          + hru_data%mparStruct%var(iLookPARAM%relTolTempSoilSnow)%dat(1) &
          + hru_data%mparStruct%var(iLookPARAM%relTolMatric)%dat(1) &
          + hru_data%mparStruct%var(iLookPARAM%relTolAquifr)%dat(1))/7._rkind

    atolWat = (hru_data%mparStruct%var(iLookPARAM%absTolWatVeg)%dat(1) &
             + hru_data%mparStruct%var(iLookPARAM%absTolWatSnow)%dat(1) &
             + hru_data%mparStruct%var(iLookPARAM%absTolMatric)%dat(1) &
             + hru_data%mparStruct%var(iLookPARAM%absTolAquifr)%dat(1))/4._rkind
    atolNrg = (hru_data%mparStruct%var(iLookPARAM%absTolTempCas)%dat(1) &
             + hru_data%mparStruct%var(iLookPARAM%absTolTempVeg)%dat(1) &
             + hru_data%mparStruct%var(iLookPARAM%absTolTempSoilSnow)%dat(1))/3._rkind
  else ! all other methods are currently BE -- 'homegrown' ('itertive'), 'kinsol'
    beSteps = NINT(hru_data%mparStruct%var(iLookPARAM%be_steps)%dat(1))
    rtol = -9999    ! BE doesn't use these
    atolWat = -9999
    atolNrg = -9999
  endif
end subroutine get_steps_tolerances

! *******************************************************************************************
! *** get_steps_tolerances
! *******************************************************************************************
subroutine set_steps_tolerances(handle_hru_data, beSteps, rtol, atolWat, atolNrg) bind(C, name='set_steps_tolerances')
  USE var_lookup,only:iLookPARAM                              ! look-up values for local column model parameters
  implicit none

  ! dummy variables
  type(c_ptr), intent(in), value           :: handle_hru_data        ! c_ptr to -- hru data
  integer(c_int), intent(in)               :: beSteps                ! number of backward Euler steps in data window
  real(c_double), intent(in)               :: rtol                   ! relative tolerance
  real(c_double), intent(in)               :: atolWat                ! absolute tolerance for water
  real(c_double), intent(in)               :: atolNrg                ! absolute tolerance for energy

  ! local variables
  type(hru_type),pointer                    :: hru_data              ! hru data
  call c_f_pointer(handle_hru_data, hru_data)

  ! set beSteps
  hru_data%mparStruct%var(iLookPARAM%be_steps)%dat(1) = REAL(beSteps)
  ! Set rtols
  hru_data%mparStruct%var(iLookPARAM%relTolTempCas)%dat(1) = rtol  
  hru_data%mparStruct%var(iLookPARAM%relTolTempVeg)%dat(1) = rtol  
  hru_data%mparStruct%var(iLookPARAM%relTolWatVeg)%dat(1) = rtol  
  hru_data%mparStruct%var(iLookPARAM%relTolTempSoilSnow)%dat(1) = rtol  
  hru_data%mparStruct%var(iLookPARAM%relTolWatSnow)%dat(1) = rtol  
  hru_data%mparStruct%var(iLookPARAM%relTolMatric)%dat(1) = rtol  
  hru_data%mparStruct%var(iLookPARAM%relTolAquifr)%dat(1) = rtol  
  ! Set atols
  hru_data%mparStruct%var(iLookPARAM%absTolTempCas)%dat(1) = atolNrg
  hru_data%mparStruct%var(iLookPARAM%absTolTempVeg)%dat(1) = atolNrg 
  hru_data%mparStruct%var(iLookPARAM%absTolWatVeg)%dat(1) = atolWat
  hru_data%mparStruct%var(iLookPARAM%absTolTempSoilSnow)%dat(1) = atolNrg
  hru_data%mparStruct%var(iLookPARAM%absTolWatSnow)%dat(1) = atolWat 
  hru_data%mparStruct%var(iLookPARAM%absTolMatric)%dat(1) = atolWat
  hru_data%mparStruct%var(iLookPARAM%absTolAquifr)%dat(1) = atolWat

end subroutine set_steps_tolerances
#endif

end module summa_modelRun
