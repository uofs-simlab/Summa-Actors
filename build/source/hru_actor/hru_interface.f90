module hru_interface
  USE,intrinsic :: iso_c_binding
  USE nrtype

  implicit none
  private
  public::initHRU_fortran
  public::setupHRU_fortran
  public::readHRURestart_fortran
  public::setTimeZoneOffset_fortran
  public::readHRUForcing_fortran
  public::runHRU_fortran
  public::writeHRUOutput_fortran
  ! public::initGRU_fortran

  contains

subroutine initHRU_fortran(indx_gru, indx_hru, num_steps, handle_hru_data, &
    err, message_r) bind(C, name="initHRU_fortran")
  USE globalData,only:numtim
  USE actor_data_types,only:hru_type             
  USE INIT_HRU_ACTOR,only:initHRU
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  implicit none
  ! Dummy Variables
  integer(c_int),intent(in)       :: indx_gru
  integer(c_int),intent(in)       :: indx_hru
  integer(c_int),intent(out)      :: num_steps
  type(c_ptr),   intent(in),value :: handle_hru_data
  integer(c_int),intent(out)      :: err
  type(c_ptr),   intent(out)      :: message_r
  ! Local Variables
  type(hru_type),pointer         :: hru_data
  character(len=256)             :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_hru_data, hru_data)

  num_steps = numtim
  call initHRU(indx_gru, indx_hru, hru_data, err, message)
  if(err /= 0) then; call f_c_string_ptr(trim(message), message_r); end if
end subroutine initHRU_fortran

subroutine setupHRU_fortran(indx_gru, indx_hru, handle_hru_data, err, message_r) & 
    bind(C, name="setupHRU_fortran")
  USE actor_data_types,only:hru_type
  USE INIT_HRU_ACTOR,only:setupHRU
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  integer(c_int), intent(in)       :: indx_hru
  type(c_ptr),    intent(in),value :: handle_hru_data
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  type(hru_type),pointer         :: hru_data
  character(len=256)             :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_hru_data, hru_data)

  call setupHRU(indx_gru, indx_hru, hru_data, err, message)
  if(err /= 0) then; call f_c_string_ptr(trim(message), message_r); end if
end subroutine setupHRU_fortran

subroutine readHRURestart_fortran(indx_gru, indx_hru, handle_hru_data, &
    err, message_r) bind(C, name="readHRURestart_fortran")
  USE actor_data_types,only:hru_type
  USE INIT_HRU_ACTOR,only:readHRURestart
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  integer(c_int), intent(in)       :: indx_hru
  type(c_ptr),    intent(in),value :: handle_hru_data
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  type(hru_type),pointer           :: hru_data
  character(len=256)               :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_hru_data, hru_data)

  call readHRURestart(indx_gru, indx_hru, hru_data, err, message)
  if(err /= 0) then; call f_c_string_ptr(trim(message), message_r); end if
end subroutine readHRURestart_fortran

subroutine setTimeZoneOffset_fortran(iFile, handle_hru_data, err, message_r) &
    bind(C, name="setTimeZoneOffset_fortran")
  USE actor_data_types,only:hru_type
  USE hru_read,only:setTimeZoneOffset
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  ! Dummy Variables
  integer(c_int), intent(in)        :: iFile
  type(c_ptr),    intent(in),value  :: handle_hru_data
  integer(c_int), intent(out)       :: err
  type(c_ptr),    intent(out)       :: message_r
  ! Local Variables
  type(hru_type),pointer            :: hru_data
  character(len=256)                :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_hru_data, hru_data)

  call setTimeZoneOffset(iFile, hru_data, err, message)
  if(err /= 0) then; call f_c_string_ptr(trim(message), message_r); end if
end subroutine setTimeZoneOffset_fortran

subroutine readHRUForcing_fortran(indx_gru, indx_hru, iStep, iRead, iFile, &
    handle_hru_data, err, message_r) bind(C, name="readHRUForcing_fortran")
  USE actor_data_types,only:hru_type
  USE hru_read,only:readHRUForcing
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  integer(c_int), intent(in)       :: indx_hru
  integer(c_int), intent(in)       :: iStep
  integer(c_int), intent(inout)    :: iRead
  integer(c_int), intent(in)       :: iFile
  type(c_ptr),    intent(in),value :: handle_hru_data
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  type(hru_type),pointer           :: hru_data
  character(len=256)               :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_hru_data, hru_data)

  call readHRUForcing(indx_gru, indx_hru, iStep, iRead, iFile, hru_data, &
                      err, message)
  if(err /= 0) then; call f_c_string_ptr(trim(message), message_r); end if
end subroutine readHRUForcing_fortran

subroutine runHRU_fortran(indx_gru, indx_hru, modelTimeStep, handle_hru_data, &
    dt_init_factor, wallTimeTimeStep, err, message_r) bind(C, name="runHRU_fortran")
  USE actor_data_types,only:hru_type
  USE summa_modelRun,only:runPhysics
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  USE globalData,only:model_decisions          ! model decision structure
  USE qTimeDelay_module,only:qOverland         ! module to route water through an "unresolved" river network
  
  USE mDecisions_module,only:&               ! look-up values for LAI decisions
      monthlyTable,& ! LAI/SAI taken directly from a monthly table for different vegetation classes
      specified,&    ! LAI/SAI computed from green vegetation fraction and winterSAI and summerLAI parameters   
      localColumn, & ! separate groundwater representation in each local soil column
      singleBasin, & ! single groundwater store over the entire basin
      bigBucket

  USE var_lookup,only:iLookFLUX              ! look-up values for local column model fluxes
  USE var_lookup,only:iLookBVAR              ! look-up values for basin-average model variables
  USE var_lookup,only:iLookDIAG              ! look-up values for local column model diagnostic variables
  USE var_lookup,only:iLookDECISIONS         ! look-up values for model decisions
  USE var_lookup,only:iLookATTR              ! look-up values for local attributes
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  integer(c_int), intent(in)       :: indx_hru
  integer(c_int), intent(in)       :: modelTimeStep
  type(c_ptr),    intent(in),value :: handle_hru_data
  integer(c_int), intent(in)       :: dt_init_factor
  real(c_double), intent(out)      :: wallTimeTimeStep
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  type(hru_type),pointer           :: hru_data
  character(len=256)               :: message = ""
  character(len=256)               :: cmessage
  real(rkind)                      :: fracHRU                ! fractional area of a given HRU (-)


  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_hru_data, hru_data)

  !****************************************************************************** 
  !****************************** From run_oneGRU *******************************
  !******************************************************************************
  ! ----- basin initialization --------------------------------------------------------------------------------------------
  ! initialize runoff variables
  hru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1)    = 0._dp  ! surface runoff (m s-1)
  hru_data%bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)     = 0._dp 
  hru_data%bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)    = 0._dp  ! outflow from all "outlet" HRUs (those with no downstream HRU)
  hru_data%bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1)      = 0._dp 

  ! initialize baseflow variables
  hru_data%bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)  = 0._dp ! recharge to the aquifer (m s-1)
  hru_data%bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  = 0._dp ! baseflow from the aquifer (m s-1)
  hru_data%bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1) = 0._dp ! transpiration loss from the aquifer (m s-1)

  ! initialize total inflow for each layer in a soil column
  ! if (modelTimeStep == 0 .and. indxHRU == 1)then
  hru_data%fluxStruct%var(iLookFLUX%mLayerColumnInflow)%dat(:) = 0._dp
  ! end if


  call runPhysics(indx_gru, indx_hru, modelTimeStep, hru_data, dt_init_factor, err, message)
  if(err /= 0) then; call f_c_string_ptr(trim(message), message_r); return; end if

  fracHRU = hru_data%attrStruct%var(iLookATTR%HRUarea) / hru_data%bvarStruct%var(iLookBVAR%basin__totalArea)%dat(1)



  ! ----- calculate weighted basin (GRU) fluxes --------------------------------------------------------------------------------------
  
  ! increment basin surface runoff (m s-1)
  hru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) = hru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + hru_data%fluxStruct%var(iLookFLUX%scalarSurfaceRunoff)%dat(1) * fracHRU
  
  !increment basin soil drainage (m s-1)
  hru_data%bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)   = hru_data%bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)  + hru_data%fluxStruct%var(iLookFLUX%scalarSoilDrainage)%dat(1)  * fracHRU
  
  ! increment aquifer variables -- ONLY if aquifer baseflow is computed individually for each HRU and aquifer is run
  ! NOTE: groundwater computed later for singleBasin
  if(model_decisions(iLookDECISIONS%spatial_gw)%iDecision == localColumn .and. model_decisions(iLookDECISIONS%groundwatr)%iDecision == bigBucket) then

    hru_data%bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)  = hru_data%bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)   + hru_data%fluxStruct%var(iLookFLUX%scalarSoilDrainage)%dat(1)     * fracHRU
    hru_data%bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1) = hru_data%bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1)  + hru_data%fluxStruct%var(iLookFLUX%scalarAquiferTranspire)%dat(1) * fracHRU
    hru_data%bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  =  hru_data%bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  &
            +  hru_data%fluxStruct%var(iLookFLUX%scalarAquiferBaseflow)%dat(1) * fracHRU
    end if

  ! perform the routing
  associate(totalArea => hru_data%bvarStruct%var(iLookBVAR%basin__totalArea)%dat(1) )

  ! compute water balance for the basin aquifer
  if(model_decisions(iLookDECISIONS%spatial_gw)%iDecision == singleBasin)then
    message=trim(message)//'multi_driver/bigBucket groundwater code not transferred from old code base yet'
    err=20; call f_c_string_ptr(trim(message), message_r); return
  end if

  ! calculate total runoff depending on whether aquifer is connected
  if(model_decisions(iLookDECISIONS%groundwatr)%iDecision == bigBucket) then
    ! aquifer
    hru_data%bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1) = hru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + hru_data%bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)/totalArea + hru_data%bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)
  else
    ! no aquifer
    hru_data%bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1) = hru_data%bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + hru_data%bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)/totalArea + hru_data%bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)
  endif

  call qOverland(&! input
                  model_decisions(iLookDECISIONS%subRouting)%iDecision,            &  ! intent(in): index for routing method
                  hru_data%bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1),             &  ! intent(in): total runoff to the channel from all active components (m s-1)
                  hru_data%bvarStruct%var(iLookBVAR%routingFractionFuture)%dat,             &  ! intent(in): fraction of runoff in future time steps (m s-1)
                  hru_data%bvarStruct%var(iLookBVAR%routingRunoffFuture)%dat,               &  ! intent(in): runoff in future time steps (m s-1)
                  ! output
                  hru_data%bvarStruct%var(iLookBVAR%averageInstantRunoff)%dat(1),           &  ! intent(out): instantaneous runoff (m s-1)
                  hru_data%bvarStruct%var(iLookBVAR%averageRoutedRunoff)%dat(1),            &  ! intent(out): routed runoff (m s-1)
                  err,message)                                                                  ! intent(out): error control
  if(err/=0)then; err=20; message=trim(message)//trim(cmessage); call f_c_string_ptr(trim(message), message_r); return; endif;
  end associate
  wallTimeTimeStep = hru_data%diagStruct%var(iLookDIAG%wallClockTime)%dat(1)


end subroutine runHRU_fortran

subroutine writeHRUOutput_fortran(indx_gru, indx_hru, timestep, outputstep, &
    handle_hru_data, y, m, d, h, err, message_r) bind(C, name="writeHRUOutput_fortran")
  USE actor_data_types,only:hru_type
  USE HRUwriteoOutput_module,only:writeHRUOutput
  USE var_lookup,only:iLookTIME                 ! named variables for time data structure
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  ! Dummy Variables
  integer(c_int), intent(in)       :: indx_gru
  integer(c_int), intent(in)       :: indx_hru
  integer(c_int), intent(in)       :: timestep
  integer(c_int), intent(in)       :: outputstep
  type(c_ptr),    intent(in),value :: handle_hru_data
  integer(c_int), intent(out)      :: y
  integer(c_int), intent(out)      :: m
  integer(c_int), intent(out)      :: d
  integer(c_int), intent(out)      :: h
  integer(c_int), intent(out)      :: err
  type(c_ptr),    intent(out)      :: message_r
  ! Local Variables
  type(hru_type),pointer           :: hru_data
  character(len=256)               :: message = ""

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_hru_data, hru_data)

  ! updating date variables to be passed back to the actors
  y = hru_data%timeStruct%var(iLookTIME%iyyy)
  m = hru_data%timeStruct%var(iLookTIME%im)
  d = hru_data%timeStruct%var(iLookTIME%id)
  h = hru_data%timeStruct%var(iLookTIME%ih)

  call writeHRUOutput(indx_gru, indx_hru, timestep, outputstep, hru_data, &
                      err, message)
  if(err /= 0) then; call f_c_string_ptr(trim(message), message_r); end if


end subroutine writeHRUOutput_fortran
end module hru_interface