module gru_interface
USE,intrinsic :: iso_c_binding
USE nrtype


implicit none
public :: getNumHRU
public :: initGRU_fortran
public :: setupGRU_fortran
public :: readGRURestart_fortran
public :: setTimeZoneOffsetGRU_fortran
public :: readGRUForcing_fortran
public :: runGRU_fortran
public :: writeGRUOutput_fortran


contains

subroutine getNumHRU(indx_gru, num_hru) bind(C, name="getNumHRU")
  USE globalData,only:gru_struc
  implicit none
  integer(c_int), intent(in)  :: indx_gru
  integer(c_int), intent(out) :: num_hru

  num_hru = gru_struc(indx_gru)%hruCount
end subroutine getNumHRU

subroutine initGRU_fortran(indx_gru, handle_gru_data, err, message_r) &
    bind(C, name="initGRU_fortran")
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
  integer(c_int), intent(out)         :: err
  type(c_ptr),   intent(out)          :: message_r

  ! local variables
  type(gru_type),pointer              :: gru_data
  integer(i4b)                        :: iHRU
  character(len=256)                  :: message = ""
  character(len=256)                  :: cmessage

  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_gru_data, gru_data)

  call allocLocal(bvar_meta,gru_data%bvarStruct,nSnow=0,nSoil=0,err=err,message=cmessage);
  if(err /= 0) then; message=trim(message)//cmessage; call f_c_string_ptr(trim(message), message_r);return;end if 
  call allocLocal(statBvar_meta(:)%var_info,gru_data%bvarStat,nSnow=0,nSoil=0,err=err,message=cmessage);
  if(err /= 0) then; message=trim(message)//cmessage; call f_c_string_ptr(trim(message), message_r);return;end if 


  do iHRU = 1, size(gru_data%hru)
    call initHRU(indx_gru, iHRU, gru_data%hru(iHRU), err, message)
    if(err /= 0) then; call f_c_string_ptr(trim(message), message_r);return; end if
  end do
end subroutine initGru_fortran

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
                  gru_data%bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1),             &  ! intent(in): total runoff to the channel from all active components (m s-1)
                  gru_data%bvarStruct%var(iLookBVAR%routingFractionFuture)%dat,             &  ! intent(in): fraction of runoff in future time steps (m s-1)
                  gru_data%bvarStruct%var(iLookBVAR%routingRunoffFuture)%dat,               &  ! intent(in): runoff in future time steps (m s-1)
                  ! output
                  gru_data%bvarStruct%var(iLookBVAR%averageInstantRunoff)%dat(1),           &  ! intent(out): instantaneous runoff (m s-1)
                  gru_data%bvarStruct%var(iLookBVAR%averageRoutedRunoff)%dat(1),            &  ! intent(out): routed runoff (m s-1)
                  err,message)                                                                  ! intent(out): error control
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

end module gru_interface
