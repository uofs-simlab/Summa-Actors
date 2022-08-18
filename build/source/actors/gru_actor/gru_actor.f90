!!! Lets try and build this for only the lateral flows case.
!!! If lateral flows exits use the code
module gru_actor
USE,intrinsic :: iso_c_binding
USE nrtype

implicit none

! public::run_gru
public::getVarSizes
public::fillvarTypeLists

contains

subroutine getVarSizes(num_var_types, &
                       num_bpar_vars, &
                       num_bvar_vars) bind(C,name="getVarSizes")
    USE var_lookup,only:maxvarBpar, maxvarBvar, maxvarVarType
    implicit none
    
    integer(c_int), intent(out)                   :: num_var_types
    integer(c_int), intent(out)                   :: num_bpar_vars
    integer(c_int), intent(out)                   :: num_bvar_vars

    num_var_types = maxvarVarType
    num_bpar_vars = maxvarBpar
    num_bvar_vars = maxvarBvar

end subroutine getVarSizes

subroutine initVarType(num_var_types, &
                       i_look_var_type_list) bind(C, name="initVarType")
    USE var_lookup,only:iLookVarType
    implicit none
    integer(c_int), intent(in)                              :: num_var_types
    integer(c_int), intent(out), dimension(num_var_types)   :: i_look_var_type_list   

    i_look_var_type_list(1) = iLookVarType%scalarv 
    i_look_var_type_list(2) = iLookVarType%wLength
    i_look_var_type_list(3) = iLookVarType%midSnow
    i_look_var_type_list(4) = iLookVarType%midSoil
    i_look_var_type_list(5) = iLookVarType%midToto
    i_look_var_type_list(6) = iLookVarType%ifcSnow
    i_look_var_type_list(7) = iLookVarType%ifcSoil
    i_look_var_type_list(8) = iLookVarType%ifcToto
    i_look_var_type_list(9) = iLookVarType%parSoil
    i_look_var_type_list(10) = iLookVarType%routing
    i_look_var_type_list(11) = iLookVarType%outstat
    i_look_var_type_list(12) = iLookVarType%unknown

end subroutine
subroutine fillVarTypeLists(num_var_types, &
                            num_bpar_vars, &
                            num_bvar_vars, &
                            i_look_var_type_list, &
                            bpar_struct_var_type_list, &
                            bvar_struct_var_type_list) bind(C, name="fillVarTypeLists")
    
    USE globalData,only:type_meta,bpar_meta,bvar_meta
    USE var_lookup,only:iLookBVAR,iLookBPAR,iLookVarType
    implicit none
    integer(c_int), intent(in)                              :: num_var_types
    integer(c_int), intent(in)                              :: num_bpar_vars
    integer(c_int), intent(in)                              :: num_bvar_vars
    integer(c_int), intent(out), dimension(num_var_types)   :: i_look_var_type_list   
    integer(c_int), intent(out), dimension(num_bpar_vars)   :: bpar_struct_var_type_list
    integer(c_int), intent(out), dimension(num_bvar_vars)   :: bvar_struct_var_type_list

    integer(i4b)                                            :: i

    i_look_var_type_list(1) = iLookVarType%scalarv 
    i_look_var_type_list(2) = iLookVarType%wLength
    i_look_var_type_list(3) = iLookVarType%midSnow
    i_look_var_type_list(4) = iLookVarType%midSoil
    i_look_var_type_list(5) = iLookVarType%midToto
    i_look_var_type_list(6) = iLookVarType%ifcSnow
    i_look_var_type_list(7) = iLookVarType%ifcSoil
    i_look_var_type_list(8) = iLookVarType%ifcToto
    i_look_var_type_list(9) = iLookVarType%parSoil
    i_look_var_type_list(10) = iLookVarType%routing
    i_look_var_type_list(11) = iLookVarType%outstat
    i_look_var_type_list(12) = iLookVarType%unknown

    do i = 1, num_var_types
       print*, "iLookVarType = ", i_look_var_type_list(i)
    end do

    do i = 1, num_bpar_vars
        bpar_struct_var_type_list(i) = bpar_meta(i)%varType
    end do

    do i = 1, num_bvar_vars
        bvar_struct_var_type_list(i) = bvar_meta(i)%varType
    end do






end subroutine fillVarTypeLists



! subroutine run_gru_for_timestep()
!     implicit none

!     ! initialize runoff variables
!     bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1)    = 0._dp  ! surface runoff (m s-1)
!     bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)     = 0._dp 
!     bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)    = 0._dp  ! outflow from all "outlet" HRUs (those with no downstream HRU)
!     bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1)      = 0._dp 
  
!     ! initialize baseflow variables
!     bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)  = 0._dp ! recharge to the aquifer (m s-1)
!     bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  = 0._dp ! baseflow from the aquifer (m s-1)
!     bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1) = 0._dp ! transpiration loss from the aquifer (m s-1)

!     ! ----- calculate weighted basin (GRU) fluxes --------------------------------------------------------------------------------------
 
!     ! increment basin surface runoff (m s-1)
!     bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) = bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + fluxStruct%var(iLookFLUX%scalarSurfaceRunoff)%dat(1) * fracHRU
    
!     !increment basin soil drainage (m s-1)
!     bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)   = bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)  + fluxStruct%var(iLookFLUX%scalarSoilDrainage)%dat(1)  * fracHRU
    
!     ! increment aquifer variables -- ONLY if aquifer baseflow is computed individually for each HRU and aquifer is run
!     ! NOTE: groundwater computed later for singleBasin
!     if(model_decisions(iLookDECISIONS%spatial_gw)%iDecision == localColumn .and. model_decisions(iLookDECISIONS%groundwatr)%iDecision == bigBucket) then
   
!       bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)  = bvarStruct%var(iLookBVAR%basin__AquiferRecharge)%dat(1)   + fluxStruct%var(iLookFLUX%scalarSoilDrainage)%dat(1)     * fracHRU
!       bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1) = bvarStruct%var(iLookBVAR%basin__AquiferTranspire)%dat(1)  + fluxStruct%var(iLookFLUX%scalarAquiferTranspire)%dat(1) * fracHRU
!       bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  =  bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)  &
!               +  fluxStruct%var(iLookFLUX%scalarAquiferBaseflow)%dat(1) * fracHRU
!      end if
   
!      ! perform the routing
!      associate(totalArea => bvarStruct%var(iLookBVAR%basin__totalArea)%dat(1) )
   
!     ! compute water balance for the basin aquifer
!     if(model_decisions(iLookDECISIONS%spatial_gw)%iDecision == singleBasin)then
!      message=trim(message)//'multi_driver/bigBucket groundwater code not transferred from old code base yet'
!      err=20; return
!     end if
   
!     ! calculate total runoff depending on whether aquifer is connected
!     if(model_decisions(iLookDECISIONS%groundwatr)%iDecision == bigBucket) then
!      ! aquifer
!      bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1) = bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)/totalArea + bvarStruct%var(iLookBVAR%basin__AquiferBaseflow)%dat(1)
!     else
!      ! no aquifer
!      bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1) = bvarStruct%var(iLookBVAR%basin__SurfaceRunoff)%dat(1) + bvarStruct%var(iLookBVAR%basin__ColumnOutflow)%dat(1)/totalArea + bvarStruct%var(iLookBVAR%basin__SoilDrainage)%dat(1)
!     endif
   
!     call qOverland(&
!                    ! input
!                    model_decisions(iLookDECISIONS%subRouting)%iDecision,            &  ! intent(in): index for routing method
!                    bvarStruct%var(iLookBVAR%basin__TotalRunoff)%dat(1),            &  ! intent(in): total runoff to the channel from all active components (m s-1)
!                    bvarStruct%var(iLookBVAR%routingFractionFuture)%dat,             &  ! intent(in): fraction of runoff in future time steps (m s-1)
!                    bvarStruct%var(iLookBVAR%routingRunoffFuture)%dat,               &  ! intent(in): runoff in future time steps (m s-1)
!                    ! output
!                    bvarStruct%var(iLookBVAR%averageInstantRunoff)%dat(1),           &  ! intent(out): instantaneous runoff (m s-1)
!                    bvarStruct%var(iLookBVAR%averageRoutedRunoff)%dat(1),            &  ! intent(out): routed runoff (m s-1)
!                    err,message)                                                                  ! intent(out): error control
!     if(err/=0)then; err=20; message=trim(message)//trim(cmessage); return; endif
!     end associate
    


! end subroutine




end module gru_actor

! the way the lateral flow interface should work seems to be at the level of the hru. 
! The HRU would have an interface where it has a donwslope compontent. It would then ask
! for this downslope component. This all does have to be setup before hand so the gru needs to 
! set these up because the hrus all need to comptue 
! a grus timestep depends on all the hrus

! So the Gru needs to be controlling the hrus. It needs to know which ones have lateral flows and which ones do not. 
! This is so the other hrus know they do not need to contact another actor. Otherwise they will have to.