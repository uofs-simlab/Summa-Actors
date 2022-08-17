!!! Lets try and build this for only the lateral flows case.
!!! If lateral flows exits use the code
module gru_actor
implicit none

! public::run_gru
public::getVarSizes
! public::run_gru_for_timestep()

contains

subroutine getVarSizes() bind(C,name="getVarSizes")
    USE globalData,only:bpar_meta,bvar_meta
    implicit none

end subroutine getVarSizes



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