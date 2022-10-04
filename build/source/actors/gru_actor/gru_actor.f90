!!! Lets try and build this for only the lateral flows case.
!!! If lateral flows exits use the code
module gru_actor
USE,intrinsic :: iso_c_binding
USE nrtype

implicit none

! public::run_gru
public::getVarSizes
public::fillvarTypeLists
public::getNumHRU

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

subroutine initVarType(var_type_lookup) bind(C, name="initVarType")
    USE var_lookup,only:iLookVarType
    USE var_lookup,only:iLook_VarType

    implicit none
    type(iLook_VarType) :: var_type_lookup

    ! Get the indexes to match C++ offset starting at 0
    var_type_lookup%scalarv = iLookVarType%scalarv - 1
    var_type_lookup%wLength = iLookVarType%wLength - 1
    var_type_lookup%midSnow = iLookVarType%midSnow - 1
    var_type_lookup%midSoil = iLookVarType%midSoil - 1
    var_type_lookup%midToto = iLookVarType%midToto - 1
    var_type_lookup%ifcSnow = iLookVarType%ifcSnow - 1
    var_type_lookup%ifcSoil = iLookVarType%ifcSoil - 1
    var_type_lookup%ifcToto = iLookVarType%ifcToto - 1
    var_type_lookup%parSoil = iLookVarType%parSoil - 1
    var_type_lookup%routing = iLookVarType%routing - 1
    var_type_lookup%outstat = iLookVarType%outstat - 1
    var_type_lookup%unknown = iLookVarType%unknown - 1

    ! check the values
    ! print*, "************FORTRAN************"
    ! print*, "iLookVarType%scalarv", iLookVarType%scalarv
    ! print*, "iLookVarType%wLength", iLookVarType%wLength
    ! print*, "iLookVarType%midSnow", iLookVarType%midSnow
    ! print*, "iLookVarType%midSoil", iLookVarType%midSoil
    ! print*, "iLookVarType%midToto", iLookVarType%midToto
    ! print*, "iLookVarType%ifcSnow", iLookVarType%ifcSnow
    ! print*, "iLookVarType%ifcSoil", iLookVarType%ifcSoil
    ! print*, "iLookVarType%ifcToto", iLookVarType%ifcToto
    ! print*, "iLookVarType%parSoil", iLookVarType%parSoil
    ! print*, "iLookVarType%routing", iLookVarType%routing
    ! print*, "iLookVarType%outstat", iLookVarType%outstat
    ! print*, "iLookVarType%unknown", iLookVarType%unknown
    ! print*, "************FORTRAN************"


end subroutine
subroutine fillVarTypeLists(num_bpar_vars, &
                            num_bvar_vars, &
                            bpar_struct_var_type_list, &
                            bvar_struct_var_type_list, &
                            err) bind(C, name="fillVarTypeLists")
    
    USE globalData,only:type_meta,bpar_meta,bvar_meta
    USE var_lookup,only:iLookBVAR,iLookBPAR,iLookVarType
    implicit none
    integer(c_int), intent(in)                              :: num_bpar_vars
    integer(c_int), intent(in)                              :: num_bvar_vars
    integer(c_int), intent(out), dimension(num_bpar_vars)   :: bpar_struct_var_type_list
    integer(c_int), intent(out), dimension(num_bvar_vars)   :: bvar_struct_var_type_list
    integer(c_int), intent(out)                             :: err  
    integer(i4b)                                            :: iVar


    ! Get the types of the variables for bparStruct
    ! Index in bpar_struct_var_type_list will match bpar_Struct
    do iVar=1, num_bpar_vars
        select case(bpar_meta(iVar)%vartype)
            case(iLookVarType%scalarv); bpar_struct_var_type_list(iVar) = iLookVarType%scalarv - 1
            case(iLookVarType%wLength); bpar_struct_var_type_list(iVar) = iLookVarType%wLength - 1
            case(iLookVarType%midSnow); bpar_struct_var_type_list(iVar) = iLookVarType%midSnow - 1
            case(iLookVarType%midSoil); bpar_struct_var_type_list(iVar) = iLookVarType%midSoil - 1
            case(iLookVarType%midToto); bpar_struct_var_type_list(iVar) = iLookVarType%midToto - 1
            case(iLookVarType%ifcSnow); bpar_struct_var_type_list(iVar) = iLookVarType%ifcSnow - 1
            case(iLookVarType%ifcSoil); bpar_struct_var_type_list(iVar) = iLookVarType%ifcSoil - 1
            case(iLookVarType%ifcToto); bpar_struct_var_type_list(iVar) = iLookVarType%ifcToto - 1
            case(iLookVarType%parSoil); bpar_struct_var_type_list(iVar) = iLookVarType%parSoil - 1
            case(iLookVarType%routing); bpar_struct_var_type_list(iVar) = iLookVarType%routing - 1
            case(iLookVarType%outstat); bpar_struct_var_type_list(iVar) = iLookVarType%outstat - 1
            case(iLookVarType%unknown); bpar_struct_var_type_list(iVar) = iLookVarType%unknown - 1
            case default
                err = 40;
                return
        end select
    end do

    do iVar=1, num_bvar_vars
        select case(bvar_meta(iVar)%vartype)
            case(iLookVarType%scalarv); bvar_struct_var_type_list(iVar) = iLookVarType%scalarv - 1
            case(iLookVarType%wLength); bvar_struct_var_type_list(iVar) = iLookVarType%wLength - 1
            case(iLookVarType%midSnow); bvar_struct_var_type_list(iVar) = iLookVarType%midSnow - 1
            case(iLookVarType%midSoil); bvar_struct_var_type_list(iVar) = iLookVarType%midSoil - 1
            case(iLookVarType%midToto); bvar_struct_var_type_list(iVar) = iLookVarType%midToto - 1
            case(iLookVarType%ifcSnow); bvar_struct_var_type_list(iVar) = iLookVarType%ifcSnow - 1
            case(iLookVarType%ifcSoil); bvar_struct_var_type_list(iVar) = iLookVarType%ifcSoil - 1
            case(iLookVarType%ifcToto); bvar_struct_var_type_list(iVar) = iLookVarType%ifcToto - 1
            case(iLookVarType%parSoil); bvar_struct_var_type_list(iVar) = iLookVarType%parSoil - 1
            case(iLookVarType%routing); bvar_struct_var_type_list(iVar) = iLookVarType%routing - 1
            case(iLookVarType%outstat); bvar_struct_var_type_list(iVar) = iLookVarType%outstat - 1
            case(iLookVarType%unknown); bvar_struct_var_type_list(iVar) = iLookVarType%unknown - 1
            case default
                err = 40;
                return
        end select
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

integer function getNumHRU(indx_gru) bind(C, name="getNumHRU")
    USE globalData,only:gru_struc
    implicit none
    integer(c_int), intent(in)        :: indx_gru
    integer(c_int)                    :: num_hru

    num_hru = gru_struc(indx_gru)%hruCount

    getNumHRU = num_hru

end function


end module gru_actor

! the way the lateral flow interface should work seems to be at the level of the hru. 
! The HRU would have an interface where it has a donwslope compontent. It would then ask
! for this downslope component. This all does have to be setup before hand so the gru needs to 
! set these up because the hrus all need to comptue 
! a grus timestep depends on all the hrus

! So the Gru needs to be controlling the hrus. It needs to know which ones have lateral flows and which ones do not. 
! This is so the other hrus know they do not need to contact another actor. Otherwise they will have to.