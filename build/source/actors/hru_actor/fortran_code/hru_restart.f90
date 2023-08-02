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

module summa_restart
! read restart data and reset the model state
USE,intrinsic :: iso_c_binding


USE data_types,only:&
                    ! no spatial dimension
                    var_i,               & ! x%var(:)            (i4b)
                    var_i8,              & ! x%var(:)            (i8b)
                    var_d,               & ! x%var(:)            (dp)
                    var_ilength,         & ! x%var(:)%dat        (i4b)
                    var_dlength            ! x%var(:)%dat        (dp)

! access missing values
USE globalData,only:integerMissing   ! missing integer
USE globalData,only:realMissing      ! missing double precision number

! named variables
USE var_lookup,only:iLookPROG                               ! look-up values for local column model prognostic (state) variables
USE var_lookup,only:iLookDIAG                               ! look-up values for local column model diagnostic variables
USE var_lookup,only:iLookFLUX                               ! look-up values for local column model fluxes
USE var_lookup,only:iLookBVAR                               ! look-up values for basin-average model variables
USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions

! safety: set private unless specified otherwise
implicit none
private
public::summa_readRestart
contains

! read restart data and reset the model state
subroutine summa_readRestart(&
                indxGRU,    & ! index of GRU in gru_struc
                indxHRU,    & ! index of HRU in gru_struc
                ! primary data structures (variable length vectors)
                handle_indxStruct, & ! c_ptr to -- model indices
                handle_mparStruct, & ! c_ptr to -- model parameters
                handle_progStruct, & ! c_ptr to -- model prognostic (state) variables
                handle_diagStruct, & ! c_ptr to -- model diagnostic variables
                handle_fluxStruct, & ! c_ptr to -- model fluxes
                ! basin-average structures
                handle_bvarStruct, & ! x%var(:)%dat        -- basin-average variables
                dt_init,    & ! used to initialize the length of the sub-step for each HRU
                err) bind(C,name='summa_readRestart')
  ! ---------------------------------------------------------------------------------------
  ! * desired modules
  ! ---------------------------------------------------------------------------------------
  ! data types
  USE nrtype                                                  ! variable types, etc.
  ! functions and subroutines
  USE time_utils_module,only:elapsedSec                       ! calculate the elapsed time
  USE read_icond_gru_hru_module,only:read_icond               ! module to read initial conditions
  USE check_icond4chm_module,only:check_icond4chm             ! module to check initial conditions
  USE var_derive_module,only:calcHeight                       ! module to calculate height at layer interfaces and layer mid-point
  USE var_derive_module,only:v_shortcut                       ! module to calculate "short-cut" variables
  USE var_derive_module,only:rootDensty                       ! module to calculate the vertical distribution of roots
  USE var_derive_module,only:satHydCond                       ! module to calculate the saturated hydraulic conductivity in each soil layer
  ! global data structures
  USE globalData,only:model_decisions                         ! model decision structure
  ! timing variables
  USE globalData,only:startRestart,endRestart                 ! date/time for the start and end of reading model restart files
  USE globalData,only:elapsedRestart                          ! elapsed time to read model restart files
  ! model decisions
  USE mDecisions_module,only:&                                ! look-up values for the choice of method for the spatial representation of groundwater
    localColumn, & ! separate groundwater representation in each local soil column
    singleBasin    ! single groundwater store over the entire basin
  implicit none
  ! ---------------------------------------------------------------------------------------
  ! Dummy variables
  ! ---------------------------------------------------------------------------------------
  integer(c_int),intent(in)                  :: indxGRU            !  index of GRU in gru_struc
  integer(c_int),intent(in)                  :: indxHRU            !  index of HRU in gru_struc
  ! primary data structures (variable length vectors)
  type(c_ptr), intent(in), value          :: handle_indxStruct !  model indices
  type(c_ptr), intent(in), value          :: handle_mparStruct !  model parameters
  type(c_ptr), intent(in), value          :: handle_progStruct !  model prognostic (state) variables
  type(c_ptr), intent(in), value          :: handle_diagStruct !  model diagnostic variables
  type(c_ptr), intent(in), value          :: handle_fluxStruct !  model fluxes
  ! define the basin-average structures
  type(c_ptr), intent(in), value          :: handle_bvarStruct !  basin-average variables
  real(c_double), intent(inout)           :: dt_init
  integer(c_int), intent(inout)           :: err
  ! ---------------------------------------------------------------------------------------
  ! Fortran Pointers
  ! ---------------------------------------------------------------------------------------
  ! primary data structures (variable length vectors)
  type(var_ilength),pointer              :: indxStruct                 !  model indices
  type(var_dlength),pointer              :: mparStruct                 !  model parameters
  type(var_dlength),pointer              :: progStruct                 !  model prognostic (state) variables
  type(var_dlength),pointer              :: diagStruct                 !  model diagnostic variables
  type(var_dlength),pointer              :: fluxStruct                 !  model fluxes
  ! define the basin-average structures
  type(var_dlength),pointer              :: bvarStruct                 !  basin-average variables
  ! ---------------------------------------------------------------------------------------
  ! local variables
  ! ---------------------------------------------------------------------------------------
  character(len=256)                       :: message            ! error message
  character(LEN=256)                       :: cmessage           ! error message of downwind routine
  character(LEN=256)                       :: restartFile        ! restart file name
  integer(i4b)                             :: nGRU
  ! ---------------------------------------------------------------------------------------

  ! #############################################################################
  call c_f_pointer(handle_indxStruct, indxStruct)
  call c_f_pointer(handle_mparStruct, mparStruct)
  call c_f_pointer(handle_progStruct, progStruct)
  call c_f_pointer(handle_diagStruct, diagStruct)
  call c_f_pointer(handle_fluxStruct, fluxStruct)
  call c_f_pointer(handle_bvarStruct, bvarStruct)
  ! #############################################################################

  ! initialize error control
  err=0; message='hru_actor_readRestart/'
  nGRU = 1

  ! *****************************************************************************
  ! *** read/check initial conditions
  ! *****************************************************************************
  ! read initial conditions
  call read_icond(&
                  indxGRU,                       & ! intent(in):    index of GRU in gru_struc
                  indxHRU,                       & ! intent(in):    index of HRU in gru_struc
                  mparStruct,                    & ! intent(in):    model parameters
                  progStruct,                    & ! intent(inout): model prognostic variables
                  bvarStruct,                    & ! intent(inout): model basin (GRU) variables
                  indxStruct,                    & ! intent(inout): model indices
                  err,cmessage)                    ! intent(out):   error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! check initial conditions
  call check_icond4chm(&
                    indxGRU,                        & ! intent(in):   index of GRU in gru_struc
                    indxHRU,                        & ! intent(in):   index of HRU in gru_struc           
                    progStruct,                     & ! intent(in):   model prognostic (state) variables
                    mparStruct,                     & ! intent(in):   model parameters
                    indxStruct,                     & ! intent(in):   layer indexes
                    err,cmessage)                   ! intent(out):   error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif


  ! *****************************************************************************
  ! *** compute ancillary variables
  ! *****************************************************************************

  ! re-calculate height of each layer
  call calcHeight(indxStruct,   & ! layer type
                  progStruct,   & ! model prognostic (state) variables for a local HRU
                  err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! calculate vertical distribution of root density
  call rootDensty(mparStruct,   & ! vector of model parameters
                  indxStruct,   & ! data structure of model indices
                  progStruct,   & ! data structure of model prognostic (state) variables
                  diagStruct,   & ! data structure of model diagnostic variables
                  err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! calculate saturated hydraulic conductivity in each soil layer
  call satHydCond(mparStruct,   & ! vector of model parameters
                  indxStruct,   & ! data structure of model indices
                  progStruct,   & ! data structure of model prognostic (state) variables
                  fluxStruct,   & ! data structure of model fluxes
                  err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! calculate "short-cut" variables such as volumetric heat capacity
  call v_shortcut(mparStruct,   & ! vector of model parameters
                  diagStruct,   & ! data structure of model diagnostic variables
                  err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! initialize canopy drip
  ! NOTE: canopy drip from the previous time step is used to compute throughfall for the current time step
  fluxStruct%var(iLookFLUX%scalarCanopyLiqDrainage)%dat(1) = 0._dp  ! not used

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

  ! select groundwater option
  select case(model_decisions(iLookDECISIONS%spatial_gw)%iDecision)

   ! the basin-average aquifer storage is not used if the groundwater is included in the local column
   case(localColumn)
    bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = 0._dp ! set to zero to be clear that there is no basin-average aquifer storage in this configuration

   ! the local column aquifer storage is not used if the groundwater is basin-average
   ! (i.e., where multiple HRUs drain to a basin-average aquifer)
   case(singleBasin)
    bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = 1._dp
    progStruct%var(iLookPROG%scalarAquiferStorage)%dat(1) = 0._dp  ! set to zero to be clear that there is no local aquifer storage in this configuration

   ! error check
   case default
    message=trim(message)//'unable to identify decision for regional representation of groundwater'
    return

  end select  ! groundwater option

  ! *****************************************************************************
  ! *** initialize time step
  ! *****************************************************************************

  ! initialize time step length
  dt_init = progStruct%var(iLookPROG%dt_init)%dat(1) ! seconds
   

  ! *****************************************************************************
  ! *** finalize
  ! *****************************************************************************

end subroutine summa_readRestart

end module summa_restart




