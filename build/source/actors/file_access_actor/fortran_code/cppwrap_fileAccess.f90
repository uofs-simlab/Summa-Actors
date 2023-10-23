module cppwrap_fileAccess


  !======= Inclusions ===========
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE data_types
  USE globalData
  USE globalData,only:integerMissing      ! missing integer value
  USE globalData,only:realMissing         ! missing double precision value

  USE var_lookup,only:maxvarFreq                ! maximum number of output files


  implicit none
  public::fileAccessActor_init_fortran
  public::FileAccessActor_DeallocateStructures
  
  contains

! Call the fortran routines that read data in and are associtated with the forcing structure
subroutine fileAccessActor_init_fortran(& ! Variables for forcing
                                        handle_forcFileInfo,&
                                        num_forcing_files,&
                                        num_timesteps,&
                                        num_timesteps_output_buffer,&
                                        ! Variables for output
                                        handle_output_ncid,&
                                        start_gru,&  
                                        num_gru,&
                                        num_hru,&
                                        actor_stats,&
                                        err) bind(C, name="fileAccessActor_init_fortran")
  USE ffile_info_actors_module,only:ffile_info
  USE mDecisions_module,only:mDecisions                       ! module to read model decisions
  USE read_pinit_module,only:read_pinit                       ! module to read initial model parameter values
  USE SummaActors_setup,only:SOIL_VEG_GEN_PARM
  USE module_sf_noahmplsm,only:read_mp_veg_parameters         ! module to read NOAH vegetation tables
  USE def_output_actors_module,only:def_output                ! module to define output variables
  USE output_structure_module,only:initOutputStructure        ! module to initialize output structure
  USE output_structure_module,only:initOutputTimeStep         ! module to initialize output timestep structure (tracks GRUs timestep for output)
  USE read_attrb_module,only:read_attrb                       ! module to read local attributes
  USE read_param_module,only:read_param                       ! module to read model parameter sets
  USE pOverwrite_module,only:pOverwrite                       ! module to overwrite default parameter values with info from the Noah tables
  USE var_derive_module,only:fracFuture                       ! module to calculate the fraction of runoff in future time steps (time delay histogram)
  USE paramCheck_module,only:paramCheck                       ! module to check consistency of model parameters
  USE read_icond_module,only:read_icond                       ! module to read initial conditions
  USE check_icond_module,only:check_icond                     ! module to check initial conditions

  USE mDecisions_module,only:&
                              sameRulesAllLayers, & ! SNTHERM option: same combination/sub-dividion rules applied to all layers
                              rulesDependLayerIndex ! CLM option: combination/sub-dividion rules depend on layer index
  USE globalData,only:localParFallback                        ! local column default parameters
  USE globalData,only:basinParFallback                        ! basin-average default parameters
  USE summaFileManager,only:LOCALPARAM_INFO,BASINPARAM_INFO   ! files defining the default values and constraints for model parameters
  USE globalData,only:mpar_meta,bpar_meta                     ! parameter metadata structures
  USE summaFileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaFileManager,only:LOCAL_ATTRIBUTES                  ! name of model initial attributes file
  USE summaFileManager,only:GENPARM,VEGPARM,SOILPARM,MPTABLE  ! files defining the noah tables
  USE summaFileManager,only:MODEL_INITCOND                    ! name of model initial conditions file
  USE summaFileManager,only:STATE_PATH                        ! optional path to state/init. condition files (defaults to SETTINGS_PATH)
  USE globalData,only:model_decisions                         ! model decision structure
  USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions
  USE var_lookup,only:iLookTYPE                               ! look-up values for model types
  USE var_lookup,only:iLookID                                 ! look-up values for model IDs
  USE var_lookup,only:iLookPARAM
  USE var_lookup,only:iLookATTR                               ! look-up values for model attributes
  USE var_lookup,only:iLookBVAR                               ! look-up values for basin-average variables
  USE output_structure_module,only:outputStructure            ! output structure
  USE globalData,only:failedHRUs                              ! Flag for file access actor to know which GRUs have failed
  
  USE globalData,only:iRunModeFull,iRunModeGRU,iRunModeHRU
  USE globalData,only:iRunMode                                ! define the current running mode
  USE globalData,only:checkHRU                                ! index of the HRU for a single HRU run
  
  ! look-up values for the choice of heat capacity computation
#ifdef V4_ACTIVE
  USE mDecisions_module,only:enthalpyFD                       ! heat capacity using enthalpy
  USE t2enthalpy_module,only:T2E_lookup                       ! module to calculate a look-up table for the temperature-enthalpy conversion
#endif
  USE mDecisions_module,only:&
                        monthlyTable,& ! LAI/SAI taken directly from a monthly table for different vegetation classes
                        specified      ! LAI/SAI computed from green vegetation fraction and winterSAI and summerLAI parameters

  USE ConvE2Temp_module,only:E2T_lookup                       ! module to calculate a look-up table for the temperature-enthalpy conversion


  USE NOAHMP_VEG_PARAMETERS,only:SAIM,LAIM                    ! 2-d tables for stem area index and leaf area index (vegType,month)
  USE NOAHMP_VEG_PARAMETERS,only:HVT,HVB                      ! height at the top and bottom of vegetation (vegType)

  USE globalData,only:numtim                 ! number of time steps in the simulation

  implicit none

  type(c_ptr), intent(in), value         :: handle_forcFileInfo
  integer(c_int),intent(out)             :: num_forcing_files
  integer(c_int),intent(out)             :: num_timesteps
  integer(c_int),intent(in)              :: num_timesteps_output_buffer
  type(c_ptr),intent(in), value          :: handle_output_ncid
  integer(c_int),intent(out)             :: start_gru
  integer(c_int),intent(out)             :: num_gru
  integer(c_int),intent(out)             :: num_hru
  type(netcdf_gru_actor_info),intent(out):: actor_stats        ! netcdf actor information 
  integer(c_int),intent(out)             :: err


  ! local Variables
  type(file_info_array),pointer          :: forcFileInfo
  type(var_i),pointer                    :: output_ncid        ! id of output file
  integer(i4b)                           :: iGRU               ! counter for GRUs
  integer(i4b)                           :: iHRU               ! counter for HRUs
  integer(i4b)                           :: jHRU,kHRU          ! HRU indices
  integer(i4b)                           :: ivar               ! counter for variables
  character(len=256)                     :: attrFile           ! attributes file name
  character(LEN=256)                    :: restartFile        ! restart file name
  integer(i4b)                           :: indxGRU=1
  character(len=256)                     :: message            ! error message for downwind routine


  err=0; message="fileAccessActor_init_fortran/"

  call c_f_pointer(handle_forcFileInfo, forcFileInfo)
  call c_f_pointer(handle_output_ncid, output_ncid)

  ! Get the initial forcing file information
  call ffile_info(indxGRU, forcFileInfo, num_forcing_files, err, message)
  if(err/=0)then; print*, trim(message); return; endif

  ! Get and save the model decisions as integers
  call mDecisions(err,message)
  if(err/=0)then; print*,trim(message); return; endif
  num_timesteps = numtim

    ! get the maximum number of snow layers
  select case(model_decisions(iLookDECISIONS%snowLayers)%iDecision)
    case(sameRulesAllLayers);    err=100; message=trim(message)//'sameRulesAllLayers not implemented';print*,message;return
    case(rulesDependLayerIndex); maxSnowLayers = 5
    case default; err=20; message=trim(message)//'unable to identify option to combine/sub-divide snow layers';print*,message;return
  end select ! (option to combine/sub-divide snow layers)


  maxLayers = gru_struc(1)%hruInfo(1)%nSoil + maxSnowLayers

  ! *****************************************************************************
  ! *** read default model parameters
  ! *****************************************************************************
  ! read default values and constraints for model parameters (local column)
  call read_pinit(LOCALPARAM_INFO,.TRUE., mpar_meta,localParFallback,err,message)
  if(err/=0)then; print*,trim(message); return; endif

  ! read default values and constraints for model parameters (basin-average)
  call read_pinit(BASINPARAM_INFO,.FALSE.,bpar_meta,basinParFallback,err,message)
  if(err/=0)then; print*,trim(message); return; endif
  
  
  ! *****************************************************************************
  ! *** read Noah vegetation and soil tables
  ! *****************************************************************************

  greenVegFrac_monthly = (/0.01_dp, 0.02_dp, 0.03_dp, 0.07_dp, 0.50_dp, 0.90_dp, 0.95_dp, 0.96_dp, 0.65_dp, 0.24_dp, 0.11_dp, 0.02_dp/)


  ! read Noah soil and vegetation tables
  call soil_veg_gen_parm(trim(SETTINGS_PATH)//trim(VEGPARM),      & ! filename for vegetation table
                         trim(SETTINGS_PATH)//trim(SOILPARM),                        & ! filename for soils table
                         trim(SETTINGS_PATH)//trim(GENPARM),                         & ! filename for general table
                         trim(model_decisions(iLookDECISIONS%vegeParTbl)%cDecision), & ! classification system used for vegetation
                         trim(model_decisions(iLookDECISIONS%soilCatTbl)%cDecision))   ! classification system used for soils
  if(err/=0)then; print*,trim(message); return; endif

  ! read Noah-MP vegetation tables
  call read_mp_veg_parameters(trim(SETTINGS_PATH)//trim(MPTABLE),                       & ! filename for Noah-MP table
                              trim(model_decisions(iLookDECISIONS%vegeParTbl)%cDecision)) ! classification system used for vegetation
  if(err/=0)then; print*,trim(message); return; endif

    ! define urban vegetation category
  select case(trim(model_decisions(iLookDECISIONS%vegeParTbl)%cDecision))
    case('USGS');                     urbanVegCategory =    1
    case('MODIFIED_IGBP_MODIS_NOAH'); urbanVegCategory =   13
    case('plumberCABLE');             urbanVegCategory = -999
    case('plumberCHTESSEL');          urbanVegCategory = -999
    case('plumberSUMMA');             urbanVegCategory = -999
    case default; message=trim(message)//'unable to identify vegetation category';print*,message;return
  end select

  ! *****************************************************************************
  ! *** Initalize failed HRU tracker
  ! *****************************************************************************
  if (allocated(failedHRUs))then; deallocate(failedHRUs); endif;
  allocate(failedHRUs(num_gru), stat=err)
  if(err/=0)then; print*,trim(message); return; endif
  failedHRUs(:) = .false.

  ! *****************************************************************************
  ! *** Define Output Files
  ! *****************************************************************************
  call def_output(output_ncid,start_gru,num_gru,num_hru,actor_stats,err,message)
  if(err/=0)then; print*,trim(message); return; endif

  ! *****************************************************************************
  ! *** Initialize output structure
  ! *****************************************************************************
  call initOutputStructure(forcFileInfo, num_timesteps_output_buffer, num_gru, err)
  if(err/=0)then; print*,trim(message); return; endif

  ! *****************************************************************************
  ! *** Initialize output time step
  ! *****************************************************************************
  call initOutputTimeStep(num_gru, err)
  if(err/=0)then; print*,trim(message); return; endif


  ! *****************************************************************************
  ! *** Read Attributes
  ! *****************************************************************************

  attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)
  call read_attrb(trim(attrFile),num_gru,outputStructure(1)%attrStruct,&
                  outputStructure(1)%typeStruct,outputStructure(1)%idStruct,err,message)
  if(err/=0)then; print*,trim(message); return; endif


  ! set default model parameters
  do iGRU=1, num_gru
    do iHRU=1, gru_struc(iGRU)%hruCount
      ! set parmameters to their default value
      outputStructure(1)%dparStruct%gru(iGRU)%hru(iHRU)%var(:) = localParFallback(:)%default_val         ! x%hru(:)%var(:)

      ! overwrite default model parameters with information from the Noah-MP tables
      call pOverwrite(outputStructure(1)%typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex),  &  ! vegetation category
                      outputStructure(1)%typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%soilTypeIndex), &  ! soil category
                      outputStructure(1)%dparStruct%gru(iGRU)%hru(iHRU)%var,                          &  ! default model parameters
                      err,message)                                                   ! error control
      if(err/=0)then; print*, trim(message); return; endif


   ! copy over to the parameter structure
   ! NOTE: constant for the dat(:) dimension (normally depth)
      do ivar=1,size(localParFallback)
        outputStructure(1)%mparStruct%gru(iGRU)%hru(iHRU)%var(ivar)%dat(:) = outputStructure(1)%dparStruct%gru(iGRU)%hru(iHRU)%var(ivar)
      end do  ! looping through variables
    
    end do  ! looping through HRUs
    
    ! set default for basin-average parameters
    outputStructure(1)%bparStruct%gru(iGRU)%var(:) = basinParFallback(:)%default_val
    
  end do  ! looping through GRUs


  ! *****************************************************************************
  ! *** Read Parameters
  ! *****************************************************************************
  checkHRU = integerMissing
  call read_param(iRunMode,checkHRU,start_gru,num_hru,num_gru,outputStructure(1)%idStruct,&
                  outputStructure(1)%mparStruct,outputStructure(1)%bparStruct,err,message)
  if(err/=0)then; print*,trim(message); return; endif

  ! *****************************************************************************
  ! *** compute derived model variables that are pretty much constant for the basin as a whole
  ! *****************************************************************************
  ! ! loop through GRUs
  do iGRU=1,num_gru
    ! calculate the fraction of runoff in future time steps
    call fracFuture(outputStructure(1)%bparStruct%gru(iGRU)%var,    &  ! vector of basin-average model parameters
                    outputStructure(1)%bvarStruct_init%gru(iGRU),    &  ! data structure of basin-average variables
                    err,message)                   ! error control
    if(err/=0)then; print*, trim(message); return; endif

    ! loop through local HRUs
    do iHRU=1,gru_struc(iGRU)%hruCount

    
      kHRU=0
      ! check the network topology (only expect there to be one downslope HRU)
      do jHRU=1,gru_struc(iGRU)%hruCount
      if(outputStructure(1)%typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%downHRUindex) == outputStructure(1)%idStruct%gru(iGRU)%hru(jHRU)%var(iLookID%hruId))then
        if(kHRU==0)then  ! check there is a unique match
        kHRU=jHRU
        else
        message=trim(message)//'only expect there to be one downslope HRU'; print*, message; return
        end if  ! (check there is a unique match)
      end if  ! (if identified a downslope HRU)
      end do


      ! check that the parameters are consistent
      call paramCheck(outputStructure(1)%mparStruct%gru(iGRU)%hru(iHRU),err,message)
      if(err/=0)then; print*, message; return; endif


      ! calculate a look-up table for the temperature-enthalpy conversion: snow
      ! NOTE1: this should eventually be replaced by the more general routine below
      ! NOTE2: this does not actually need to be called for each HRU and GRU
      call E2T_lookup(outputStructure(1)%mparStruct%gru(iGRU)%hru(iHRU),err,message)
      if(err/=0)then; print*,message; return; endif

      ! calculate a lookup table to compute enthalpy from temperature, only for enthalpyFD
#ifdef V4_ACTIVE      
      if(model_decisions(iLookDECISIONS%howHeatCap)%iDecision == enthalpyFD)then
        call T2E_lookup(gru_struc(iGRU)%hruInfo(iHRU)%nSoil,   &   ! intent(in):    number of soil layers
                        outputStructure(1)%mparStruct%gru(iGRU)%hru(iHRU),        &   ! intent(in):    parameter data structure
                        outputStructure(1)%lookupStruct%gru(iGRU)%hru(iHRU),      &   ! intent(inout): lookup table data structure
                        err,message)                              ! intent(out):   error control
        if(err/=0)then; print*, message; return; endif
      endif
#endif
      ! overwrite the vegetation height
      HVT(outputStructure(1)%typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex)) = outputStructure(1)%mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%heightCanopyTop)%dat(1)
      HVB(outputStructure(1)%typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex)) = outputStructure(1)%mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%heightCanopyBottom)%dat(1)
         
      ! overwrite the tables for LAI and SAI
      if(model_decisions(iLookDECISIONS%LAI_method)%iDecision == specified)then
        SAIM(outputStructure(1)%typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex),:) = outputStructure(1)%mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%winterSAI)%dat(1)
        LAIM(outputStructure(1)%typeStruct%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex),:) = outputStructure(1)%mparStruct%gru(iGRU)%hru(iHRU)%var(iLookPARAM%summerLAI)%dat(1)*greenVegFrac_monthly
      endif

    end do ! HRU
    
    ! compute total area of the upstream HRUS that flow into each HRU
    do iHRU=1,gru_struc(iGRU)%hruCount
      outputStructure(1)%upArea%gru(iGRU)%hru(iHRU) = 0._rkind
      do jHRU=1,gru_struc(iGRU)%hruCount
       ! check if jHRU flows into iHRU; assume no exchange between GRUs
       if(outputStructure(1)%typeStruct%gru(iGRU)%hru(jHRU)%var(iLookTYPE%downHRUindex)==outputStructure(1)%typeStruct%gru(iGRU)%hru(iHRU)%var(iLookID%hruId))then
        outputStructure(1)%upArea%gru(iGRU)%hru(iHRU) = outputStructure(1)%upArea%gru(iGRU)%hru(iHRU) + outputStructure(1)%attrStruct%gru(iGRU)%hru(jHRU)%var(iLookATTR%HRUarea)
       endif   ! (if jHRU is an upstream HRU)
      end do  ! jHRU
    end do  ! iHRU
  
    ! identify the total basin area for a GRU (m2)  
    outputStructure(1)%bvarStruct_init%gru(iGRU)%var(iLookBVAR%basin__totalArea)%dat(1) = 0._rkind
    do iHRU=1,gru_struc(iGRU)%hruCount
      outputStructure(1)%bvarStruct_init%gru(iGRU)%var(iLookBVAR%basin__totalArea)%dat(1) = &
      outputStructure(1)%bvarStruct_init%gru(iGRU)%var(iLookBVAR%basin__totalArea)%dat(1) + outputStructure(1)%attrStruct%gru(iGRU)%hru(iHRU)%var(iLookATTR%HRUarea)
    end do
  
  end do ! GRU






  ! *****************************************************************************
  ! Restart File
  ! *****************************************************************************
  ! define restart file path/name
  if(STATE_PATH == '') then
    restartFile = trim(SETTINGS_PATH)//trim(MODEL_INITCOND)
  else
    restartFile = trim(STATE_PATH)//trim(MODEL_INITCOND)
  endif

 ! read initial conditions
  call read_icond(restartFile,                        & ! intent(in):    name of initial conditions file
                  num_gru,                            & ! intent(in):    number of response units
                  outputStructure(1)%mparStruct,      & ! intent(in):    model parameters
                  outputStructure(1)%progStruct_init, & ! intent(inout): model prognostic variables
                  outputStructure(1)%bvarStruct_init, & ! intent(inout): model basin (GRU) variables
                  outputStructure(1)%indxStruct_init, & ! intent(inout): model indices
                  err,message)                          ! intent(out):   error control
  if(err/=0)then; print*, message; return; endif

  call check_icond(num_gru,                            &
                   outputStructure(1)%progStruct_init, &  ! intent(inout): model prognostic variables
                   outputStructure(1)%mparStruct,      & ! intent(in):    model parameters
                   outputStructure(1)%indxStruct_init, & ! intent(inout): model indices
                   err,message)                          ! intent(out):   error control
  if(err/=0)then; print*, message; return; endif  
end subroutine fileAccessActor_init_fortran



subroutine updateFailed(indxHRU) bind(C, name="updateFailed")
  USE globalData,only:failedHRUs
  implicit none
  integer(c_int), intent(in)        :: indxHRU

  failedHRUs(indxHRU) = .true.
end subroutine

subroutine resetFailedArray() bind(C, name="resetFailedArray")
  USE globalData,only:failedHRUs
  implicit none

  failedHRUs(:) = .false.

end subroutine


subroutine FileAccessActor_DeallocateStructures(handle_forcFileInfo, handle_ncid) bind(C,name="FileAccessActor_DeallocateStructures")
  USE netcdf_util_module,only:nc_file_close 
  USE globalData,only:structInfo                              ! information on the data structures
  USE globalData,only:failedHRUs
  USE globalData,only:forcingDataStruct
  USE globalData,only:vectime
  USE output_structure_module,only:outputTimeStep
  implicit none
  type(c_ptr),intent(in), value        :: handle_forcFileInfo
  type(c_ptr),intent(in), value        :: handle_ncid
  type(var_i),pointer                  :: ncid

  type(file_info_array), pointer       :: forcFileInfo
  integer(i4b)                         :: iFreq
  character(LEN=256)                   :: cmessage
  character(LEN=256)                   :: message
  integer(i4b)                         :: err

  call c_f_pointer(handle_ncid, ncid)
  call c_f_pointer(handle_forcFileInfo, forcFileInfo)


  ! close the open output FIle
  do iFreq=1,maxvarFreq
    if (ncid%var(iFreq)/=integerMissing) then
        call nc_file_close(ncid%var(iFreq),err,cmessage)
        if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
    endif   
  end do
  
  deallocate(forcFileInfo)
  deallocate(ncid)
  deallocate(failedHRUs)
  deallocate(outputTimeStep)
end subroutine FileAccessActor_DeallocateStructures


end module cppwrap_fileAccess



