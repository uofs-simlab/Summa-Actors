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
  USE globalData,only:model_decisions                         ! model decision structure
  USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions
  USE var_lookup,only:iLookTYPE                               ! look-up values for model types
  USE output_structure_module,only:outputStructure            ! output structure
  USE globalData,only:failedHRUs                              ! Flag for file access actor to know which GRUs have failed
  
  USE globalData,only:iRunModeFull,iRunModeGRU,iRunModeHRU
  USE globalData,only:iRunMode                                ! define the current running mode
  USE globalData,only:checkHRU                                ! index of the HRU for a single HRU run


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
  integer(i4b)                           :: ivar               ! counter for variables
  character(len=256)                     :: attrFile           ! attributes file name
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
    case(sameRulesAllLayers);    maxSnowLayers = 100
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
  call read_attrb(trim(attrFile),num_gru,outputStructure(1)%attrStruct(1),&
                  outputStructure(1)%typeStruct(1),outputStructure(1)%idStruct(1),err,message)
  if(err/=0)then; print*,trim(message); return; endif


  ! set default model parameters
  do iGRU=1, num_gru
    do iHRU=1, gru_struc(iGRU)%hruCount
      ! set parmameters to their default value
      outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(iHRU)%var(:) = localParFallback(:)%default_val         ! x%hru(:)%var(:)

      ! overwrite default model parameters with information from the Noah-MP tables
      call pOverwrite(outputStructure(1)%typeStruct(1)%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex),  &  ! vegetation category
                      outputStructure(1)%typeStruct(1)%gru(iGRU)%hru(iHRU)%var(iLookTYPE%soilTypeIndex), &  ! soil category
                      outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(iHRU)%var,                          &  ! default model parameters
                      err,message)                                                   ! error control
      if(err/=0)then; print*, trim(message); return; endif


   ! copy over to the parameter structure
   ! NOTE: constant for the dat(:) dimension (normally depth)
      do ivar=1,size(localParFallback)
        outputStructure(1)%mparStruct(1)%gru(iGRU)%hru(iHRU)%var(ivar)%dat(:) = outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(iHRU)%var(ivar)
      end do  ! looping through variables
    
    end do  ! looping through HRUs
    
    ! set default for basin-average parameters
    outputStructure(1)%bparStruct(1)%gru(iGRU)%var(:) = basinParFallback(:)%default_val
    
  end do  ! looping through GRUs


  ! *****************************************************************************
  ! *** Read Parameters
  ! *****************************************************************************
  irunMode = iRunModeFull; checkHRU = integerMissing
  call read_param(iRunMode,checkHRU,start_gru,num_hru,num_gru,outputStructure(1)%idStruct(1),&
                  outputStructure(1)%mparStruct(1),outputStructure(1)%bparStruct(1),err,message)
  if(err/=0)then; print*,trim(message); return; endif

  
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
  USE globalData,only:outputTimeStep
  USE globalData,only:init_cond_prog
  USE globalData,only:init_cond_bvar
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
  deallocate(init_cond_prog)
  if (allocated(init_cond_bvar))then; deallocate(init_cond_bvar); endif;
end subroutine FileAccessActor_DeallocateStructures


end module cppwrap_fileAccess



