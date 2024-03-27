module cppwrap_fileAccess


  !======= Inclusions ===========
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE data_types
  USE actor_data_types
  USE globalData
  USE globalData,only:integerMissing      ! missing integer value
  USE globalData,only:realMissing         ! missing double precision value



  implicit none
  public::fileAccessActor_init_fortran
  public::defOutputFortran
  public::FileAccessActor_DeallocateStructures
  public::SOIL_VEG_GEN_PARM

  character(len=64), parameter     :: summaVersion = ''
  character(len=64), parameter     :: buildTime = ''
  character(len=64), parameter     :: gitBranch = ''
  character(len=64), parameter     :: gitHash = ''
  
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
                                        err) bind(C, name="fileAccessActor_init_fortran")
  USE ffile_info_actors_module,only:ffile_info
  USE mDecisions_module,only:mDecisions                       ! module to read model decisions
  USE read_pinit_module,only:read_pinit                       ! module to read initial model parameter values
  USE module_sf_noahmplsm,only:read_mp_veg_parameters         ! module to read NOAH vegetation tables
  USE output_structure_module,only:initOutputStructure        ! module to initialize output structure
  USE output_structure_module,only:initOutputTimeStep         ! module to initialize output timestep structure (tracks GRUs timestep for output)
  USE read_attrb_module,only:read_attrb                       ! module to read local attributes
  USE read_param_module,only:read_param                       ! module to read model parameter sets
  USE pOverwrite_module,only:pOverwrite                       ! module to overwrite default parameter values with info from the Noah tables
  USE var_derive_module,only:fracFuture                       ! module to calculate the fraction of runoff in future time steps (time delay histogram)
  USE paramCheck_module,only:paramCheck                       ! module to check consistency of model parameters
  USE read_icond_module,only:read_icond                       ! module to read initial conditions
  USE check_icond_module,only:check_icond                     ! module to check initial conditions
  ! USE def_output_module,only:def_output                       ! module to define model output
  USE globalData,only:localParFallback                        ! local column default parameters
  USE globalData,only:basinParFallback                        ! basin-average default parameters
  USE globalData,only:mpar_meta,bpar_meta                     ! parameter metadata structures
  USE summaFileManager,only:LOCALPARAM_INFO,BASINPARAM_INFO   ! files defining the default values and constraints for model parameters
  USE summaFileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaFileManager,only:LOCAL_ATTRIBUTES                  ! name of model initial attributes file
  USE summaFileManager,only:GENPARM,VEGPARM,SOILPARM,MPTABLE  ! files defining the noah tables
  USE summaFileManager,only:MODEL_INITCOND                    ! name of model initial conditions file
  USE summaFileManager,only:STATE_PATH                        ! optional path to state/init. condition files (defaults to SETTINGS_PATH)
  USE summaFileManager,only:OUTPUT_PATH,OUTPUT_PREFIX ! define output file
  USE globalData,only:model_decisions                         ! model decision structure
  USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions
  USE var_lookup,only:iLookTYPE                               ! look-up values for model types
  USE var_lookup,only:iLookID                                 ! look-up values for model IDs
  USE var_lookup,only:iLookPARAM
  USE var_lookup,only:iLookATTR                               ! look-up values for model attributes
  USE var_lookup,only:iLookBVAR                               ! look-up values for basin-average variables
  USE output_structure_module,only:outputStructure            ! output structure

  USE globalData,only:iRunModeFull,iRunModeGRU,iRunModeHRU
  USE globalData,only:iRunMode                                ! define the current running mode
  USE globalData,only:checkHRU                                ! index of the HRU for a single HRU run
  
  ! look-up values for the choice of heat capacity computation
  USE mDecisions_module,only:enthalpyFD,enthalpyFDlu                       ! heat capacity using enthalpy
  
  USE mDecisions_module,only:&
                        monthlyTable,&        ! LAI/SAI taken directly from a monthly table for different vegetation classes
                        specified,&           ! LAI/SAI computed from green vegetation fraction and winterSAI and summerLAI parameters
                        sameRulesAllLayers, & ! SNTHERM option: same combination/sub-dividion rules applied to all layers
                        rulesDependLayerIndex ! CLM option: combination/sub-dividion rules depend on layer index

  USE enthalpyTemp_module,only:T2H_lookup_snow                ! module to calculate a look-up table for the snow temperature-enthalpy conversion
  USE enthalpyTemp_module,only:T2L_lookup_soil                ! module to calculate a look-up table for the soil temperature-enthalpy conversion



  USE NOAHMP_VEG_PARAMETERS,only:SAIM,LAIM    ! 2-d tables for stem area index and leaf area index (vegType,month)
  USE NOAHMP_VEG_PARAMETERS,only:HVT,HVB      ! height at the top and bottom of vegetation (vegType)
  USE globalData,only:numtim                  ! number of time steps in the simulation
  USE globalData,only:fileout                 ! name of the output file
  USE globalData,only:ncid                    ! id of the output file
  implicit none

  type(c_ptr), intent(in), value         :: handle_forcFileInfo
  integer(c_int),intent(out)             :: num_forcing_files
  integer(c_int),intent(out)             :: num_timesteps
  integer(c_int),intent(in)              :: num_timesteps_output_buffer
  type(c_ptr),intent(in), value          :: handle_output_ncid
  integer(c_int),intent(out)             :: start_gru
  integer(c_int),intent(out)             :: num_gru
  integer(c_int),intent(out)             :: num_hru
  integer(c_int),intent(out)             :: err

  ! local Variables
  type(file_info_array),pointer          :: forcFileInfo
  type(var_i),pointer                    :: output_ncid        ! id of output file
  integer(i4b)                           :: iGRU               ! counter for GRUs
  integer(i4b)                           :: iHRU               ! counter for HRUs
  integer(i4b)                           :: jHRU,kHRU          ! HRU indices
  integer(i4b)                           :: ivar               ! counter for variables
  character(len=256)                     :: attrFile           ! attributes file name
  character(LEN=256)                     :: restartFile        ! restart file name
  logical                                :: needLookup         ! logical to decide if computing enthalpy lookup tables
  
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
  
  num_timesteps = numtim ! Returns to the file_access_actor

  ! decide if computing enthalpy lookup tables, if need enthalpy and not using hypergeometric function
  needLookup = .false.
  if(model_decisions(iLookDECISIONS%nrgConserv)%iDecision == enthalpyFDlu) needLookup = .true.

  ! get the maximum number of snow layers
  select case(model_decisions(iLookDECISIONS%snowLayers)%iDecision)
    case(sameRulesAllLayers);    maxSnowLayers = 100
    case(rulesDependLayerIndex); maxSnowLayers = 5
    case default; err=20; message=trim(message)//'unable to identify option to combine/sub-divide snow layers'; return
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

      ! calculate a look-up table for the temperature-enthalpy conversion of snow for future snow layer merging
      ! NOTE2: H is the mixture enthalpy of snow liquid and ice
      call T2H_lookup_snow(outputStructure(1)%mparStruct%gru(iGRU)%hru(iHRU),err,message)
      if(err/=0)then; print*, message; return; endif

      ! calculate a lookup table for the temperature-enthalpy conversion of soil
      ! NOTE: L is the integral of soil Clapeyron equation liquid water matric potential from temperature
      !       multiply by Cp_liq*iden_water to get temperature component of enthalpy
#ifdef V4_ACTIVE      
      if(needLookup)then
        call T2L_lookup_soil(gru_struc(iGRU)%hruInfo(iHRU)%nSoil,   &   ! intent(in):    number of soil layers
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

subroutine defOutputFortran(handle_output_ncid, start_gru, num_gru, num_hru, &
    file_gru, err) bind(C, name="defOutputFortran")
  USE globalData,only:nGRUrun,nHRUrun
  USE globalData,only:fileout,output_fileSuffix
  USE globalData,only:ncid
  USE globalData,only:integerMissing
  USE globalData,only:iRunMode,iRunModeFull,iRunModeGRU,iRunModeHRU ! define the running modes

  USE summaFileManager,only:OUTPUT_PATH,OUTPUT_PREFIX ! define output file

  USE var_lookup,only:maxvarFreq                ! maximum number of output files

  USE def_output_module,only:def_output ! module to define model output
  
  implicit none

  ! Dummy Variables
  type(c_ptr),intent(in), value          :: handle_output_ncid
  integer(c_int),intent(in)              :: start_gru
  integer(c_int),intent(in)              :: num_gru
  integer(c_int),intent(in)              :: num_hru
  integer(c_int),intent(in)              :: file_gru
  integer(c_int),intent(out)             :: err
  ! Local Variables
  type(var_i),pointer                    :: output_ncid
  character(len=128)                     :: fmtGruOutput ! a format string used to write start and end GRU in output file names

  character(len=256)                     :: message ! error message


  call c_f_pointer(handle_output_ncid, output_ncid)

  output_fileSuffix = ''
  if (output_fileSuffix(1:1) /= '_') output_fileSuffix='_'//trim(output_fileSuffix)
  if (output_fileSuffix(len_trim(output_fileSuffix):len_trim(output_fileSuffix)) == '_') output_fileSuffix(len_trim(output_fileSuffix):len_trim(output_fileSuffix)) = ' '
  select case (iRunMode)
    case(iRunModeGRU)
      ! left zero padding for startGRU and endGRU
      write(fmtGruOutput,"(i0)") ceiling(log10(real(file_gru)+0.1))                      ! maximum width of startGRU and endGRU
      fmtGruOutput = "i"//trim(fmtGruOutput)//"."//trim(fmtGruOutput)                   ! construct the format string for startGRU and endGRU
      fmtGruOutput = "('_G',"//trim(fmtGruOutput)//",'-',"//trim(fmtGruOutput)//")"
      write(output_fileSuffix((len_trim(output_fileSuffix)+1):len(output_fileSuffix)),fmtGruOutput) start_gru,start_gru+num_gru-1
    case(iRunModeHRU)
      write(output_fileSuffix((len_trim(output_fileSuffix)+1):len(output_fileSuffix)),"('_H',i0)") checkHRU
  end select



  nGRUrun = num_gru
  nHRUrun = num_hru
  fileout = trim(OUTPUT_PATH)//trim(OUTPUT_PREFIX)//trim(output_fileSuffix)
  ncid(:) = integerMissing
  call def_output(summaVersion,buildTime,gitBranch,gitHash,num_gru,num_hru,&
      gru_struc(1)%hruInfo(1)%nSoil,fileout,err,message)
  if(err/=0)then; print*,trim(message); return; endif
  ! allocate space for the output file ID array
  if (.not.allocated(output_ncid%var))then
    allocate(output_ncid%var(maxVarFreq))
    output_ncid%var(:) = integerMissing
  endif
  ! copy ncid
  output_ncid%var(:) = ncid(:)


end subroutine defOutputFortran


subroutine FileAccessActor_DeallocateStructures(handle_forcFileInfo, handle_ncid) bind(C,name="FileAccessActor_DeallocateStructures")
  USE netcdf_util_module,only:nc_file_close 
  USE globalData,only:structInfo                              ! information on the data structures
  USE access_forcing_module,only:forcingDataStruct
  USE access_forcing_module,only:vectime
  USE output_structure_module,only:outputTimeStep
  USE var_lookup,only:maxvarFreq                ! maximum number of output files
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
  deallocate(outputTimeStep)
end subroutine FileAccessActor_DeallocateStructures

! **************************************************************************************************
! private subroutine SOIL_VEG_GEN_PARM: Read soil, vegetation and other model parameters (from NOAH)
! **************************************************************************************************
SUBROUTINE SOIL_VEG_GEN_PARM(FILENAME_VEGTABLE, FILENAME_SOILTABLE, FILENAME_GENERAL, MMINLU, MMINSL)
  !-----------------------------------------------------------------
  use module_sf_noahlsm, only : shdtbl, nrotbl, rstbl, rgltbl, &
  &                        hstbl, snuptbl, maxalb, laimintbl, &
  &                        bb, drysmc, f11, maxsmc, laimaxtbl, &
  &                        emissmintbl, emissmaxtbl, albedomintbl, &
  &                        albedomaxtbl, wltsmc, qtz, refsmc, &
  &                        z0mintbl, z0maxtbl, &
  &                        satpsi, satdk, satdw, &
  &                        theta_res, theta_sat, vGn_alpha, vGn_n, k_soil, &  ! MPC add van Genutchen parameters
  &                        fxexp_data, lvcoef_data, &
  &                        lutype, maxalb, &
  &                        slope_data, frzk_data, bare, cmcmax_data, &
  &                        cfactr_data, csoil_data, czil_data, &
  &                        refkdt_data, natural, refdk_data, &
  &                        rsmax_data, salp_data, sbeta_data, &
  &                        zbot_data, smhigh_data, smlow_data, &
  &                        lucats, topt_data, slcats, slpcats, sltype

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: FILENAME_VEGTABLE, FILENAME_SOILTABLE, FILENAME_GENERAL
  CHARACTER(LEN=*), INTENT(IN) :: MMINLU, MMINSL
  integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
  integer :: ierr
  INTEGER , PARAMETER :: OPEN_OK = 0

  character*128 :: mess , message

  !-----SPECIFY VEGETATION RELATED CHARACTERISTICS :
  !             ALBBCK: SFC albedo (in percentage)
  !                 Z0: Roughness length (m)
  !             SHDFAC: Green vegetation fraction (in percentage)
  !  Note: The ALBEDO, Z0, and SHDFAC values read from the following table
  !          ALBEDO, amd Z0 are specified in LAND-USE TABLE; and SHDFAC is
  !          the monthly green vegetation data
  !             CMXTBL: MAX CNPY Capacity (m)
  !             NROTBL: Rooting depth (layer)
  !              RSMIN: Mimimum stomatal resistance (s m-1)
  !              RSMAX: Max. stomatal resistance (s m-1)
  !                RGL: Parameters used in radiation stress function
  !                 HS: Parameter used in vapor pressure deficit functio
  !               TOPT: Optimum transpiration air temperature. (K)
  !             CMCMAX: Maximum canopy water capacity
  !             CFACTR: Parameter used in the canopy inteception calculati
  !               SNUP: Threshold snow depth (in water equivalent m) that
  !                     implies 100% snow cover
  !                LAI: Leaf area index (dimensionless)
  !             MAXALB: Upper bound on maximum albedo over deep snow
  !
  !-----READ IN VEGETAION PROPERTIES FROM VEGPARM.TBL
  !

  OPEN(19, FILE=trim(FILENAME_VEGTABLE),FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
  WRITE(message,FMT='(A)') &
  'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
  CALL wrf_error_fatal ( message )
  END IF

  LUMATCH=0

  FIND_LUTYPE : DO WHILE (LUMATCH == 0)
  READ (19,*,END=2002)
  READ (19,*,END=2002)LUTYPE
  READ (19,*)LUCATS,IINDEX

  IF(LUTYPE.EQ.MMINLU)THEN
  WRITE( mess , * ) 'LANDUSE TYPE = ' // TRIM ( LUTYPE ) // ' FOUND', LUCATS,' CATEGORIES'
  ! CALL wrf_message( mess )
  LUMATCH=1
  ELSE
  ! call wrf_message ( "Skipping over LUTYPE = " // TRIM ( LUTYPE ) )
  DO LC = 1, LUCATS+12
  read(19,*)
  ENDDO
  ENDIF
  ENDDO FIND_LUTYPE
  ! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(SHDTBL)       < LUCATS .OR. &
  SIZE(NROTBL)       < LUCATS .OR. &
  SIZE(RSTBL)        < LUCATS .OR. &
  SIZE(RGLTBL)       < LUCATS .OR. &
  SIZE(HSTBL)        < LUCATS .OR. &
  SIZE(SNUPTBL)      < LUCATS .OR. &
  SIZE(MAXALB)       < LUCATS .OR. &
  SIZE(LAIMINTBL)    < LUCATS .OR. &
  SIZE(LAIMAXTBL)    < LUCATS .OR. &
  SIZE(Z0MINTBL)     < LUCATS .OR. &
  SIZE(Z0MAXTBL)     < LUCATS .OR. &
  SIZE(ALBEDOMINTBL) < LUCATS .OR. &
  SIZE(ALBEDOMAXTBL) < LUCATS .OR. &
  SIZE(EMISSMINTBL ) < LUCATS .OR. &
  SIZE(EMISSMAXTBL ) < LUCATS ) THEN
  CALL wrf_error_fatal('Table sizes too small for value of LUCATS in module_sf_noahdrv.F')
  ENDIF

  IF(LUTYPE.EQ.MMINLU)THEN
  DO LC=1,LUCATS
  READ (19,*)IINDEX,SHDTBL(LC),                        &
  NROTBL(LC),RSTBL(LC),RGLTBL(LC),HSTBL(LC), &
  SNUPTBL(LC),MAXALB(LC), LAIMINTBL(LC),     &
  LAIMAXTBL(LC),EMISSMINTBL(LC),             &
  EMISSMAXTBL(LC), ALBEDOMINTBL(LC),         &
  ALBEDOMAXTBL(LC), Z0MINTBL(LC), Z0MAXTBL(LC)
  ENDDO

  READ (19,*)
  READ (19,*)TOPT_DATA
  READ (19,*)
  READ (19,*)CMCMAX_DATA
  READ (19,*)
  READ (19,*)CFACTR_DATA
  READ (19,*)
  READ (19,*)RSMAX_DATA
  READ (19,*)
  READ (19,*)BARE
  READ (19,*)
  READ (19,*)NATURAL
  ENDIF

  2002 CONTINUE

  CLOSE (19)
  IF (LUMATCH == 0) then
  CALL wrf_error_fatal ("Land Use Dataset '"//MMINLU//"' not found in VEGPARM.TBL.")
  ENDIF

  !
  !-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
  !
  OPEN(19, FILE=trim(FILENAME_SOILTABLE),FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
  WRITE(message,FMT='(A)') &
  'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
  CALL wrf_error_fatal ( message )
  END IF

  WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICATION = ', TRIM ( MMINSL )
  ! CALL wrf_message( mess )

  LUMATCH=0

  ! MPC add a new soil table
  FIND_soilTYPE : DO WHILE (LUMATCH == 0)
  READ (19,*)
  READ (19,*,END=2003)SLTYPE
  READ (19,*)SLCATS,IINDEX
  IF(SLTYPE.EQ.MMINSL)THEN
  WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ', TRIM ( SLTYPE ) , ' FOUND', &
  SLCATS,' CATEGORIES'
  ! CALL wrf_message ( mess )
  LUMATCH=1
  ELSE
  ! call wrf_message ( "Skipping over SLTYPE = " // TRIM ( SLTYPE ) )
  DO LC = 1, SLCATS
  read(19,*)
  ENDDO
  ENDIF
  ENDDO FIND_soilTYPE
  ! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(BB    ) < SLCATS .OR. &
  SIZE(DRYSMC) < SLCATS .OR. &
  SIZE(F11   ) < SLCATS .OR. &
  SIZE(MAXSMC) < SLCATS .OR. &
  SIZE(REFSMC) < SLCATS .OR. &
  SIZE(SATPSI) < SLCATS .OR. &
  SIZE(SATDK ) < SLCATS .OR. &
  SIZE(SATDW ) < SLCATS .OR. &
  SIZE(WLTSMC) < SLCATS .OR. &
  SIZE(QTZ   ) < SLCATS  ) THEN
  CALL wrf_error_fatal('Table sizes too small for value of SLCATS in module_sf_noahdrv.F')
  ENDIF

  ! MPC add new soil table
  select case(trim(SLTYPE))
  case('STAS','STAS-RUC')  ! original soil tables
  DO LC=1,SLCATS
  READ (19,*) IINDEX,BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
  REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
  WLTSMC(LC), QTZ(LC)
  ENDDO
  case('ROSETTA')          ! new soil table
  DO LC=1,SLCATS
  READ (19,*) IINDEX,&
  ! new soil parameters (from Rosetta)
  theta_res(LC), theta_sat(LC),        &
  vGn_alpha(LC), vGn_n(LC), k_soil(LC), &
  ! original soil parameters
  BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
  REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
  WLTSMC(LC), QTZ(LC)
  ENDDO
  case default
  CALL wrf_message( 'SOIL TEXTURE IN INPUT FILE DOES NOT ' )
  CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
  CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
  end select

  2003 CONTINUE

  CLOSE (19)

  IF(LUMATCH.EQ.0)THEN
  CALL wrf_message( 'SOIL TEXTURE IN INPUT FILE DOES NOT ' )
  CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
  CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
  ENDIF

  !
  !-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
  !
  OPEN(19, FILE=trim(FILENAME_GENERAL),FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
  WRITE(message,FMT='(A)') &
  'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
  CALL wrf_error_fatal ( message )
  END IF

  READ (19,*)
  READ (19,*)
  READ (19,*) NUM_SLOPE

  SLPCATS=NUM_SLOPE
  ! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(slope_data) < NUM_SLOPE ) THEN
  CALL wrf_error_fatal('NUM_SLOPE too large for slope_data array in module_sf_noahdrv')
  ENDIF

  DO LC=1,SLPCATS
  READ (19,*)SLOPE_DATA(LC)
  ENDDO

  READ (19,*)
  READ (19,*)SBETA_DATA
  READ (19,*)
  READ (19,*)FXEXP_DATA
  READ (19,*)
  READ (19,*)CSOIL_DATA
  READ (19,*)
  READ (19,*)SALP_DATA
  READ (19,*)
  READ (19,*)REFDK_DATA
  READ (19,*)
  READ (19,*)REFKDT_DATA
  READ (19,*)
  READ (19,*)FRZK_DATA
  READ (19,*)
  READ (19,*)ZBOT_DATA
  READ (19,*)
  READ (19,*)CZIL_DATA
  READ (19,*)
  READ (19,*)SMLOW_DATA
  READ (19,*)
  READ (19,*)SMHIGH_DATA
  READ (19,*)
  READ (19,*)LVCOEF_DATA
  CLOSE (19)

END SUBROUTINE SOIL_VEG_GEN_PARM

end module cppwrap_fileAccess
