module cppwrap_fileAccess


  !======= Inclusions ===========
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE data_types
  USE globalData
  USE var_lookup,only:maxvarFreq                ! maximum number of output files


  implicit none
  public::ffile_info_C
  public::mDecisions_C
  public::Init_OutputStruct
  public::FileAccessActor_ReadForcing
  public::FileAccessActor_WriteOutput
  
  contains

subroutine ffile_info_C(indxGRU, handle_forcFileInfo, numFiles, err) bind(C, name='ffile_info_C')
  USE ffile_info_module,only:ffile_info 
  
  implicit none
  ! dummy variables

  integer(c_int), intent(in)             :: indxGRU
  type(c_ptr), intent(in), value         :: handle_forcFileInfo
  integer(c_int), intent(out)            :: numFiles
  integer(c_int)                         :: err
  ! local variables
  type(file_info_array),pointer          :: forcFileInfo
  character(len=256)                     :: message

  call c_f_pointer(handle_forcFileInfo, forcFileInfo)

  call ffile_info(indxGRU,forcFileInfo,numFiles,err,message)
  if(err/=0)then 
    message=trim(message)
    write(*,*) message
    return 
  endif

end subroutine ffile_info_C

subroutine mDecisions_C(num_steps,err) bind(C, name='mDecisions_C')
  USE mDecisions_module,only:mDecisions ! module to read model decisions
  USE allocspace4chm_module,only:allocLocal
  USE globalData,only:startTime,finshTime,refTime,oldTime
  USE globalData,only:time_meta

  implicit none
  ! dummy variables
  integer(c_int),intent(inout)        :: num_steps
  integer(c_int),intent(inout)        :: err                ! Error Code
  ! local variables
  integer(i4b)                        :: iStruct
  character(len=256)                  :: message            ! error message 
  character(LEN=256)                  :: cmessage           ! error message of downwind routine
  ! We need to initalize these time variables for the global variables that the hrus will copy
  ! allocate time structures
  do iStruct=1,4
    select case(iStruct)
    case(1); call allocLocal(time_meta, startTime, err=err, message=cmessage)  ! start time for the model simulation
    case(2); call allocLocal(time_meta, finshTime, err=err, message=cmessage)  ! end time for the model simulation
    case(3); call allocLocal(time_meta, refTime,   err=err, message=cmessage)  ! reference time for the model simulation
    case(4); call allocLocal(time_meta, oldTime,   err=err, message=cmessage)  ! time from the previous step
    end select
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  end do  ! looping through time structures

  call mDecisions(num_steps,err,cmessage)
    if(err/=0)then 
    message=trim(message)//trim(cmessage)
    print*, message
    return
  endif

end subroutine mDecisions_C

subroutine read_pinit_C(err) bind(C, name='read_pinit_C')
  USE globalData,only:localParFallback                        ! local column default parameters
  USE globalData,only:basinParFallback                        ! basin-average default parameters
  USE summaActors_FileManager,only:LOCALPARAM_INFO,BASINPARAM_INFO   ! files defining the default values and constraints for model parameters
  USE globalData,only:mpar_meta,bpar_meta                     ! parameter metadata structures
  USE read_pinit_module,only:read_pinit                       ! module to read initial model parameter values

  
  implicit none
  integer(c_int),intent(inout)        :: err                ! Error Code

  character(LEN=256)                  :: message           ! error message of downwind routine
  character(LEN=256)                  :: cmessage           ! error message of downwind routine


 ! *****************************************************************************
 ! *** read default model parameters
 ! *****************************************************************************

 ! read default values and constraints for model parameters (local column)
 call read_pinit(LOCALPARAM_INFO,.TRUE., mpar_meta,localParFallback,err,cmessage)
 if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

 ! read default values and constraints for model parameters (basin-average)
 call read_pinit(BASINPARAM_INFO,.FALSE.,bpar_meta,basinParFallback,err,cmessage)
 if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
end subroutine read_pinit_C

subroutine read_vegitationTables(err) bind(C, name="read_vegitationTables")
  USE SummaActors_setup,only:SOIL_VEG_GEN_PARM
  USE module_sf_noahmplsm,only:read_mp_veg_parameters         ! module to read NOAH vegetation tables
  USE summaActors_FileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaActors_FileManager,only:GENPARM,VEGPARM,SOILPARM,MPTABLE  ! files defining the noah tables
  USE globalData,only:model_decisions                         ! model decision structure
  USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions

  implicit none
  
  integer(c_int),intent(inout)        :: err                ! Error Code

  err = 0

 ! read Noah soil and vegetation tables
  call soil_veg_gen_parm(trim(SETTINGS_PATH)//trim(VEGPARM),                            & ! filename for vegetation table
  trim(SETTINGS_PATH)//trim(SOILPARM),                           & ! filename for soils table
  trim(SETTINGS_PATH)//trim(GENPARM),                            & ! filename for general table
  trim(model_decisions(iLookDECISIONS%vegeParTbl)%cDecision),    & ! classification system used for vegetation
  trim(model_decisions(iLookDECISIONS%soilCatTbl)%cDecision))      ! classification system used for soils

  ! read Noah-MP vegetation tables
  call read_mp_veg_parameters(trim(SETTINGS_PATH)//trim(MPTABLE),                       & ! filename for Noah-MP table
       trim(model_decisions(iLookDECISIONS%vegeParTbl)%cDecision)) ! classification system used for vegetation
  
end subroutine


subroutine resetOutputCounter(indxGRU) bind(C, name="resetOutputCounter")
  USE globalData,only:outputTimeStep
  implicit none
  integer(c_int),intent(in)         :: indxGRU

  outputTimeStep(indxGRU)%dat(:) = 1

end subroutine resetOutputCounter

subroutine Init_OutputStruct(handle_forcFileInfo, maxSteps, nGRU, err) bind(C, name="Init_OutputStruct")
  USE summaActors_initOutputStruct,only:initalizeOutput
  USE globalData,only:outputStructure

  implicit none
  type(c_ptr), intent(in), value        :: handle_forcFileInfo
  integer(c_int), intent(in)            :: maxSteps
  integer(c_int), intent(in)            :: nGRU
  integer(c_int), intent(inout)         :: err 

  ! local Variables
  type(file_info_array), pointer        :: forcFileInfo
  call c_f_pointer(handle_forcFileInfo, forcFileInfo)

  if (allocated(outputStructure))then
    print*, "Already Allocated"
  else
    call initalizeOutput(forcFileInfo,maxSteps,nGRU,err)
  endif

end subroutine Init_OutputStruct

subroutine FileAccessActor_ReadForcing(handle_forcFileInfo, currentFile, stepsInFile, startGRU, numGRU, err) bind(C,name="FileAccessActor_ReadForcing")
  USE access_forcing_module,only:access_forcingFile
  implicit none
  type(c_ptr), intent(in), value         :: handle_forcFileInfo
  integer(c_int), intent(in)             :: currentFile ! the current forcing file we are on
  integer(c_int), intent(inout)          :: stepsInFile
  integer(c_int), intent(in)             :: startGRU
  integer(c_int), intent(in)             :: numGRU
  integer(c_int), intent(inout)          :: err

  type(file_info_array), pointer         :: forcFileInfo
  character(len=256)                     :: message              ! error message 

  call c_f_pointer(handle_forcFileInfo, forcFileInfo)

  call access_forcingFile(forcFileInfo, currentFile, stepsInFile, startGRU, numGRU, err, message)

end subroutine FileAccessActor_ReadForcing

subroutine FileAccessActor_WriteOutput(&
                                handle_ncid,      & ! ncid of the output file
                                outputFileExists, & ! flag to check if the output file exsists
                                nSteps,           & ! number of steps to write
                                startGRU,         & ! startGRU for the entire job (for file creation)
                                numGRU,           & ! numGRUs for the entire job (for file creation)
                                hruFileInit,      & ! flag to check if specific hru params have been written
                                indxGRU,          & ! index of GRU we are currently writing for
                                indxHRU,          & ! index of HRU we are currently writing for
                                err) bind(C, name="FileAccessActor_WriteOutput")
  USE globalData,only:fileout
  USE summaActors_FileManager,only:OUTPUT_PATH,OUTPUT_PREFIX         ! define output file
  USE def_output_module,only:def_output                       ! module to define model output
  USE globalData,only:gru_struc
  USE var_lookup,only:maxVarFreq                               ! # of available output frequencies
  USE writeOutput_module,only:writeParm,writeBasin,writeTime,writeData
  USE globalData,only:structInfo
  USE globalData,only:outputStructure
  USE globalData,only:attr_meta                 ! attributes metadata structure
  USE globalData,only:type_meta                 ! veg/soil type metadata structure
  USE globalData,only:mpar_meta                 ! local parameter metadata structure
  USE globalData,only:bpar_meta                 ! basin parameter metadata structure
  USE var_lookup,only:iLookFreq                 ! named variables for the frequency structure
  USE globalData,only:bvarChild_map             ! index of the child data structure: stats bvar
  USE netcdf_util_module,only:nc_file_close 
  USE netcdf_util_module,only:nc_file_open 

  USE globalData,only:outputTimeStep
  USE globalData,only:finalizeStats
  USE netcdf

  implicit none
  ! dummy variables
  type(c_ptr),intent(in), value        :: handle_ncid       ! ncid of the output file
  logical(c_bool),intent(inout)        :: outputFileExists  ! flag to check if the output file exsists
  integer(c_int),intent(in)            :: nSteps            ! number of steps to write
  integer(c_int),intent(in)            :: startGRU          ! startGRU for the entire job (for file creation)
  integer(c_int),intent(in)            :: numGRU            ! numGRUs for the entire job (for file creation)
  logical(c_bool),intent(inout)        :: hruFileInit       ! flag to check if specific hru params have been written
  integer(c_int),intent(in)            :: indxGRU           ! index of GRU we are currently writing for
  integer(c_int),intent(in)            :: indxHRU           ! index of HRU we are currently writing for
  integer(c_int),intent(inout)         :: err               ! Error code
  
  ! local variables 
  type(var_i),pointer                  :: ncid
  character(LEN=256)                   :: startGRUString
  character(LEN=256)                   :: numGRUString
  character(LEN=256)                   :: message
  character(LEN=256)                   :: cmessage
  integer(i4b)                         :: iGRU
  integer(i4b)                         :: iStruct
  integer(i4b)                         :: iStep
  integer(i4b)                         :: iFreq

  call c_f_pointer(handle_ncid, ncid)


  ! check if we have created the file, if no create it
  if(.not.outputFileExists)then
    ! allocate space for the output file ID array
    allocate(ncid%var(maxVarFreq))
    ncid%var(:) = integerMissing

    ! initialize finalizeStats for testing purposes
    allocate(outputTimeStep(numGRU))
    do iGRU = 1, numGRU
      allocate(outputTimeStep(iGRU)%dat(maxVarFreq))
      outputTimeStep(iGRU)%dat(:) = 1
    end do 
    ! outputTimeStep(1:maxvarFreq) = 1    
    finalizeStats(:) = .false.
    finalizeStats(iLookFreq%timestep) = .true.
    ! initialize number of hru and gru in global data
    nGRUrun = numGRU
    nHRUrun = numGRU

    write(unit=startGRUString,fmt=*)startGRU
    write(unit=numGRUString,fmt=*)  numGRU
    fileout = trim(OUTPUT_PATH)//trim(OUTPUT_PREFIX)//"GRU"&
                //trim(adjustl(startGRUString))//"-"//trim(adjustl(numGRUString))

    ! def_output call will need to change to allow for numHRUs in future
    ! NA_Domain numGRU = numHRU, this is why we pass numGRU twice
    call def_output("summaVersion","buildTime","gitBranch","gitHash",numGRU,numGRU,&
      gru_struc(1)%hruInfo(1)%nSoil,fileout,ncid,err,cmessage)
      print*, "Creating Output File "//trim(fileout)
    outputFileExists = .true.
  endif

      ! Write Parameters for each HRU
  if (.not.hruFileInit)then
    do iStruct=1,size(structInfo)
      select case(trim(structInfo(iStruct)%structName))
        case('attr'); call writeParm(ncid,gru_struc(indxGRU)%hruInfo(indxHRU)%hru_ix, &
          outputStructure(1)%attrStruct(1)%gru(indxGRU)%hru(indxHRU),attr_meta,err,cmessage)
        case('type'); call writeParm(ncid,gru_struc(indxGRU)%hruInfo(indxHRU)%hru_ix, &
          outputStructure(1)%typeStruct(1)%gru(indxGRU)%hru(indxHRU),type_meta,err,cmessage)
        case('mpar'); call writeParm(ncid,gru_struc(indxGRU)%hruInfo(indxHRU)%hru_ix, &
          outputStructure(1)%mparStruct(1)%gru(indxGRU)%hru(indxHRU),mpar_meta,err,cmessage)
      end select
      if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
    end do
    hruFileInit = .true.
  
    ! write GRU parameters
    call writeParm(ncid,iGRU,outputStructure(1)%bparStruct(1)%gru(indxGRU),bpar_meta,err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

    ! set the outputTimestep variable so it is not reset every time we need to write to a file
      outputTimeStep(indxGRU)%dat(:) = 1
  endif

  ! ****************************************************************************
  ! *** write data
  ! ****************************************************************************
  do iStep=1, nSteps
    call writeBasin(ncid,indxGRU,outputTimeStep(indxGRU)%dat(:),iStep,bvar_meta, &
              outputStructure(1)%bvarStat(1)%gru(indxGRU)%hru(indxHRU)%var, &
              outputStructure(1)%bvarStruct(1)%gru(indxGRU)%hru(indxHRU)%var, bvarChild_map, err, cmessage)
  

    ! reset outputTimeStep
    ! get the number of HRUs in the run domain
    nHRUrun = sum(gru_struc%hruCount)
    ! write time information
    call writeTime(ncid,outputTimeStep(indxGRU)%dat(:),iStep,time_meta, &
              outputStructure(1)%timeStruct(1)%gru(indxGRU)%hru(indxHRU)%var,err,cmessage)
    
    do iStruct=1,size(structInfo)
      select case(trim(structInfo(iStruct)%structName))
        case('forc')
          call writeData(ncid,outputTimeStep(indxGRU)%dat(:),nHRUrun,maxLayers,indxGRU,iStep,forc_meta, &
                        outputStructure(1)%forcStat(1),outputStructure(1)%forcStruct(1),'forc', &
                        forcChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
        case('prog')
          call writeData(ncid,outputTimeStep(indxGRU)%dat(:),nHRUrun,maxLayers,indxGRU,iStep,prog_meta, & 
                        outputStructure(1)%progStat(1),outputStructure(1)%progStruct(1),'prog', &
                        progChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
        case('diag')
          call writeData(ncid,outputTimeStep(indxGRU)%dat(:),nHRUrun,maxLayers,indxGRU,iStep,diag_meta, &
                        outputStructure(1)%diagStat(1),outputStructure(1)%diagStruct(1),'diag', &
                        diagChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
        case('flux')
          call writeData(ncid,outputTimeStep(indxGRU)%dat(:),nHRUrun,maxLayers,indxGRU,iStep,flux_meta, &
                        outputStructure(1)%fluxStat(1),outputStructure(1)%fluxStruct(1),'flux', &
                        fluxChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
        case('indx')
          call writeData(ncid,outputTimeStep(indxGRU)%dat(:),nHRUrun,maxLayers,indxGRU,iStep,indx_meta, &
                        outputStructure(1)%indxStat(1),outputStructure(1)%indxStruct(1),'indx', &
                        indxChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
      end select
      if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
    end do  ! (looping through structures)
    do iFreq = 1,maxvarFreq
      if(outputStructure(1)%finalizeStats(1)%gru(indxGRU)%hru(1)%tim(iStep)%dat(iFreq)) outputTimeStep(indxGRU)%dat(iFreq) = outputTimeStep(indxGRU)%dat(iFreq) + 1
    end do ! ifreq
  end do ! istep
end subroutine

subroutine FileAccessActor_DeallocateStructures(handle_forcFileInfo, handle_ncid) bind(C,name="FileAccessActor_DeallocateStructures")
  USE globalData,only:forcingDataStruct     
  USE netcdf_util_module,only:nc_file_close 
  USE globalData,only:structInfo                              ! information on the data structures
  USE globalData,only:vecTime
  USE globalData,only:statForc_meta                           ! child metadata for stats
  USE globalData,only:statProg_meta                           ! child metadata for stats
  USE globalData,only:statDiag_meta                           ! child metadata for stats
  USE globalData,only:statFlux_meta                           ! child metadata for stats
  USE globalData,only:statIndx_meta                           ! child metadata for stats
  USE globalData,only:statBvar_meta                           ! child metadata for stats
  USE globalData,only:averageFlux_meta                        ! metadata for time-step average fluxes
  USE globalData,only:forcChild_map                           ! index of the child data structure: stats forc
  USE globalData,only:progChild_map                           ! index of the child data structure: stats prog
  USE globalData,only:diagChild_map                           ! index of the child data structure: stats diag
  USE globalData,only:fluxChild_map                           ! index of the child data structure: stats flux
  USE globalData,only:indxChild_map                           ! index of the child data structure: stats indx
  USE globalData,only:bvarChild_map                           ! index of the child data structure: stats bvar
  USE var_lookup,only:childFLUX_MEAN                          ! look-up values for timestep-average model fluxes
  USE globalData,only:gru_struc                               ! gru->hru mapping structure
  USE globalData,only:index_map  
  USE globalData,only:outputTimeStep
  USE summaActors_deallocateOuptutStruct,only:deallocateOutputStruc
  USE globalData,only:startTime,finshTime,refTime,oldTime
  implicit none
  type(c_ptr),intent(in), value        :: handle_forcFileInfo
  type(c_ptr),intent(in), value        :: handle_ncid
  type(var_i),pointer                  :: ncid

  type(file_info_array), pointer       :: forcFileInfo
  integer(i4b)                         :: iFreq
  character(LEN=256)                   :: cmessage
  character(LEN=256)                   :: message
  integer(i4b)                         :: err
  integer(i4b)                         :: iFile
  integer(i4b)                         :: iVar

  call c_f_pointer(handle_ncid, ncid)
  call c_f_pointer(handle_forcFileInfo, forcFileInfo)


  ! close the open output FIle
  do iFreq=1,maxvarFreq
    if (ncid%var(iFreq)/=integerMissing) then
        call nc_file_close(ncid%var(iFreq),err,cmessage)
        if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
    endif   
  end do
  
  ! Deallocate Forcing Structure
  do iFile = 1, size(forcingDataStruct(:))
    do iVar = 1, size(forcingDataStruct(iFile)%var(:))
      if (allocated(forcingDataStruct(iFile)%var(iVar)%dataFromFile))then
        deallocate(forcingDataStruct(iFile)%var(iVar)%dataFromFile)
      endif
    end do
    deallocate(forcingDataStruct(iFile)%var_ix)
  end do
  deallocate(forcingDataStruct)

  deallocate(forcFileInfo)
  deallocate(outputTimeStep)
  deallocate(ncid)
  deallocate(startTime%var);
  deallocate(finshTime%var);
  deallocate(refTime%var);
  deallocate(oldTime%var);
  if(allocated(vecTime)) then; deallocate(vecTime); endif
  if(allocated(averageFlux_meta)) then; deallocate(averageFlux_meta); endif
  if(allocated(statForc_meta)) then; deallocate(statForc_meta); endif
  if(allocated(statProg_meta)) then; deallocate(statProg_meta); endif
  if(allocated(statDiag_meta)) then; deallocate(statDiag_meta); endif
  if(allocated(statFlux_meta)) then; deallocate(statFlux_meta); endif
  if(allocated(statIndx_meta)) then; deallocate(statIndx_meta); endif
  if(allocated(statBvar_meta)) then; deallocate(statBvar_meta); endif
  if(allocated(forcChild_map)) then; deallocate(forcChild_map); endif
  if(allocated(progChild_map)) then; deallocate(progChild_map); endif
  if(allocated(diagChild_map)) then; deallocate(diagChild_map); endif
  if(allocated(fluxChild_map)) then; deallocate(fluxChild_map); endif
  if(allocated(indxChild_map)) then; deallocate(indxChild_map); endif
  if(allocated(bvarChild_map)) then; deallocate(bvarChild_map); endif
  if(allocated(childFLUX_MEAN))then; deallocate(childFLUX_MEAN);endif
  if(allocated(gru_struc))then; deallocate(gru_struc);endif
  if(allocated(index_map))then; deallocate(index_map);endif

end subroutine FileAccessActor_DeallocateStructures


end module cppwrap_fileAccess



