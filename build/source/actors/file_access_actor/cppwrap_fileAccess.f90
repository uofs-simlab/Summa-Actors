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
  public::initFailedHRUTracker
  public::FileAccessActor_ReadForcing
  ! public::Create_Output_File
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

! THis subroutine needs to be called after the ffile_info_C subroutine
! It might make more sense to have this in the global data inialization 
! subroutine. But here it sits
subroutine mDecisions_C(num_steps,err) bind(C, name='mDecisions_C')
  USE mDecisions_module,only:mDecisions ! module to read model decisions

  implicit none
  ! dummy variables
  integer(c_int),intent(inout)        :: num_steps
  integer(c_int),intent(inout)        :: err                ! Error Code
  ! local variables
  character(len=256)                  :: message            ! error message 
  character(LEN=256)                  :: cmessage           ! error message of downwind routine

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

! allocate the failedHRU logical array and intialize it with all false values
subroutine initFailedHRUTracker(numGRU) bind(C, name="initFailedHRUTracker")
  USE globalData,only:failedHRUs
  implicit none
  integer(c_int), intent(in)        :: numGRU

  if (allocated(failedHRUs))then; deallocate(failedHRUs); endif;
  allocate(failedHRUs(numGRU))

  failedHRUs(:) = .false.


end subroutine

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

subroutine Write_HRU_Param(&
        handle_ncid,       &
        indxGRU,           &
        indxHRU,           &
        err) bind(C, name="Write_HRU_Param")
  
  USE globalData,only:attr_meta,type_meta,mpar_meta,bpar_meta ! meta structures
  USE globalData,only:gru_struc
  USE globalData,only:outputStructure
  USE writeOutput_module,only:writeParm                              

  implicit none
  ! dummy variables
  type(c_ptr),   intent(in), value  :: handle_ncid ! ncid of the output file
  integer(c_int),intent(in)         :: indxGRU     ! index of GRU in outputStructure
  integer(c_int),intent(in)         :: indxHRU     ! index of HRU in outputStructure
  integer(c_int),intent(inout)      :: err         ! err value for error control

  ! local variables
  type(var_i),pointer               :: ncid
  integer(i4b)                      :: iStruct
  character(LEN=256)                :: cmessage
  character(LEN=256)                :: message

  call c_f_pointer(handle_ncid, ncid)
  
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

  ! write GRU parameters
  call writeParm(ncid,indxGRU,outputStructure(1)%bparStruct(1)%gru(indxGRU),bpar_meta,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! set the outputTimestep variable so it is not reset every time we need to write to a file
  outputTimeStep(indxGRU)%dat(:) = 1
end subroutine

subroutine FileAccessActor_WriteOutput(&
                                handle_ncid,     & ! ncid of the output file
                                nSteps,          & ! number of steps to write
                                minGRU,          & ! index of GRU we are currently writing for
                                maxGRU,          & ! index of HRU we are currently writing for
                                err) bind(C, name="FileAccessActor_WriteOutput")
  USE def_output_module,only:def_output                       ! module to define model output
  USE var_lookup,only:maxVarFreq                               ! # of available output frequencies
  USE writeOutput_module,only:writeBasin,writeTime,writeData
  USE globalData,only:structInfo
  USE globalData,only:outputStructure
  USE var_lookup,only:iLookFreq                 ! named variables for the frequency structure
  USE globalData,only:bvarChild_map             ! index of the child data structure: stats bvar
  USE netcdf_util_module,only:nc_file_close 
  USE netcdf_util_module,only:nc_file_open 

  USE globalData,only:outputTimeStep
  USE netcdf

  implicit none
  ! dummy variables
  type(c_ptr),intent(in), value        :: handle_ncid       ! ncid of the output file
  integer(c_int),intent(in)            :: nSteps            ! number of steps to write
  integer(c_int),intent(in)            :: minGRU           ! index of GRU we are currently writing for
  integer(c_int),intent(in)            :: maxGRU           ! index of HRU we are currently writing for
  integer(c_int),intent(inout)         :: err               ! Error code
  
  ! local variables 
  type(var_i),pointer                  :: ncid
  character(LEN=256)                   :: message
  character(LEN=256)                   :: cmessage
  integer(i4b)                         :: iStruct
  integer(i4b)                         :: numGRU
  integer(i4b)                         :: iGRU
  integer(i4b)                         :: iStep
  integer(i4b)                         :: iFreq
  integer(i4b)                         :: indxHRU=1
  integer(i4b), dimension(maxVarFreq)  :: outputTimestepUpdate
  integer(i4b), dimension(maxVarFreq)  :: stepCounter

  call c_f_pointer(handle_ncid, ncid)
  ! ****************************************************************************
  ! *** write data
  ! ****************************************************************************
  do iGRU=minGRU, maxGRU
    stepCounter(:) = outputTimeStep(iGRU)%dat(:) ! We want to avoid updating outputTimeStep
    do iStep=1, nSteps
      ! call writeBasin(ncid,iGRU,stepCounter(:),iStep,bvar_meta, &
      !         outputStructure(1)%bvarStat(1)%gru(iGRU)%hru(indxHRU)%var, &
      !         outputStructure(1)%bvarStruct(1)%gru(iGRU)%hru(indxHRU)%var, bvarChild_map, err, cmessage)
      
      ! call writeTime(ncid,outputTimeStep(iGRU)%dat(:),iStep,time_meta, &
      !         outputStructure(1)%timeStruct(1)%gru(iGRU)%hru(indxHRU)%var,err,cmessage)
    end do ! istep
  end do ! iGRU
  numGRU = maxGRU-minGRU + 1 
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc')
        call writeData(ncid,outputTimeStep(minGRU)%dat(:),outputTimestepUpdate,maxLayers,nSteps,&
                      minGRU, maxGRU, numGRU, & 
                      forc_meta,outputStructure(1)%forcStat(1),outputStructure(1)%forcStruct(1),'forc', &
                      forcChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
      case('prog')
        call writeData(ncid,outputTimeStep(minGRU)%dat(:),outputTimestepUpdate,maxLayers,nSteps,&
                      minGRU, maxGRU, numGRU, &
                      prog_meta,outputStructure(1)%progStat(1),outputStructure(1)%progStruct(1),'prog', &
                      progChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
      case('diag')
        call writeData(ncid,outputTimeStep(minGRU)%dat(:),outputTimestepUpdate,maxLayers,nSteps,&
                      minGRU, maxGRU, numGRU, &
                      diag_meta,outputStructure(1)%diagStat(1),outputStructure(1)%diagStruct(1),'diag', &
                      diagChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
      case('flux')
        call writeData(ncid,outputTimeStep(minGRU)%dat(:),outputTimestepUpdate,maxLayers,nSteps,&
                      minGRU, maxGRU, numGRU, &
                      flux_meta,outputStructure(1)%fluxStat(1),outputStructure(1)%fluxStruct(1),'flux', &
                      fluxChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
      case('indx')
        call writeData(ncid,outputTimeStep(minGRU)%dat(:),outputTimestepUpdate,maxLayers,nSteps,&
                      minGRU, maxGRU, numGRU, &
                      indx_meta,outputStructure(1)%indxStat(1),outputStructure(1)%indxStruct(1),'indx', &
                      indxChild_map,outputStructure(1)%indxStruct(1),err,cmessage)
    end select
    if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
  end do  ! (looping through structures)
  
  do iFreq = 1,maxvarFreq
    outputTimeStep(minGRU)%dat(iFreq) = outputTimeStep(minGRU)%dat(iFreq) + outputTimeStepUpdate(iFreq) 
  end do ! ifreq

end subroutine




subroutine FileAccessActor_DeallocateStructures(handle_forcFileInfo, handle_ncid) bind(C,name="FileAccessActor_DeallocateStructures")
  USE netcdf_util_module,only:nc_file_close 
  USE globalData,only:structInfo                              ! information on the data structures
  USE globalData,only:outputTimeStep
  USE globalData,only:failedHRUs
  USE summaActors_deallocateOuptutStruct,only:deallocateOutputStruc
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
  deallocate(outputTimeStep)
  deallocate(ncid)
  deallocate(failedHRUs)

  call deallocateOutputStruc(err)

end subroutine FileAccessActor_DeallocateStructures


end module cppwrap_fileAccess



