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


! Read in the inital parameters, from the txt files that are give to summa as input
! LocalParamInfo.txt
! BasinParamInfo.txt
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



