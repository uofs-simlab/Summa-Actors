module cppwrap_job
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE data_types
  USE globalData

  implicit none
  public::initGlobals
  public::cleanUpJobActor
  private::allocTime

  contains
! **********************************************************************************************************
! public subroutine initGlobals: set global vars and directories,
! **********************************************************************************************************
subroutine initGlobals(file_manager, totalGRUs, totalHRUs, numGRUs, numHRUs, startGRUIndex, err) bind(C, name='initGlobals')
  ! subroutines and functions: initial priming
  ! USE summa4chm_util, only:getCommandArguments4chm            ! process command line arguments
  USE summaActors_FileManager,only:summa_SetTimesDirsAndFiles        ! sets directories and filenames
  USE summa_globalData,only:summa_defineGlobalData            ! used to define global summa data structures
  ! subroutines and functions: read dimensions (NOTE: NetCDF)
  USE read_attribute_module,only:read_dimension               ! module to read dimensions of GRU and HRU
  USE read_icond4chm_module,only:read_icond_nlayers           ! module to read initial condition dimensions
  ! provide access to file paths
  USE summaActors_FileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaActors_FileManager,only:STATE_PATH                        ! optional path to state/init. condition files (defaults to SETTINGS_PATH)
  USE summaActors_FileManager,only:MODEL_INITCOND                    ! name of model initial conditions file
  USE summaActors_FileManager,only:LOCAL_ATTRIBUTES                  ! name of model initial attributes file
  ! set up forcing data, needed here to avoid concurrent access
  USE ffile_info_module,only:ffile_info                       ! module to read information on forcing datafile
  USE mDecisions_module,only:mDecisions                       ! module to read model decisions
  USE allocspace4chm_module,only:allocLocal

  USE cppwrap_auxiliary,only:c_f_string

  implicit none
  ! dummy variables
  character(kind=c_char,len=1),intent(in)   :: file_manager(*)
  integer(c_int),intent(inout)              :: totalGRUs            ! Variable to return total number of GRUs (not really needed)
  integer(c_int),intent(inout)              :: totalHRUs            ! Variable to return total number of HRUs (not really needed)
  integer(c_int),intent(in)                 :: numGRUs              ! Number of GRUs for this current run
  integer(c_int),intent(out)                :: numHRUs              ! Number of HRUs for this current run
  integer(c_int),intent(in)                 :: startGRUIndex        ! Index of the starting GRU
  integer(c_int),intent(out)                :: err                  ! Error Code

  ! local variables
  character(len=256)                        :: file_manager_path    ! path/name of file defining directories and files
  character(len=256)                        :: message              ! error message 
  character(LEN=256)                        :: cmessage             ! error message of downwind routine
  character(len=256)                        :: restartFile          ! restart file name
  character(len=256)                        :: attrFile             ! attributes file name
  integer(i4b)                              :: fileGRU              ! [used for filenames] number of GRUs in the input file
  integer(i4b)                              :: fileHRU              ! [used for filenames] number of HRUs in the input file
  character(len=256)                        :: summaFileManagerFile ! path/name of file defining directories and files

  call c_f_string(file_manager, file_manager_path, 256)

  ! Conver the fileManager path to format needed for summa_SetTimesDirsAndFIles
  ! summaFileManagerFile = trim(file_manager_path)
  ! set directories and files -- summaFileManager used as command-line argument
  ! call summa_SetTimesDirsAndFiles(summaFileManagerFile,err,cmessage)
  ! ! if(err/=0)then; message=trim(message)//trim(cmessage); return; endif;
  ! if(err/=0)then
  !   message=trim(message)//trim(cmessage)
  !   print*, cmessage
  !   return
  ! endif

  ! define global data (parameters, metadata)
  call summa_defineGlobalData(err, cmessage)
  ! if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  if(err/=0)then
    message=trim(message)//trim(cmessage)
    print*, cmessage
    return
  endif
  ! Set the index of the start GRU
  startGRU = startGRUIndex

  ! *****************************************************************************
  ! *** read the number of GRUs and HRUs
  ! *****************************************************************************
  attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)
  call read_dimension(trim(attrFile),fileGRU,fileHRU,numGRUs,numHRUs,startGRUIndex,err,cmessage)
  if(err/=0)then
    message=trim(message)//trim(cmessage)
    print*, cmessage
  return
  endif
  totalGRUs = fileGRU
  totalHRUs = fileHRU

  ! *****************************************************************************
  ! *** read the number of snow and soil layers
  ! *****************************************************************************
  !  set restart filename and read the number of snow and soil layers from the initial conditions (restart) file
  if(STATE_PATH == '') then
    restartFile = trim(SETTINGS_PATH)//trim(MODEL_INITCOND)
  else
    restartFile = trim(STATE_PATH)//trim(MODEL_INITCOND)
  endif
 
  call read_icond_nlayers(trim(restartFile),numGRUs,indx_meta,err,cmessage)
  if(err/=0)then
    message=trim(message)//trim(cmessage)
    print*, cmessage
    return
  endif

  ! Initalize time structures
  call allocTime(err)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  
end subroutine initGlobals


subroutine allocTime(err)
  USE globalData,only:startTime,finshTime,refTime,oldTime
  USE allocspace4chm_module,only:allocLocal
  USE globalData,only:time_meta
  
  implicit none
  ! dummy variables
  integer(i4b),intent(inout)      :: err
  ! local variables
  character(len=256)              :: cmessage

  call allocLocal(time_meta, startTime, err=err, message=cmessage)
  call allocLocal(time_meta, finshTime, err=err, message=cmessage)
  call allocLocal(time_meta, refTime,   err=err, message=cmessage)
  call allocLocal(time_meta, oldTime,   err=err, message=cmessage)

end subroutine allocTime


subroutine cleanUpJobActor(err) bind(C, name='cleanUpJobActor')
  USE globalData,only:structInfo                              ! information on the data structures
  USE globalData,only:statForc_meta                           ! child metadata for stats
  USE globalData,only:statProg_meta                           ! child metadata for stats
  USE globalData,only:statDiag_meta                           ! child metadata for stats
  USE globalData,only:statFlux_meta                           ! child metadata for stats
  USE globalData,only:statIndx_meta                           ! child metadata for stats
  USE globalData,only:statBvar_meta                           ! child metadata for stats
  USE globalData,only:forcChild_map                           ! index of the child data structure: stats forc
  USE globalData,only:progChild_map                           ! index of the child data structure: stats prog
  USE globalData,only:diagChild_map                           ! index of the child data structure: stats diag
  USE globalData,only:fluxChild_map                           ! index of the child data structure: stats flux
  USE globalData,only:indxChild_map                           ! index of the child data structure: stats indx
  USE globalData,only:bvarChild_map                           ! index of the child data structure: stats bvar
  USE globalData,only:gru_struc                               ! gru->hru mapping structure
  USE globalData,only:index_map
  USE globalData,only:startTime,finshTime,refTime,oldTime
  USE var_lookup,only:childFLUX_MEAN                          ! look-up values for timestep-average model fluxes

  implicit none
  integer(c_int), intent(inout)     :: err
  err=0

  ! Deallocate Time Varaibles
  deallocate(startTime%var);
  deallocate(finshTime%var);
  deallocate(refTime%var);
  deallocate(oldTime%var);

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

end subroutine cleanUpJobActor

end module cppwrap_job