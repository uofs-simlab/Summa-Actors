module job_actor
  USE, intrinsic :: iso_c_binding
  
  ! global data
  USE globalData,only:integerMissing      ! missing integer value
  USE globalData,only:realMissing         ! missing double precision value
    
    
  implicit none
  public::job_init_fortran
  public::deallocateJobActor

    contains

subroutine job_init_fortran(file_manager, start_gru, num_gru,&
                            num_hru, file_gru, err) bind(C, name="job_init_fortran")
  USE nrtype  ! variable types, etc.
  
  USE summaFileManager,only:summa_SetTimesDirsAndFiles       ! sets directories and filenames
  USE summa_globalData,only:summa_defineGlobalData           ! used to define global summa data structures
  
  USE cppwrap_auxiliary,only:c_f_string           ! Convert C String to Fortran String
  
  ! provide access to file paths
  USE summaFileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaFileManager,only:STATE_PATH                        ! optional path to state/init. condition files (defaults to SETTINGS_PATH)
  USE summaFileManager,only:MODEL_INITCOND                    ! name of model initial conditions file
  USE summaFileManager,only:LOCAL_ATTRIBUTES                  ! name of model initial attributes file
  
  ! subroutines and functions: read dimensions (NOTE: NetCDF)
  USE read_attrb_module,only:read_dimension              ! module to read dimensions of GRU and HRU
  USE read_icond_module,only:read_icond_nlayers               ! module to read initial condition dimensions

  USE globalData,only:indx_meta                     ! metadata structures
  USE globalData,only:startTime,finshTime,refTime,oldTime
  USE allocspace_module,only:allocLocal
  USE globalData,only:time_meta

  ! Variables that were set by getCommandArguments()
  USE globalData,only: startGRU          ! index of the starting GRU for parallelization run
  USE globalData,only: checkHRU          ! index of the HRU for a single HRU run
  USE globalData,only: iRunMode          ! define the current running mode    
  USE globalData,only:iRunModeFull, iRunModeGRU, iRunModeHRU  ! define the running modes
  USE globalData,only:output_fileSuffix                       ! suffix for the output file
  
  implicit none

  ! dummy variables
  character(kind=c_char,len=1),intent(in)   :: file_manager
  integer(c_int),intent(inout)              :: start_gru
  integer(c_int),intent(inout)              :: num_gru
  integer(c_int),intent(inout)              :: num_hru
  integer(c_int),intent(out)                :: file_gru
  integer(c_int),intent(out)                :: err

  ! local variables
  character(len=256)                        :: summaFileManagerIn
  character(len=256)                        :: restartFile        ! restart file name
  character(len=256)                        :: attrFile           ! attributes file name
  integer(i4b)                              :: fileHRU            ! [used for filenames] number of HRUs in the input file
  character(len=128)                        :: fmtGruOutput       ! a format string used to write start and end GRU in output file names

  
  character(len=256)                        :: message

  ! Convert C Variables to Fortran Variables
  call c_f_string(file_manager, summaFileManagerIn, 256)
  summaFileManagerIn = trim(summaFileManagerIn)


  ! Set variables that were previosuly set by getCommandArguments()
  startGRU=start_gru
  iRunMode=iRunModeGRU
  checkHRU=integerMissing

  call summa_SetTimesDirsAndFiles(summaFileManagerIn,err,message)
  if(err/=0)then; print*, trim(message); return; endif

  call summa_defineGlobalData(err, message)
  if(err/=0)then; print*, trim(message); return; endif
  
  ! *****************************************************************************
  ! *** read the number of GRUs and HRUs
  ! *****************************************************************************
  ! obtain the HRU and GRU dimensions in the LocalAttribute file
  attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)
  select case (iRunMode)
    case(iRunModeFull); err=20; message='iRunModeFull not implemented for Actors Code'
    case(iRunModeGRU ); call read_dimension(trim(attrFile),file_gru,fileHRU,num_gru,num_hru,err,message,startGRU=start_gru)
    case(iRunModeHRU ); err=20; message='iRunModeHRU not implemented for Actors Code'
  end select
  if(err/=0)then; print*, trim(message); return; endif

  ! *****************************************************************************
  ! *** read the number of snow and soil layers
  ! *****************************************************************************
  ! set restart filename and read the number of snow and soil layers from the initial conditions (restart) file
  if(STATE_PATH == '') then
    restartFile = trim(SETTINGS_PATH)//trim(MODEL_INITCOND)
  else
    restartFile = trim(STATE_PATH)//trim(MODEL_INITCOND)
  endif
  call read_icond_nlayers(trim(restartFile),num_gru,indx_meta,err,message)
  if(err/=0)then; print*, trim(message); return; endif


  ! Allocate the time structures
  call allocLocal(time_meta, startTime, err=err, message=message)
  call allocLocal(time_meta, finshTime, err=err, message=message)
  call allocLocal(time_meta, refTime,   err=err, message=message)
  call allocLocal(time_meta, oldTime,   err=err, message=message)
  if(err/=0)then; print*, trim(message); return; endif

end subroutine job_init_fortran


subroutine deallocateJobActor(err) bind(C, name="deallocateJobActor")
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
    USE globalData,only:averageFlux_meta
    USE globalData,only:index_map
    USE globalData,only:startTime,finshTime,refTime,oldTime
    USE var_lookup,only:childFLUX_MEAN                          ! look-up values for timestep-average model fluxes
  
    implicit none
    integer(c_int), intent(out)     :: err
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
end subroutine



end module