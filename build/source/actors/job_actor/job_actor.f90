module job_actor
  USE, intrinsic :: iso_c_binding
    
    
  implicit none
  public::job_init_fortran
  public::allocateTimeStructure
  public::deallocateJobActor

    contains

subroutine job_init_fortran(file_manager, start_gru, num_gru,&
                            num_hru, err) bind(C, name="job_init_fortran")
  USE summaFileManager,only:summa_SetTimesDirsAndFiles       ! sets directories and filenames
  USE summa_globalData,only:summa_defineGlobalData           ! used to define global summa data structures
  
  USE cppwrap_auxiliary,only:c_f_string           ! Convert C String to Fortran String
  
  ! Variables that were set by getCommandArguments()
  USE globalData,only: startGRU          ! index of the starting GRU for parallelization run

  
  implicit none

  ! dummy variables
  character(kind=c_char,len=1),intent(in)   :: file_manager
  integer(c_int),intent(in)                 :: start_gru
  integer(c_int),intent(in)                 :: num_gru
  integer(c_int),intent(in)                 :: num_hru
  integer(c_int),intent(out)                :: err

  ! local variables
  character(len=256)                        :: summaFileManagerIn
  character(len=256)                        :: message

  ! Convert C Variables to Fortran Variables
  call c_f_string(file_manager, summaFileManagerIn, 256)
  summaFileManagerIn = trim(summaFileManagerIn)


  ! Set variables that were previosuly set by getCommandArguments()
  startGRU=start_gru

  call summa_SetTimesDirsAndFiles(summaFileManagerIn,err,message)
  if(err/=0)then; print*, message; return; endif

  call summa_defineGlobalData(err, message)
  if(err/=0)then; print*, message; return; endif

end subroutine job_init_fortran


subroutine allocateTimeStructure(err) bind(C, name="allocateTimeStructure")
    USE globalData,only:startTime,finshTime,refTime,oldTime
    USE allocspace_module,only:allocLocal
    USE globalData,only:time_meta

    implicit none
    ! dummy variables
    integer(c_int),intent(inout)      :: err
    ! local variables
    character(len=256)              :: cmessage

    call allocLocal(time_meta, startTime, err=err, message=cmessage)
    call allocLocal(time_meta, finshTime, err=err, message=cmessage)
    call allocLocal(time_meta, refTime,   err=err, message=cmessage)
    call allocLocal(time_meta, oldTime,   err=err, message=cmessage)
end subroutine

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