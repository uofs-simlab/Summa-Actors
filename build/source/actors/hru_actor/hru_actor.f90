module hru_actor
USE,intrinsic :: iso_c_binding
USE nrtype
USE data_types,only:&
                    var_i,          &  
                    var_i8,         &
                    var_d,          &
                    var_ilength,    &
                    var_dlength,    &
                    flagVec
implicit none


public::getSummaVariableInfo
public::prepareOutput
contains
subroutine getSummaVariableInfo(var_type, var_fortran_index, data_struct) bind(C, name="getSummaVariableInfo")
  integer(c_int)          :: var_type
  integer(c_int)          :: var_fortran_index
  type(c_ptr)             :: data_struct

end subroutine getSummaVariableInfo

subroutine prepareOutput(&
                        modelTimeStep,      &
                        ! statistics variables
                        handle_forcStat,           & ! model forcing data
                        handle_progStat,           & ! model prognostic (state) variables
                        handle_diagStat,           & ! model diagnostic variables
                        handle_fluxStat,           & ! model fluxes
                        handle_indxStat,           & ! model indices
                        handle_bvarStat,           & ! basin-average variables
                        ! primary data structures (scalars)
                        handle_timeStruct,         & ! x%var(:)     -- model time data
                        handle_forcStruct,         & ! x%var(:)     -- model forcing data
                        handle_attrStruct,         & ! x%var(:)     -- local attributes for each HRU
                        handle_typeStruct,         & ! x%var(:)     -- local classification of soil veg etc. for each HRU
                        ! primary data structures (variable length vectors)
                        handle_indxStruct,         & ! x%var(:)%dat -- model indices
                        handle_mparStruct,         & ! x%var(:)%dat -- model parameters
                        handle_progStruct,         & ! x%var(:)%dat -- model prognostic (state) variables
                        handle_diagStruct,         & ! x%var(:)%dat -- model diagnostic variables
                        handle_fluxStruct,         & ! x%var(:)%dat -- model fluxes
                        ! basin-average structures
                        handle_bparStruct,         & ! x%var(:)     -- basin-average parameters
                        handle_bvarStruct,         & ! x%var(:)%dat -- basin-average variables
                        handle_statCounter,        &
                        handle_outputTimeStep,     & ! x%var(:)
                        handle_resetStats,         & ! x%var(:)
                        handle_finalizeStats,      & ! x%var(:)
                        handle_finshTime,          & ! x%var(:)    -- end time for the model simulation
                        handle_oldTime,            & ! x%var(:)    -- time for the previous model time step
                        err) bind(C, name="prepareOutput")
  USE globalData,only:structInfo
  USE globalData,only:startWrite,endWrite

  USE globalData,only:ixProgress                              ! define frequency to write progress
  USE globalData,only:ixRestart                               ! define frequency to write restart files
  USE globalData,only:gru_struc

  USE globalData,only:newOutputFile                           ! define option for new output files
  USE summa_alarms,only:summa_setWriteAlarms

  USE globalData,only:forc_meta,attr_meta,type_meta           ! metaData structures
  USE output_stats,only:calcStats                             ! module for compiling output statistics
  USE var_lookup,only:iLookTIME,iLookDIAG,iLookPROG,iLookINDEX, &
    iLookFreq,maxvarFreq ! named variables for time data structure
  USE globalData,only:time_meta,forc_meta,diag_meta,prog_meta,&
    flux_meta,indx_meta,bvar_meta,bpar_meta,mpar_meta           ! metadata on the model time
  USE globalData,only:statForc_meta,statProg_meta,statDiag_meta,&
    statFlux_meta,statIndx_meta,statBvar_meta             ! child metadata for stats
  ! index of the child data structure
  USE globalData,only:forcChild_map,progChild_map,diagChild_map,&
    fluxChild_map,indxChild_map,bvarChild_map             ! index of the child data structure: stats forc
  implicit none
  integer(i4b),intent(in)                  :: modelTimeStep   ! time step index
  type(c_ptr), intent(in), value           :: handle_forcStat !  model forcing data
  type(c_ptr), intent(in), value           :: handle_progStat !  model prognostic (state) variables
  type(c_ptr), intent(in), value           :: handle_diagStat !  model diagnostic variables
  type(c_ptr), intent(in), value           :: handle_fluxStat !  model fluxes
  type(c_ptr), intent(in), value           :: handle_indxStat !  model indices
  type(c_ptr), intent(in), value           :: handle_bvarStat !  basin-average variables
  ! primary data structures (scalars)
  type(c_ptr), intent(in), value           :: handle_timeStruct !  model time data
  type(c_ptr), intent(in), value           :: handle_forcStruct !  model forcing data
  type(c_ptr), intent(in), value           :: handle_attrStruct !  local attributes for each HRU
  type(c_ptr), intent(in), value           :: handle_typeStruct !  local classification of soil veg etc. for each HRU
  ! primary data structures (variable length vectors)
  type(c_ptr), intent(in), value           :: handle_indxStruct !  model indices
  type(c_ptr), intent(in), value           :: handle_mparStruct !  model parameters
  type(c_ptr), intent(in), value           :: handle_progStruct !  model prognostic (state) variables
  type(c_ptr), intent(in), value           :: handle_diagStruct !  model diagnostic variables
  type(c_ptr), intent(in), value           :: handle_fluxStruct !  model fluxes
  ! basin-average structures
  type(c_ptr), intent(in), value           :: handle_bparStruct !  basin-average parameters
  type(c_ptr), intent(in), value           :: handle_bvarStruct !  basin-average variables
  ! local HRU variables
  type(c_ptr), intent(in), value           :: handle_statCounter
  type(c_ptr), intent(in), value           :: handle_outputTimeStep
  type(c_ptr), intent(in), value           :: handle_resetStats
  type(c_ptr), intent(in), value           :: handle_finalizeStats
  type(c_ptr), intent(in), value           :: handle_finshTime    ! end time for the model simulation
  type(c_ptr), intent(in), value           :: handle_oldTime      ! time for the previous model time step
  ! run time variables
  integer(i4b),intent(out)                 :: err
  ! local vairiables for pointers
  type(var_dlength),pointer                :: forcStat        ! model forcing data
  type(var_dlength),pointer                :: progStat        ! model prognostic (state) variables
  type(var_dlength),pointer                :: diagStat        ! model diagnostic variables
  type(var_dlength),pointer                :: fluxStat        ! model fluxes
  type(var_dlength),pointer                :: indxStat        ! model indices
  type(var_dlength),pointer                :: bvarStat        ! basin-average variabl
  type(var_i),pointer                      :: timeStruct      ! model time data
  type(var_d),pointer                      :: forcStruct      ! model forcing data
  type(var_d),pointer                      :: attrStruct      ! local attributes for each HRU
  type(var_i),pointer                      :: typeStruct      ! local classification of soil veg etc. for each HRU
  type(var_ilength),pointer                :: indxStruct      ! model indices
  type(var_dlength),pointer                :: mparStruct      ! model parameters
  type(var_dlength),pointer                :: progStruct      ! model prognostic (state) variables
  type(var_dlength),pointer                :: diagStruct      ! model diagnostic variables
  type(var_dlength),pointer                :: fluxStruct      ! model fluxes
  type(var_d),pointer                      :: bparStruct      ! basin-average parameters
  type(var_dlength),pointer                :: bvarStruct      ! basin-average variables
  type(var_i),pointer                      :: statCounter     ! time counter for stats
  type(var_i),pointer                      :: outputTimeStep  ! timestep in output files
  type(flagVec),pointer                    :: resetStats      ! flags to reset statistics
  type(flagVec),pointer                    :: finalizeStats   ! flags to finalize statistics
  type(var_i),pointer                      :: finshTime       ! end time for the model simulation
  type(var_i),pointer                      :: oldTime         !
  ! local variables
  character(len=256)                       :: message 
  character(len=256)                       :: cmessage
  logical(lgt)                             :: defNewOutputFile=.false.
  integer(i4b)                             :: iFreq             ! index of the output frequency
  integer(i4b)                             :: iStruct           ! index of model structure
  logical(lgt)                             :: printProgress=.false.
  logical(lgt)                             :: printRestart=.false.
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_forcStat, forcStat)
  call c_f_pointer(handle_progStat, progStat)
  call c_f_pointer(handle_diagStat, diagStat)
  call c_f_pointer(handle_fluxStat, fluxStat)
  call c_f_pointer(handle_indxStat, indxStat)
  call c_f_pointer(handle_bvarStat, bvarStat)
  call c_f_pointer(handle_timeStruct, timeStruct)
  call c_f_pointer(handle_forcStruct, forcStruct)
  call c_f_pointer(handle_attrStruct, attrStruct)
  call c_f_pointer(handle_typeStruct, typeStruct)
  call c_f_pointer(handle_indxStruct, indxStruct)
  call c_f_pointer(handle_mparStruct, mparStruct)
  call c_f_pointer(handle_progStruct, progStruct)
  call c_f_pointer(handle_diagStruct, diagStruct)
  call c_f_pointer(handle_fluxStruct, fluxStruct)
  call c_f_pointer(handle_bparStruct, bparStruct)
  call c_f_pointer(handle_bvarStruct, bvarStruct)
  call c_f_pointer(handle_statCounter, statCounter)
  call c_f_pointer(handle_outputTimeStep, outputTimeStep)
  call c_f_pointer(handle_resetStats, resetStats)
  call c_f_pointer(handle_finalizeStats, finalizeStats)
  call c_f_pointer(handle_finshTime, finshTime);
  call c_f_pointer(handle_oldTime, oldTime)
  
  ! Start of Subroutine
  err=0; message='summa_manageOutputFiles/'
  ! identify the start of the writing
  call date_and_time(values=startWrite)

  ! initialize the statistics flags
  if(modelTimeStep==1)then

    ! initialize time step index
    allocate(statCounter%var(maxVarFreq))
    allocate(outputTimeStep%var(maxVarFreq))
    statCounter%var(1:maxVarFreq) = 1
    outputTimeStep%var(1:maxVarFreq) = 1

    allocate(resetStats%dat(maxVarFreq))
    allocate(finalizeStats%dat(maxVarFreq))
    ! initialize flags to reset/finalize statistics
    resetStats%dat(:)    = .true.   ! start by resetting statistics
    finalizeStats%dat(:) = .false.  ! do not finalize stats on the first time step

    ! set stats flag for the timestep-level output
    finalizeStats%dat(iLookFreq%timestep)=.true.
  endif  ! if the first time step

  ! Many variables get there values from summa4chm_util.f90:getCommandArguments()
  call summa_setWriteAlarms(oldTime%var, timeStruct%var, finshTime%var,  &   ! time vectors
                            newOutputFile,  defNewOutputFile,            &
                            ixRestart,      printRestart,                &   ! flag to print the restart file
                            ixProgress,     printProgress,               &   ! flag to print simulation progress
                            resetStats%dat, finalizeStats%dat,           &   ! flags to reset and finalize stats
                            statCounter%var,                             &   ! statistics counter
                            err, cmessage)                                  ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

 ! ****************************************************************************
 ! *** calculate output statistics
 ! ****************************************************************************
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc'); call calcStats(forcStat%var, forcStruct%var, statForc_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
      case('prog'); call calcStats(progStat%var, progStruct%var, statProg_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
      case('diag'); call calcStats(diagStat%var, diagStruct%var, statDiag_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
      case('flux'); call calcStats(fluxStat%var, fluxStruct%var, statFlux_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
      case('indx'); call calcStats(indxStat%var, indxStruct%var, statIndx_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)     
    end select
    if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
  end do  ! (looping through structures)
    
  ! calc basin stats
  call calcStats(bvarStat%var(:), bvarStruct%var(:), statBvar_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage)//'[bvar stats]'; return; endif

end subroutine prepareOutput

subroutine updateCounters(handle_timeStruct, handle_statCounter, handle_outputTimeStep, &
        handle_resetStats, handle_oldTime, handle_finalizeStats) bind(C, name="updateCounters")
  USE globalData,only:startWrite,endWrite,elapsedWrite
  USE var_lookup,only:maxvarFreq
  USE time_utils_module,only:elapsedSec                       ! calculate the elapsed time

  type(c_ptr), intent(in), value           :: handle_statCounter
  type(c_ptr), intent(in), value           :: handle_outputTimeStep
  type(c_ptr), intent(in), value           :: handle_resetStats
  type(c_ptr), intent(in), value           :: handle_oldTime      ! time for the previous model time step
  type(c_ptr), intent(in), value           :: handle_timeStruct !  model time data
  type(c_ptr), intent(in), value           :: handle_finalizeStats

  type(var_i),pointer                      :: statCounter     ! time counter for stats
  type(var_i),pointer                      :: outputTimeStep  ! timestep in output files
  type(flagVec),pointer                    :: resetStats      ! flags to reset statistics
  type(var_i),pointer                      :: oldTime         !
  type(var_i),pointer                      :: timeStruct      ! model time data
  type(flagVec),pointer                    :: finalizeStats   ! flags to finalize statistics

  integer(i4b)                             :: iFreq
  
  call c_f_pointer(handle_statCounter, statCounter)
  call c_f_pointer(handle_outputTimeStep, outputTimeStep)
  call c_f_pointer(handle_resetStats, resetStats)
  call c_f_pointer(handle_oldTime, oldTime)
  call c_f_pointer(handle_timeStruct, timeStruct)
  call c_f_pointer(handle_finalizeStats, finalizeStats)

  ! *****************************************************************************
  ! *** update counters
  ! *****************************************************************************
  ! increment output file timestep
  do iFreq = 1,maxvarFreq
    statCounter%var(iFreq) = statCounter%var(iFreq)+1
    if(finalizeStats%dat(iFreq)) outputTimeStep%var(iFreq) = outputTimeStep%var(iFreq) + 1
  end do

 ! if finalized stats, then reset stats on the next time step
 resetStats%dat(:) = finalizeStats%dat(:)

 ! save time vector
 oldTime%var(:) = timeStruct%var(:)

 ! *****************************************************************************
 ! *** finalize
 ! *****************************************************************************

 ! identify the end of the writing
 call date_and_time(values=endWrite)

 elapsedWrite = elapsedWrite + elapsedSec(startWrite, endWrite)
end subroutine updateCounters

end module hru_actor