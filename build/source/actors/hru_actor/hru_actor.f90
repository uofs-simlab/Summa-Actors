module hru_actor
USE,intrinsic :: iso_c_binding
USE nrtype
implicit none


public::getSummaVariableInfo
public::prepare_output
contains
subroutine getSummaVariableInfo(var_type, var_fortran_index, data_struct) bind(C, name="getSummaVariableInfo")
  integer(c_int)          :: var_type
  integer(c_int)          :: var_fortran_index
  type(c_ptr)             :: data_struct

end subroutine getSummaVariableInfo

subroutine prepareOutput() bind(C, name="prepareOutput")
  implicit none




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

   ! check the need to create a new output file
  if(modelTimeStep==1)then
    ! define summa output files
     ! initialize error control
    err=0; message='summaActors_defineOuputParm/'
    ! write parameters for the HRU
    do iStruct=1,size(structInfo)
      select case(trim(structInfo(iStruct)%structName))
        case('attr'); call writeParm(indxGRU,indxHRU,gru_struc(indxGRU)%hruInfo(indxHRU)%hru_ix, &
                                    attrStruct,attr_meta,'attr',err,cmessage)
        case('type'); call writeParm(indxGRU,indxHRU,gru_struc(indxGRU)%hruInfo(indxHRU)%hru_ix, &
                                    typeStruct,type_meta,'type',err,cmessage)
        case('mpar'); call writeParm(indxGRU,indxHRU,gru_struc(indxGRU)%hruInfo(indxHRU)%hru_ix, &
                                    mparStruct,mpar_meta,'mpar',err,cmessage)
      end select
      if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
    end do  ! (looping through structures)

    ! write GRU parameters
    call writeParm(indxGRU,indxHRU,integerMissing,bparStruct,bpar_meta,'bpar',err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif 
    
    ! re-initalize the indices for model writing
    outputTimeStep%var(:)=1
  end if  ! if defining a new file

 ! If we do not do this looping we segfault - I am not sure why
  ! outputStructure(1)%finalizeStats(1)%gru(indxGRU)%hru(indxHRU)%tim(outputStep)%dat(:) = finalizeStats%dat(:)
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


end subroutine prepareOutput


end module hru_actor