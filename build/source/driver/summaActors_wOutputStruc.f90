module summaActors_writeOutputStruc

USE data_types,only:&
                    var_i,          &  
                    var_i8,         &
                    var_d,          &
                    var_ilength,    &
                    var_dlength,    &
                    flagVec
! named variables to define new output files
USE netcdf
USE netcdf_util_module,only:netcdf_err 
USE nrtype
USE globalData,only:noNewFiles
USE globalData,only:newFileEveryOct1
USE globalData,only:chunkSize               ! size of chunks to write
USE globalData,only:outputPrecision         ! data structure for output precision
USE globalData,only:integerMissing            ! missing integer
! metadata
USE globalData,only:time_meta                 ! metadata on the model time
USE globalData,only:forc_meta                 ! metadata on the model forcing data
USE globalData,only:diag_meta                 ! metadata on the model diagnostic variables
USE globalData,only:prog_meta                 ! metadata on the model prognostic variables
USE globalData,only:flux_meta                 ! metadata on the model fluxes
USE globalData,only:indx_meta                 ! metadata on the model index variables
USE globalData,only:bvar_meta                 ! metadata on basin-average variables
USE globalData,only:bpar_meta                 ! basin parameter metadata structure
USE globalData,only:mpar_meta                 ! local parameter metadata structure
! child metadata for stats
USE globalData,only:statForc_meta             ! child metadata for stats
USE globalData,only:statProg_meta             ! child metadata for stats
USE globalData,only:statDiag_meta             ! child metadata for stats
USE globalData,only:statFlux_meta             ! child metadata for stats
USE globalData,only:statIndx_meta             ! child metadata for stats
USE globalData,only:statBvar_meta             ! child metadata for stats
! index of the child data structure
USE globalData,only:forcChild_map             ! index of the child data structure: stats forc
USE globalData,only:progChild_map             ! index of the child data structure: stats prog
USE globalData,only:diagChild_map             ! index of the child data structure: stats diag
USE globalData,only:fluxChild_map             ! index of the child data structure: stats flux
USE globalData,only:indxChild_map             ! index of the child data structure: stats indx
USE globalData,only:bvarChild_map             ! index of the child data structure: stats bvar
USE globalData,only:outFreq                   ! output frequencies
! named variables
USE var_lookup,only:maxvarFreq                ! maximum number of output files
USE var_lookup,only:iLookTIME                 ! named variables for time data structure
USE var_lookup,only:iLookDIAG                 ! named variables for local column model diagnostic variables
USE var_lookup,only:iLookPROG                 ! named variables for local column model prognostic variables
USE var_lookup,only:iLookINDEX                ! named variables for local column index variables
USE var_lookup,only:iLookFreq                 ! named variables for the frequency structure
USE get_ixname_module,only:get_freqName       ! get name of frequency from frequency index


implicit none
private
public::summaActors_writeToOutputStruc

contains
subroutine summaActors_writeToOutputStruc(&
                            indxHRU,            &
                            indxGRU,            &
                            modelTimeStep,      &
                            ! statistics variables
                            forcStat,           & ! model forcing data
                            progStat,           & ! model prognostic (state) variables
                            diagStat,           & ! model diagnostic variables
                            fluxStat,           & ! model fluxes
                            indxStat,           & ! model indices
                            bvarStat,           & ! basin-average variables
                            ! primary data structures (scalars)
                            timeStruct,         & ! x%var(:)     -- model time data
                            forcStruct,         & ! x%var(:)     -- model forcing data
                            attrStruct,         & ! x%var(:)     -- local attributes for each HRU
                            typeStruct,         & ! x%var(:)     -- local classification of soil veg etc. for each HRU
                            ! primary data structures (variable length vectors)
                            indxStruct,         & ! x%var(:)%dat -- model indices
                            mparStruct,         & ! x%var(:)%dat -- model parameters
                            progStruct,         & ! x%var(:)%dat -- model prognostic (state) variables
                            diagStruct,         & ! x%var(:)%dat -- model diagnostic variables
                            fluxStruct,         & ! x%var(:)%dat -- model fluxes
                            ! basin-average structures
                            bparStruct,         & ! x%var(:)     -- basin-average parameters
                            bvarStruct,         & ! x%var(:)%dat -- basin-average variables
                            ! local HRU data
                            statCounter,        & ! x%var(:)
                            outputTimeStep,     & ! x%var(:)
                            resetStats,         & ! x%var(:)
                            finalizeStats,      & ! x%var(:)
                            finshTime,          & ! x%var(:)    -- end time for the model simulation
                            oldTime,            & ! x%var(:)    -- time for the previous model time step
                            outputStep,         & ! index into the output Struc
                            ! run time variables
                            err, message)
  USE nrtype
  USE globalData,only:structInfo
  USE globalData,only:startWrite,endWrite
  USE globalData,only:maxLayers                               ! maximum number of layers
  USE globalData,only:maxSnowLayers                           ! maximum number of snow layers

  USE globalData,only:ixProgress                              ! define frequency to write progress
  USE globalData,only:ixRestart                               ! define frequency to write restart files
  USE globalData,only:gru_struc

  USE globalData,only:newOutputFile                           ! define option for new output files
  USE summa_alarms,only:summa_setWriteAlarms
  USE summaActors_FileManager,only:OUTPUT_PATH,OUTPUT_PREFIX         ! define output file
  USE summaActors_FileManager,only:STATE_PATH                        ! optional path to state output files (defaults to OUTPUT_PATH)
  USE globalData,only:output_fileSuffix

  USE globalData,only:forc_meta,attr_meta,type_meta           ! metaData structures
  USE output_stats,only:calcStats                             ! module for compiling output statistics
  USE outputStrucWrite_module,only:writeData,writeBasin       ! module to write model output
  USE outputStrucWrite_module,only:writeTime                  ! module to write model time
  USE outputStrucWrite_module,only:writeRestart               ! module to write model Restart
  USE outputStrucWrite_module,only:writeParm                  ! module to write model parameters
  USE time_utils_module,only:elapsedSec                       ! calculate the elapsed time
  USE globalData,only:elapsedWrite                            ! elapsed time to write data
  USE globalData,only:outputStructure
  USE netcdf_util_module,only:nc_file_close  ! close netcdf file
  USE netcdf_util_module,only:nc_file_open   ! open netcdf file

  implicit none
  integer(i4b),intent(in)                  :: indxHRU         ! index of hru in GRU
  integer(i4b),intent(in)                  :: indxGRU         ! index of the GRU
  integer(i4b),intent(in)                  :: modelTimeStep   ! time step index
  ! statistics variables 
  type(var_dlength),intent(inout)          :: forcStat        ! model forcing data
  type(var_dlength),intent(inout)          :: progStat        ! model prognostic (state) variables
  type(var_dlength),intent(inout)          :: diagStat        ! model diagnostic variables
  type(var_dlength),intent(inout)          :: fluxStat        ! model fluxes
  type(var_dlength),intent(inout)          :: indxStat        ! model indices
  type(var_dlength),intent(inout)          :: bvarStat        ! basin-average variabl
  ! primary data structures (scalars)
  type(var_i),intent(inout)                :: timeStruct      ! model time data
  type(var_d),intent(inout)                :: forcStruct      ! model forcing data
  type(var_d),intent(inout)                :: attrStruct      ! local attributes for each HRU
  type(var_i),intent(inout)                :: typeStruct      ! local classification of soil veg etc. for each HRU
  ! primary data structures (variable length vectors)
  type(var_ilength),intent(inout)          :: indxStruct      ! model indices
  type(var_dlength),intent(inout)          :: mparStruct      ! model parameters
  type(var_dlength),intent(inout)          :: progStruct      ! model prognostic (state) variables
  type(var_dlength),intent(inout)          :: diagStruct      ! model diagnostic variables
  type(var_dlength),intent(inout)          :: fluxStruct      ! model fluxes
  ! basin-average structures
  type(var_d),intent(inout)                :: bparStruct      ! basin-average parameters
  type(var_dlength),intent(inout)          :: bvarStruct      ! basin-average variables
  ! local HRU data
  type(var_i),intent(inout)                :: statCounter     ! time counter for stats
  type(var_i),intent(inout)                :: outputTimeStep  ! timestep in output files
  type(flagVec),intent(inout)              :: resetStats      ! flags to reset statistics
  type(flagVec),intent(inout)              :: finalizeStats   ! flags to finalize statistics
  type(var_i),intent(inout)                :: finshTime       ! end time for the model simulation
  type(var_i),intent(inout)                :: oldTime         !
  integer(i4b),intent(in)                  :: outputStep      ! index into the outputStructure
  ! run time variables
  integer(i4b),intent(out)                 :: err
  character(*),intent(out)                 :: message 

  ! local variables
  character(len=256)                       :: cmessage
  logical(lgt)                             :: defNewOutputFile=.false.
  logical(lgt)                             :: printRestart=.false.
  logical(lgt)                             :: printProgress=.false.
  character(len=256)                       :: restartFile       ! restart file name
  character(len=256)                       :: timeString        ! portion of restart file name that contains the write-out time
  integer(i4b)                             :: iStruct           ! index of model structure
  integer(i4b)                             :: nGRU
  integer(i4b)                             :: nHRU
  integer(i4b)                             :: iFreq             ! index of the output frequency
  integer(i4b)                             :: iGRU              ! Temporary index for GRU        
  nGRU = 1
  nHRU = 1
  iGRU = 1

  err=0; message='summa_manageOutputFiles/'
  ! identify the start of the writing
  call date_and_time(values=startWrite)
  ! print*, "HRU WRite timestep = ", modelTimeStep
  ! print*, "OutputStep = ", outputStep
  ! ! initialize the statistics flags
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

  ! if(printProgress) write(*,'(i4,1x,5(i2,1x))') timeStruct%var(1:5)

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

    !   ! write GRU parameters
    call writeParm(indxGRU,indxHRU,integerMissing,bparStruct,bpar_meta,'bpar',err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif 
    
    ! re-initalize the indices for model writing
    outputTimeStep%var(:)=1
  end if  ! if defining a new file

  ! copy finalized stats to output structure
  outputStructure(1)%finalizeStats(1)%gru(indxGRU)%hru(indxHRU)%tim(outputStep)%dat(:) = finalizeStats%dat(:)

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
  
  ! write basin-average variables
  call writeBasin(indxGRU,indxHRU,outputStep,finalizeStats%dat, &
                  outputTimeStep%var,bvar_meta,bvarStat%var,bvarStruct%var,bvarChild_map,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage)//'[bvar]'; return; endif

  ! ****************************************************************************
  ! *** write data
  ! ****************************************************************************
  call writeTime(indxGRU,indxHRU,outputStep,finalizeStats%dat, &
                time_meta,timeStruct%var,err,message)

  ! write the model output to the NetCDF file
  ! Passes the full metadata structure rather than the stats metadata structure because
  ! we have the option to write out data of types other than statistics.
  ! Thus, we must also pass the stats parent->child maps from childStruct.
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc'); call writeData(indxGRU,indxHRU,outputStep,"forc",finalizeStats%dat,&
                    maxLayers,forc_meta,forcStat,forcStruct,forcChild_map,indxStruct,err,cmessage)
      case('prog'); call writeData(indxGRU,indxHRU,outputStep,"prog",finalizeStats%dat,&
                    maxLayers,prog_meta,progStat,progStruct,progChild_map,indxStruct,err,cmessage)
      case('diag'); call writeData(indxGRU,indxHRU,outputStep,"diag",finalizeStats%dat,&
                    maxLayers,diag_meta,diagStat,diagStruct,diagChild_map,indxStruct,err,cmessage)
      case('flux'); call writeData(indxGRU,indxHRU,outputStep,"flux",finalizeStats%dat,&
                    maxLayers,flux_meta,fluxStat,fluxStruct,fluxChild_map,indxStruct,err,cmessage)
      case('indx'); call writeData(indxGRU,indxHRU,outputStep,"indx",finalizeStats%dat,&
                    maxLayers,indx_meta,indxStat,indxStruct,indxChild_map,indxStruct,err,cmessage)
    end select
    if(err/=0)then 
      message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
      return
    endif
  end do  ! (looping through structures)
  ! *****************************************************************************
  ! *** write restart file
  ! *****************************************************************************

  ! print a restart file if requested
  if(printRestart)then
    write(timeString,'(i4,3(i2.2))') timeStruct%var(iLookTIME%iyyy),timeStruct%var(iLookTIME%im),timeStruct%var(iLookTIME%id),timeStruct%var(iLookTIME%ih)
  
    if(STATE_PATH == '') then
      restartFile=trim(OUTPUT_PATH)//trim(OUTPUT_PREFIX)//'_restart_'//trim(timeString)//trim(output_fileSuffix)//'.nc'
    else
      restartFile=trim(STATE_PATH)//trim(OUTPUT_PREFIX)//'_restart_'//trim(timeString)//trim(output_fileSuffix)//'.nc'
    endif

    call writeRestart(restartFile,nGRU,nHRU,prog_meta,progStruct,bvar_meta,bvarStruct,maxLayers,maxSnowLayers,indx_meta,indxStruct,err,cmessage)  
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
 end if

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



end subroutine summaActors_writeToOutputStruc

end module summaActors_writeOutputStruc