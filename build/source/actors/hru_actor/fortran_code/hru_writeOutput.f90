module HRUwriteoOutput_module
USE,intrinsic :: iso_c_binding

USE data_types,only:&
                    var_i,          &  
                    var_i8,         &
                    var_d,          &
                    var_ilength,    &
                    var_dlength,    &
                    flagVec,        &
                    hru_type
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
USE var_lookup,only:iLookTIME                 ! named variables for time data structure
USE var_lookup,only:iLookDIAG                 ! named variables for local column model diagnostic variables
USE var_lookup,only:iLookPROG                 ! named variables for local column model prognostic variables
USE var_lookup,only:iLookINDEX                ! named variables for local column index variables
USE var_lookup,only:iLookFreq                 ! named variables for the frequency structure
USE var_lookup,only:iLookBVAR                 ! named variables for basin parameters
USE get_ixname_module,only:get_freqName       ! get name of frequency from frequency index


implicit none
private
public::writeHRUToOutputStructure

contains
subroutine writeHRUToOutputStructure(&
                            indxHRU,                   &
                            indxGRU,                   &
                            outputStep,                & ! index into the output Struc
                            handle_hru_data,           & ! local HRU data  
                            err) bind(C, name="writeHRUToOutputStructure") 
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

  USE globalData,only:forc_meta,attr_meta,type_meta           ! metaData structures
  USE output_stats,only:calcStats                             ! module for compiling output statistics
  USE hru_modelwrite_module,only:writeData,writeBasin       ! module to write model output
  USE hru_modelwrite_module,only:writeTime                  ! module to write model time
  USE hru_modelwrite_module,only:writeRestart               ! module to write model summa_readRestart
  USE hru_modelwrite_module,only:writeParm                  ! module to write model parameters
  USE time_utils_module,only:elapsedSec                       ! calculate the elapsed time
  USE globalData,only:elapsedWrite                            ! elapsed time to write data
  USE output_structure_module,only:outputStructure
  USE netcdf_util_module,only:nc_file_close                   ! close netcdf file
  USE netcdf_util_module,only:nc_file_open                    ! open netcdf file
  USE var_lookup,only:maxvarFreq                              ! maximum number of output files

  implicit none
  integer(c_int),intent(in)             :: indxHRU               ! index of hru in GRU
  integer(c_int),intent(in)             :: indxGRU               ! index of the GRU
  integer(c_int),intent(in)             :: outputStep            ! index into the output Struc
  type(c_ptr),intent(in),value          :: handle_hru_data       ! local HRU data
  integer(c_int),intent(out)            :: err

  ! local pointers
  type(hru_type), pointer               :: hru_data              ! local HRU data
  ! local variables
  character(len=256)                    :: cmessage
  character(len=256)                    :: message 
  logical(lgt)                          :: defNewOutputFile=.false.
  logical(lgt)                          :: printRestart=.false.
  logical(lgt)                          :: printProgress=.false.
  character(len=256)                    :: restartFile       ! restart file name
  character(len=256)                    :: timeString        ! portion of restart file name that contains the write-out time
  integer(i4b)                          :: iStruct           ! index of model structure
  integer(i4b)                          :: iFreq             ! index of the output frequency
  ! convert the C pointers to Fortran pointers
  call c_f_pointer(handle_hru_data, hru_data)
  err=0; message='summa_manageOutputFiles/'
  ! identify the start of the writing

  ! Many variables get there values from summa4chm_util.f90:getCommandArguments()
  call summa_setWriteAlarms(hru_data%oldTime_hru%var, hru_data%timeStruct%var, hru_data%finishTime_hru%var,  &   ! time vectors
                            newOutputFile,  defNewOutputFile,            &
                            ixRestart,      printRestart,                &   ! flag to print the restart file
                            ixProgress,     printProgress,               &   ! flag to print simulation progress
                            hru_data%resetStats%dat, hru_data%finalizeStats%dat,           &   ! flags to reset and finalize stats
                            hru_data%statCounter%var,                             &   ! statistics counter
                            err, cmessage)                                  ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

 ! If we do not do this looping we segfault - I am not sure why
  outputStructure(1)%finalizeStats%gru(indxGRU)%hru(indxHRU)%tim(outputStep)%dat(:) = hru_data%finalizeStats%dat(:)

 ! ****************************************************************************
 ! *** calculate output statistics
 ! ****************************************************************************
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc'); call calcStats(hru_data%forcStat%var, hru_data%forcStruct%var, statForc_meta, hru_data%resetStats%dat, hru_data%finalizeStats%dat, hru_data%statCounter%var, err, cmessage)
      case('prog'); call calcStats(hru_data%progStat%var, hru_data%progStruct%var, statProg_meta, hru_data%resetStats%dat, hru_data%finalizeStats%dat, hru_data%statCounter%var, err, cmessage)
      case('diag'); call calcStats(hru_data%diagStat%var, hru_data%diagStruct%var, statDiag_meta, hru_data%resetStats%dat, hru_data%finalizeStats%dat, hru_data%statCounter%var, err, cmessage)
      case('flux'); call calcStats(hru_data%fluxStat%var, hru_data%fluxStruct%var, statFlux_meta, hru_data%resetStats%dat, hru_data%finalizeStats%dat, hru_data%statCounter%var, err, cmessage)
      case('indx'); call calcStats(hru_data%indxStat%var, hru_data%indxStruct%var, statIndx_meta, hru_data%resetStats%dat, hru_data%finalizeStats%dat, hru_data%statCounter%var, err, cmessage)     
    end select
    if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
  end do  ! (looping through structures)
    
  ! calc basin stats
  call calcStats(hru_data%bvarStat%var(:), hru_data%bvarStruct%var(:), statBvar_meta, hru_data%resetStats%dat, hru_data%finalizeStats%dat, hru_data%statCounter%var, err, cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage)//'[bvar stats]'; return; endif
  
  ! write basin-average variables
  call writeBasin(indxGRU,indxHRU,outputStep,hru_data%finalizeStats%dat, &
                  hru_data%outputTimeStep%var,bvar_meta,hru_data%bvarStat%var,hru_data%bvarStruct%var,bvarChild_map,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage)//'[bvar]'; return; endif

  ! ****************************************************************************
  ! *** write data
  ! ****************************************************************************
  call writeTime(indxGRU,indxHRU,outputStep,hru_data%finalizeStats%dat, &
                time_meta,hru_data%timeStruct%var,err,message)

  ! write the model output to the OutputStructure
  ! Passes the full metadata structure rather than the stats metadata structure because
  ! we have the option to write out data of types other than statistics.
  ! Thus, we must also pass the stats parent->child maps from childStruct.
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc'); call writeData(indxGRU,indxHRU,outputStep,"forc",hru_data%finalizeStats%dat,&
                    maxLayers,forc_meta,hru_data%forcStat,hru_data%forcStruct,forcChild_map,hru_data%indxStruct,err,cmessage)
      case('prog'); call writeData(indxGRU,indxHRU,outputStep,"prog",hru_data%finalizeStats%dat,&
                    maxLayers,prog_meta,hru_data%progStat,hru_data%progStruct,progChild_map,hru_data%indxStruct,err,cmessage)
      case('diag'); call writeData(indxGRU,indxHRU,outputStep,"diag",hru_data%finalizeStats%dat,&
                    maxLayers,diag_meta,hru_data%diagStat,hru_data%diagStruct,diagChild_map,hru_data%indxStruct,err,cmessage)
      case('flux'); call writeData(indxGRU,indxHRU,outputStep,"flux",hru_data%finalizeStats%dat,&
                    maxLayers,flux_meta,hru_data%fluxStat,hru_data%fluxStruct,fluxChild_map,hru_data%indxStruct,err,cmessage)
      case('indx'); call writeData(indxGRU,indxHRU,outputStep,"indx",hru_data%finalizeStats%dat,&
                    maxLayers,indx_meta,hru_data%indxStat,hru_data%indxStruct,indxChild_map,hru_data%indxStruct,err,cmessage)
    end select
    if(err/=0)then 
      message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
      return
    endif
  end do  ! (looping through structures)

  ! *****************************************************************************
  ! *** update counters
  ! *****************************************************************************

  ! increment output file timestep
  do iFreq = 1,maxvarFreq
    hru_data%statCounter%var(iFreq) = hru_data%statCounter%var(iFreq)+1
    if(hru_data%finalizeStats%dat(iFreq)) hru_data%outputTimeStep%var(iFreq) = hru_data%outputTimeStep%var(iFreq) + 1
  end do

  ! if finalized stats, then reset stats on the next time step
  hru_data%resetStats%dat(:) = hru_data%finalizeStats%dat(:)

  ! save time vector
  hru_data%oldTime_hru%var(:) = hru_data%timeStruct%var(:)

end subroutine writeHRUToOutputStructure

end module HRUwriteoOutput_module