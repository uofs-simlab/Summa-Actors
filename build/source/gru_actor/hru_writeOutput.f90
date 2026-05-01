module HRUwriteoOutput_module
USE,intrinsic :: iso_c_binding

USE globalData,only:maxSnowLayers       ! maximum number of snow layers
USE globalData,only:maxSoilLayers       ! maximum number of soil layers
USE globalData,only:maxLayers           ! maximum number of layers
USE globalData,only:nTimeDelay          ! number of timesteps in the time delay histogram
USE globalData,only:nSpecBand           ! maximum number of spectral bands
USE globalData,only:allowRoutingOutput  ! flag to allow routing variable output

USE data_types,only:&
                    var_i,            & ! x%var(:)            (i4b)
                    var_i8,           & ! x%var(:)            (i8b)
                    var_d,            & ! x%var(:)            (dp)
                    var_ilength,      & ! x%var(:)%dat        (i4b)
                    var_dlength         ! x%var(:)%dat        (dp)

USE actor_data_types,only:hru_type
! named variables to define new output files
USE netcdf
USE netcdf_util_module,only:netcdf_err 
USE nr_type
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
USE globalData,only:type_meta                 ! type metadata structure
USE globalData,only:attr_meta                 ! attribute metadata structure
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
USE var_lookup,only:iLookINDEX                ! named variables for local column index variables
USE var_lookup,only:iLookFreq                 ! named variables for the frequency structure
USE var_lookup,only:iLookBVAR                 ! named variables for basin parameters
! vector lengths
USE var_lookup, only: maxvarFreq ! number of output frequencies
USE var_lookup, only: maxvarStat ! number of statistics
USE get_ixname_module,only:get_freqName       ! get name of frequency from frequency index
USE output_buffer,only:summa_struct

implicit none
public::writeHRUOutput
public::hru_writeRestart
public::setFinalizeStatsFalse
private::writeParam
private::writeData
private::writeTime

contains

! **********************************************************************************************************
! public subroutine writeHRUOutput: write model output to file
! **********************************************************************************************************
subroutine writeHRUOutput(indxGRU, indxHRU, timestep, outputStep, hru_data, err, message)
  USE nr_type
  USE globalData,only:structInfo
  USE globalData,only:ixProgress                              ! define frequency to write progress
  USE globalData,only:ixRestart                               ! define frequency to write restart files
  USE globalData,only:gru_struc
  USE globalData,only:newOutputFile                           ! define option for new output files
  USE summa_alarms,only:summa_setWriteAlarms
  USE output_stats,only:calcStats                             ! module for compiling output statistics
  USE output_buffer,only:summa_struct
  implicit none

  ! Dummy variables
  integer(c_int),intent(in)             :: indxGRU            ! index of the GRU
  integer(c_int),intent(in)             :: indxHRU            ! index of hru in GRU
  integer(c_int),intent(in)             :: timestep           ! model timestep
  integer(c_int),intent(in)             :: outputStep         ! index into the output Struc
  type(hru_type),intent(inout)          :: hru_data           ! local HRU data
  integer(c_int),intent(out)            :: err
  character(len=256),intent(out)        :: message
  ! local variables
  character(len=256)                    :: cmessage
  logical(lgt)                          :: defNewOutputFile=.false.
  logical(lgt)                          :: printRestart=.false.
  logical(lgt)                          :: printProgress=.false.
  integer(i4b)                          :: maxLengthAll      ! maxLength all data writing
  character(len=256)                    :: restartFile       ! restart file name
  character(len=256)                    :: timeString        ! portion of restart file name that contains the write-out time
  integer(i4b)                          :: iStruct           ! index of model structure
  integer(i4b)                          :: iFreq             ! index of the output frequency

  err=0; message='summa_manageOutputFiles/'
  ! identify the start of the writing
  ! Many variables get their values from summa4chm_util.f90:getCommandArguments()
  call summa_setWriteAlarms(timestep,.false., hru_data%oldTime_hru%var, hru_data%timeStruct%var,  & 
                            hru_data%finishTime_hru%var, newOutputFile, defNewOutputFile,         &
                            ixRestart, printRestart, ixProgress, printProgress,                   &
                            hru_data%resetStats%dat, hru_data%finalizeStats%dat,                  &
                            hru_data%statCounter%var, err, cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! find longest possible length
  maxLengthAll = max(nSpecBand,maxLayers+1)
  if(allowRoutingOutput) maxLengthAll = max(maxLengthAll, nTimeDelay)

  ! Write parameters to output structure if this is the first
  if (timestep == 1)then
    do iStruct=1,size(structInfo)
      select case(trim(structInfo(iStruct)%structName))
        case('attr'); call writeParam(indxGRU,indxHRU,                           &
                                      hru_data%attrStruct,attr_meta,             &
                                      structInfo(iStruct)%structName,            &
                                      err,cmessage)
        case('type'); call writeParam(indxGRU,indxHRU,                           &
                                      hru_data%typeStruct,type_meta,             &
                                      structInfo(iStruct)%structName,            &
                                      err,cmessage)
        case('mpar'); call writeParam(indxGRU,indxHRU,                           &
                                      hru_data%mparStruct,mpar_meta,             &
                                      structInfo(iStruct)%structName,            &
                                      err,cmessage)
        case('bpar'); call writeParam(indxGRU,indxHRU,                           &
                                      hru_data%bparStruct,bpar_meta,             &
                                      structInfo(iStruct)%structName,            &
                                      err,cmessage)
      end select
      if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
    end do  ! (looping through structures)
  endif

  ! If we do not do this looping we segfault - I am not sure why
  summa_struct(1)%finalizeStats%gru(indxGRU)%hru(indxHRU)%tim(outputStep)%dat(:) = hru_data%finalizeStats%dat(:)

 ! ****************************************************************************
 ! *** calculate output statistics
 ! ****************************************************************************
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc'); call calcStats(hru_data%forcStat%var,                     &
                                   hru_data%forcStruct%var, statForc_meta,    & 
                                   hru_data%resetStats%dat,                   &     
                                   hru_data%finalizeStats%dat,                &
                                   hru_data%statCounter%var, err, cmessage)
      case('prog'); call calcStats(hru_data%progStat%var,                     &
                                   hru_data%progStruct%var, statProg_meta,    &
                                   hru_data%resetStats%dat,                   &
                                   hru_data%finalizeStats%dat,                &
                                   hru_data%statCounter%var, err, cmessage)
      case('diag'); call calcStats(hru_data%diagStat%var,                     &
                                   hru_data%diagStruct%var, statDiag_meta,    &
                                   hru_data%resetStats%dat,                   &
                                   hru_data%finalizeStats%dat,                &
                                   hru_data%statCounter%var, err, cmessage)
      case('flux'); call calcStats(hru_data%fluxStat%var,                     &
                                   hru_data%fluxStruct%var, statFlux_meta,    &
                                   hru_data%resetStats%dat,                   &
                                   hru_data%finalizeStats%dat,                &
                                   hru_data%statCounter%var, err, cmessage)
      case('indx'); call calcStats(hru_data%indxStat%var,                     &
                                   hru_data%indxStruct%var, statIndx_meta,    &
                                   hru_data%resetStats%dat,                   &
                                   hru_data%finalizeStats%dat,                &
                                   hru_data%statCounter%var, err, cmessage)     
      case('bvar'); call calcStats(hru_data%bvarStat%var,                     &
                                   hru_data%bvarStruct%var, statBvar_meta,    &
                                   hru_data%resetStats%dat,                   &
                                   hru_data%finalizeStats%dat,                &
                                   hru_data%statCounter%var, err, cmessage)
    end select
    if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
  end do  ! (looping through structures)
  
  ! ****************************************************************************
  ! *** write time, SUMMA buffered write option turned off as Actors handles buffering
  ! ****************************************************************************
  call writeTime(indxGRU,indxHRU,outputStep,hru_data%finalizeStats%dat, &
                 time_meta,hru_data%timeStruct%var,err,message)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! ****************************************************************************
  ! *** write data
  ! ****************************************************************************
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc'); call writeData(indxGRU,indxHRU,outputStep,maxLengthAll,   &
                                   "forc",hru_data%finalizeStats%dat,         &
                                   forc_meta,hru_data%forcStat,               &
                                   hru_data%forcStruct,forcChild_map,         &
                                   hru_data%indxStruct,err,cmessage)
      case('prog'); call writeData(indxGRU,indxHRU,outputStep,maxLengthAll,   &
                                   "prog",hru_data%finalizeStats%dat,         &
                                   prog_meta,hru_data%progStat,               &
                                   hru_data%progStruct,progChild_map,         &
                                   hru_data%indxStruct,err,cmessage)
      case('diag'); call writeData(indxGRU,indxHRU,outputStep,maxLengthAll,   &
                                   "diag",hru_data%finalizeStats%dat,         &
                                   diag_meta,hru_data%diagStat,               &
                                   hru_data%diagStruct,diagChild_map,         &
                                   hru_data%indxStruct,err,cmessage)
      case('flux'); call writeData(indxGRU,indxHRU,outputStep,maxLengthAll,   &
                                   "flux",hru_data%finalizeStats%dat,         &
                                   flux_meta,hru_data%fluxStat,               &
                                   hru_data%fluxStruct,fluxChild_map,         &
                                   hru_data%indxStruct,err,cmessage)
      case('indx'); call writeData(indxGRU,indxHRU,outputStep,maxLengthAll,   &
                                   "indx",hru_data%finalizeStats%dat,         &
                                   indx_meta,hru_data%indxStat,               &
                                   hru_data%indxStruct,indxChild_map,         &
                                   hru_data%indxStruct,err,cmessage)
      case('bvar'); call writeData(indxGRU,indxHRU,outputStep,maxLengthAll,   &
                                   "bvar",hru_data%finalizeStats%dat,         &
                                   bvar_meta,hru_data%bvarStat,               &
                                   hru_data%bvarStruct,bvarChild_map,         &
                                   hru_data%indxStruct,err,cmessage)                                   
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
    if(hru_data%finalizeStats%dat(iFreq)) then
      hru_data%outputTimeStep%var(iFreq) = hru_data%outputTimeStep%var(iFreq) + 1
    end if
  end do

  ! if finalized stats, then reset stats on the next time step
  hru_data%resetStats%dat(:) = hru_data%finalizeStats%dat(:)

  ! save time vector
  hru_data%oldTime_hru%var(:) = hru_data%timeStruct%var(:)

end subroutine writeHRUOutput

! **********************************************************************************************************
! public subroutine hru_writeRestart: write restart data to the output structure
! **********************************************************************************************************
subroutine hru_writeRestart(&
  indxGRU,                   &
  indxHRU,                   &
  timestep,                  & ! model checkPoint, index into the output Struc
  outputStep,                & ! unused :(
  hru_data,                  & ! local HRU data  
  err) 
  USE nr_type
  USE output_buffer,only:summa_struct
  USE var_lookup,only:iLookVarType           ! named variables for structure elements
  implicit none

  integer(c_int),intent(in)        :: indxHRU            ! index of hru in GRU
  integer(c_int),intent(in)        :: indxGRU            ! index of the GRU
  integer(c_int),intent(in)        :: timestep           ! model checkPoint
  integer(c_int),intent(in)        :: outputStep         ! index into the output Struc
  type(hru_type),intent(in)        :: hru_data           ! local HRU data
  integer(c_int),intent(out)       :: err
  character(len=256)               :: message 
  character (len = 5)              :: output_fileSuffix
  integer(i4b)                     :: iStruct            ! index of model structure
  integer(i4b)                     :: iFreq              ! index of the output frequency
  integer(i4b)                     :: iVar 
  integer(i4b)                     :: iDat
  integer(i4b)                     :: checkPoint
  
  ! convert the C pointers to Fortran pointers
  ! call c_f_pointer(handle_hru_data, hru_data)
  err=0; message='summa_manageOutputFiles/'

  checkpoint = mod(timestep, summa_struct(1)%nTimeSteps)
  if (checkpoint == 0) checkpoint = summa_struct(1)%nTimeSteps
  
  ! ****************************************************************************
  ! *** write restart data
  ! ****************************************************************************
  
  ! write prog vars
  do iVar = 1, size(hru_data%progstruct%var(:))
    select case (prog_meta(iVar)%varType)
      case(iLookVarType%scalarv);
        summa_struct(1)%progStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(checkPoint)%dat(:) = hru_data%progstruct%var(iVar)%dat(:) 
      case(iLookVarType%wlength);              
        summa_struct(1)%progStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(checkPoint)%dat(:) = hru_data%progstruct%var(iVar)%dat(:) 
      case(iLookVarType%midSoil);              
        summa_struct(1)%progStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(checkPoint)%dat(:) = hru_data%progstruct%var(iVar)%dat(:) 
      case(iLookVarType%midToto);              
        do iDat = 1, size(hru_data%progstruct%var(iVar)%dat(:))
          summa_struct(1)%progStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(checkPoint)%dat(iDat) = hru_data%progstruct%var(iVar)%dat(iDat)
        end do ! iDat 
      case(iLookVarType%ifcSoil);              
        summa_struct(1)%progStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(checkPoint)%dat(:) = hru_data%progstruct%var(iVar)%dat(:) 
      case(iLookVarType%ifcToto);              
        do iDat = 0, size(hru_data%progstruct%var(iVar)%dat(:)) -1 !varType 8 in hru_data begins its index at 0 instead of the default of 1. this case accomodates that
          summa_struct(1)%progStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(checkPoint)%dat(iDat+1) = hru_data%progstruct%var(iVar)%dat(iDat)
        end do ! iDat 
      case(iLookVarType%midSnow);
        summa_struct(1)%progStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(checkPoint)%dat(:) = hru_data%progstruct%var(iVar)%dat(:) 
      case(iLookVarType%ifcSnow); 
        summa_struct(1)%progStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(checkPoint)%dat(:) = hru_data%progstruct%var(iVar)%dat(:) 
      case default; err=20; message=trim(message)//'unknown var type'; return
   end select
  end do ! iVar

  ! write basin var
  summa_struct(1)%bvarStruct%gru(indxGRU)%hru(indxHRU)%var(iLookBVAR%routingRunoffFuture)%tim(checkPoint)%dat(:) = hru_data%bvarstruct%var(iLookBVAR%routingRunoffFuture)%dat(:)
  summa_struct(1)%indxStruct%gru(indxGRU)%hru(indxHRU)%var(iLookINDEX%nSnow)%tim(checkPoint)%dat(1) = hru_data%indxStruct%var(iLookINDEX%nSnow)%dat(1)
  
end subroutine hru_writeRestart


! **********************************************************************************************************
! private subroutine writeParam: write model parameters
! **********************************************************************************************************
subroutine writeParam(indxGRU,indxHRU,struct,meta,structName,err,message)
  ! USE globalData,only:ncid                      ! netcdf file ids
  USE data_types,only:var_info                    ! metadata info
  USE var_lookup,only:iLookStat                   ! index in statistics vector
  USE var_lookup,only:iLookFreq                   ! index in vector of model output frequencies
  implicit none
  
  ! declare input variables
  integer(i4b)  ,intent(in)   :: indxGRU          ! Index into output Structure
  integer(i4b)  ,intent(in)   :: indxHRU          ! Index into output Structure
  class(*)      ,intent(in)   :: struct           ! data structure
  type(var_info),intent(in)   :: meta(:)          ! metadata structure
  character(*)  ,intent(in)   :: structName       ! Name to know which global struct to write to
  integer(i4b)  ,intent(out)  :: err              ! error code
  character(*)  ,intent(out)  :: message          ! error message
  ! local variables
  integer(i4b)                :: iVar             ! loop through variables
  
  ! initialize error control
  err=0;message="writeParam/"

  ! loop through local column model parameters
  do iVar = 1,size(meta)

    ! check that the variable is desired
    if (meta(iVar)%statIndex(iLookFREQ%timestep)==integerMissing) cycle

    ! initialize message
    message=trim(message)//trim(meta(iVar)%varName)//'/'

    ! HRU data
    select type (struct)
      class is (var_i)
       if (structName == "type") summa_struct(1)%typeStruct%gru(indxGRU)%hru(indxHRU)%var(iVar) = struct%var(iVar)
      class is (var_i8) ! "id", but do not write here
      class is (var_d)
       if (structName == "attr") summa_struct(1)%attrStruct%gru(indxGRU)%hru(indxHRU)%var(iVar) = struct%var(iVar)
       if (structName == "bpar") summa_struct(1)%bparStruct%gru(indxGRU)%var(iVar) = struct%var(iVar) ! this will overwrite data
      class is (var_dlength)
       if (structName == "mpar") summa_struct(1)%mparStruct%gru(indxGRU)%hru(indxHRU)%var(iVar) = struct%var(iVar)
      class default; err=20; message=trim(message)//'parameter type must be var_i, var_i8, var_d, or var_dlength'; return
    end select

    ! re-initialize message
    message="writeParam/"
  end do  ! looping through local column model parameters

end subroutine writeParam

! **************************************************************************************
! private subroutine writeData: write model time-dependent data
! **************************************************************************************
subroutine writeData(indxGRU,indxHRU,iStep,maxLengthAll,structName,finalizeStats, &
                      meta,stat,datt,map,indx,err,message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:maxvarStat                     ! index into stats structure
  USE var_lookup,only:iLookVarType                   ! index into type structure
  USE var_lookup,only:iLookIndex                     ! index into index structure
  USE var_lookup,only:iLookStat                      ! index into stat structure
  USE globalData,only:outFreq                        ! output file information
  USE get_ixName_module,only:get_varTypeName         ! to access type strings for error messages
  USE get_ixName_module,only:get_statName            ! to access type strings for error messages
  implicit none

  ! declare dummy variables
  integer(i4b)  ,intent(in)        :: indxGRU
  integer(i4b)  ,intent(in)        :: indxHRU
  integer(i4b)  ,intent(in)        :: iStep
  integer(i4b)  ,intent(in)        :: maxLengthAll      ! maxLength all data
  character(*)  ,intent(in)        :: structName
  logical(lgt)  ,intent(in)        :: finalizeStats(:)  ! flags to finalize statistics
  type(var_info),intent(in)        :: meta(:)           ! meta data
  class(*)      ,intent(in)        :: stat              ! stats data
  class(*)      ,intent(in)        :: datt              ! timestep data
  integer(i4b)  ,intent(in)        :: map(:)            ! map into stats child struct
  type(var_ilength) ,intent(in)    :: indx              ! index data
  integer(i4b)  ,intent(out)       :: err               ! error code
  character(*)  ,intent(out)       :: message           ! error message
  ! local variables
  integer(i4b)                     :: iVar              ! variable index
  integer(i4b)                     :: iStat             ! statistics index
  integer(i4b)                     :: iFreq             ! frequency index
  integer(i4b)                     :: nSnow             ! number of snow layers
  integer(i4b)                     :: nSoil             ! number of soil layers
  integer(i4b)                     :: nLayers           ! total number of layers
  ! output arrays
  integer(i4b)                     :: datLength         ! length of each data vector
  ! initialize error control
  err=0

  ! loop through output frequencies
  do iFreq=1,maxvarFreq

    ! skip frequencies that are not needed
    if(.not.outFreq(iFreq)) cycle

    ! check that we have finalized statistics for a given frequency
    if(.not.finalizeStats(iFreq)) cycle

    ! loop through model variables
    iVar_loop: do iVar = 1,size(meta)

      ! initialize message
      message="writeData/"//trim(meta(iVar)%varName)

      ! handle time first
      if (meta(iVar)%varName=='time')then
         message=trim(message)//':' ! add statistic (none) to message 

        ! Write the time step values
        select type(datt)     ! forcStruc
          class is (var_d); summa_struct(1)%forcStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(iStep) = datt%var(iVar)
          class default; err=20; message=trim(message)//'time variable must be of type var_d (forcing data structure)'; return
        end select
        cycle ! move onto the next variable

      end if  ! id time

      ! define the statistics index
      iStat = meta(iVar)%statIndex(iFreq)
      message=trim(message)//'_'//trim(get_statName(iStat))//':' ! add statistic to message

      ! check that the variable is desired, currently do not write large variables (unknown and routing) as they are large and slow things down a lot
      if (iStat==integerMissing .or. meta(iVar)%varType==iLookVarType%unknown .or. meta(iVar)%varType==integerMissing) cycle
      if (meta(iVar)%varType==iLookVarType%routing .and. .not.allowRoutingOutput) cycle ! routing variable write can be turned on with the allowRoutingOutput flag

      ! stats output: only scalar variable type
      if(meta(iVar)%varType==iLookVarType%scalarv) then
        select type(stat)
          class is (var_dlength)
            select case(trim(structName))
            case('forc'); summa_struct(1)%forcStat%gru(indxGRU)%hru(indxHRU)%var(map(iVar))%tim(iStep)%dat(iFreq) = stat%var(map(iVar))%dat(iFreq)
            case('prog'); summa_struct(1)%progStat%gru(indxGRU)%hru(indxHRU)%var(map(iVar))%tim(iStep)%dat(iFreq) = stat%var(map(iVar))%dat(iFreq)
            case('diag'); summa_struct(1)%diagStat%gru(indxGRU)%hru(indxHRU)%var(map(iVar))%tim(iStep)%dat(iFreq) = stat%var(map(iVar))%dat(iFreq)
            case('flux'); summa_struct(1)%fluxStat%gru(indxGRU)%hru(indxHRU)%var(map(iVar))%tim(iStep)%dat(iFreq) = stat%var(map(iVar))%dat(iFreq)
            case('indx'); summa_struct(1)%indxStat%gru(indxGRU)%hru(indxHRU)%var(map(iVar))%tim(iStep)%dat(iFreq) = stat%var(map(iVar))%dat(iFreq)
            case('bvar'); summa_struct(1)%bvarStat%gru(indxGRU)%hru(indxHRU)%var(map(iVar))%tim(iStep)%dat(iFreq) = stat%var(map(iVar))%dat(iFreq)
            case default; err=21; message=trim(message)//"stats structure not found"; return
            end select
          class default; err=20; message=trim(message)//'stats must be scalarv and of type gru_hru_doubleVec or gru_doubleVec'; return
        end select  ! stat

        ! non-scalar variables: regular data structures
      else

        ! get the model layers
        nSoil   = indx%var(iLookIndex%nSoil)%dat(1)
        summa_struct(1)%indxStruct%gru(indxGRU)%hru(indxHRU)%var(iLookIndex%nSoil)%tim(iStep)%dat(1)   = nSoil
        nSnow   = indx%var(iLookIndex%nSnow)%dat(1)
        summa_struct(1)%indxStruct%gru(indxGRU)%hru(indxHRU)%var(iLookIndex%nSnow)%tim(iStep)%dat(1)   = nSnow
        nLayers = indx%var(iLookIndex%nLayers)%dat(1)
        summa_struct(1)%indxStruct%gru(indxGRU)%hru(indxHRU)%var(iLookIndex%nLayers)%tim(iStep)%dat(1) = nLayers

        ! get the length of each data vector
        select case (meta(iVar)%varType)
          case(iLookVarType%wLength); datLength = nSpecBand
          case(iLookVarType%midToto); datLength = nLayers
          case(iLookVarType%midSnow); datLength = nSnow
          case(iLookVarType%midSoil); datLength = nSoil
          case(iLookVarType%ifcToto); datLength = nLayers+1
          case(iLookVarType%ifcSnow); datLength = nSnow+1
          case(iLookVarType%ifcSoil); datLength = nSoil+1
          case(iLookVarType%routing); datLength = nTimeDelay
          message="writeData/"; cycle iVar_loop ! move onto the next variable
          ! case parSoil only in parameters (mpar, not written here) 
          ! case unknown skipped above
        end select ! varType
      
        ! get the data vectors
        select type (datt)
          class is (var_dlength)
            select case(trim(structName))
              case('prog'); summa_struct(1)%progStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(iStep)%dat(1:datLength) = datt%var(iVar)%dat(:)
              case('diag'); summa_struct(1)%diagStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(iStep)%dat(1:datLength) = datt%var(iVar)%dat(:)
              case('flux'); summa_struct(1)%fluxStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(iStep)%dat(1:datLength) = datt%var(iVar)%dat(:)
              case('bvar'); summa_struct(1)%bvarStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(iStep)%dat(1:datLength) = datt%var(iVar)%dat(:)
              ! note, 'forc' data structure only has scalar variables, and thus is covered above in the stats output section 
              case default; err=21; message=trim(message)//'data structure not found for var_dlength output'
            end select
          class is (var_ilength)
            select case(trim(structName))
              case('indx'); summa_struct(1)%indxStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(iStep)%dat(1:datLength) = datt%var(iVar)%dat(:)
              case default; err=21; message=trim(message)//'data structure not found for var_ilength output'
            end select
          class default; err=20; message=trim(message)//'data must not be scalarv and either of type var_dlength or var_ilength'; return
        end select

      end if ! not scalarv
      message="writeData/" ! re-initialize message

    end do iVar_loop ! iVar
  end do ! iFreq
end subroutine writeData

! **************************************************************************************
! private subroutine writeTime: write current time to all files
! **************************************************************************************
subroutine writeTime(indxGRU,indxHRU,iStep,finalizeStats,meta,datt,err,message)
 USE data_types,only:var_info                       ! metadata type
 USE var_lookup,only:iLookStat                      ! index into stat structure
 implicit none

 ! declare dummy variables
 integer(i4b)  ,intent(in)     :: indxGRU
 integer(i4b)  ,intent(in)     :: indxHRU
 integer(i4b)  ,intent(in)     :: iStep
 logical(lgt)  ,intent(in)     :: finalizeStats(:)  ! flags to finalize statistics
 type(var_info),intent(in)     :: meta(:)           ! meta data
 integer       ,intent(in)     :: datt(:)           ! timestep data
 integer(i4b)  ,intent(out)    :: err               ! error code
 character(*)  ,intent(out)    :: message           ! error message
 ! local variables
 integer(i4b)                  :: iVar              ! variable index
 integer(i4b)                  :: iFreq             ! frequency index
 ! initialize error control
 err=0;message="writeTime/"

 ! loop through output frequencies
 do iFreq=1,maxvarFreq

  ! check that we have finalized statistics for a given frequency
  if(.not.finalizeStats(iFreq)) cycle

  ! loop through model variables
  do iVar = 1,size(meta)

   ! check instantaneous
   if (meta(iVar)%statIndex(iFreq)/=iLookStat%inst) cycle

   ! add to summa_struct
   summa_struct(1)%timeStruct%gru(indxGRU)%hru(indxHRU)%var(iVar)%tim(iStep) = datt(iVar)
   if (err/=0) then; message=trim(message)//trim(meta(iVar)%varName); err=20; return; end if

  end do ! iVar
 end do ! iFreq

end subroutine writeTime

! ******************************************************************************
! public subroutine setFinalizeStatsFalse: set finalizeStats to false
! ****************************************************************************** 
subroutine setFinalizeStatsFalse(indx_gru) & 
  bind(C, name='setFinalizeStatsFalse')
  USE output_buffer,only:summa_struct
  implicit none
  integer(c_int), intent(in)        :: indx_gru
  integer(i4b)                      :: iStep

  ! set finalizeStats to false
  do iStep=1, size(summa_struct(1)%finalizeStats%gru(indx_gru)%hru(1)%tim)
    summa_struct(1)%finalizeStats%gru(indx_gru)%hru(1)%tim(iStep)%dat = .false.
  end do
end subroutine setFinalizeStatsFalse

end module HRUwriteoOutput_module