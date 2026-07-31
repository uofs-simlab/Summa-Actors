module output_buffer_write
  USE, intrinsic :: iso_c_binding
  ! NetCDF types
  USE netcdf
  USE netcdf_util_module,only:netcdf_err  ! netcdf error handling function
  ! top-level data types
  USE nr_type
  USE globalData,only:integerMissing, realMissing
  USE globalData,only:gru_struc  ! gru->hru mapping structure
  USE output_buffer,only:summa_struct
  USE output_buffer,only:outputTimeStep

  ! provide access to the derived types to define the data structures
  USE data_types,only:&
                      var_i,         & ! x%var(:)                   (i4b)
                      var_i8,        & ! x%var(:)                   integer(8)
                      var_d,         & ! x%var(:)                   (dp)
                      var_ilength,   & ! x%var(:)%dat               (i4b)
                      var_dlength      ! x%var(:)%dat               (dp)         

  USE actor_data_types,only:time_i,                & ! var(:)%tim(:)                     (i4b)
                            gru_hru_time_intVec,   & ! x%gru(:)%hru(:)%var(:)%tim(:)%dat (i4b)
                            gru_hru_time_doubleVec   ! x%gru(:)%hru(:)%var(:)%tim(:)%dat (dp)

  ! vector lengths
  USE var_lookup,only:maxvarFreq ! number of output frequencies
  USE var_lookup,only:maxvarStat ! number of statistics
  ! output constraints
  USE globalData,only:maxSnowLayers       ! maximum number of snow layers
  USE globalData,only:maxSoilLayers       ! maximum number of soil layers
  USE globalData,only:maxLayers           ! maximum number of layers
  USE globalData,only:nTimeDelay          ! number of timesteps in the time delay histogram
  USE globalData,only:nSpecBand           ! maximum number of spectral bands
  USE globalData,only:allowRoutingOutput  ! flag to allow routing variable output

  implicit none
  public::f_writeOutputDA
  private::writeParam
  private::writeData
  private::writeScalar
  private::writeVector
  private::writeTime
  
  contains

! ******************************************************************************
! public subroutine f_writeOutputDA: write model output to file
! ******************************************************************************
subroutine f_writeOutputDA(handle_ncid, output_step, start_gru, max_gru, &
    write_param_flag, err, message_r) bind(C, name="f_writeOutputDA")
  USE var_lookup,only:maxvarFreq
  USE globalData,only:structInfo
  USE globalData,only:bvarChild_map,forcChild_map,progChild_map,diagChild_map,&
      fluxChild_map,indxChild_map             
  USE globalData,only:attr_meta,bvar_meta,type_meta,time_meta,forc_meta,&
      prog_meta,diag_meta,flux_meta,indx_meta,bpar_meta,mpar_meta
  USE globalData,only:maxLayers
  USE C_interface_module,only:f_c_string_ptr
  implicit none

  ! dummy variables
  type(c_ptr),intent(in), value        :: handle_ncid       ! ncid of the output file
  integer(c_int),intent(in)            :: output_step       ! number of steps to write
  integer(c_int),intent(in)            :: start_gru         ! index of GRU we are currently writing for
  integer(c_int),intent(in)            :: max_gru           ! index of HRU we are currently writing for
  logical(c_bool),intent(in)           :: write_param_flag  ! flag to write parameters
  integer(c_int),intent(out)           :: err               ! Error code
  type(c_ptr),intent(out)              :: message_r ! message to return to the caller
  ! local variables
  type(var_i),pointer                  :: ncid
  integer(i4b)                         :: maxLengthAll      ! maxLength all data writing
  integer(i4b)                         :: iGRU,iHRU         ! loop through GRUs
  integer(i4b)                         :: iFreq             ! loop through output frequencies
  integer(i4b)                         :: indxHRU=1         ! index of HRU to write
  character(LEN=256)                   :: message = ""
  character(LEN=256)                   :: cmessage
  integer(i4b)                         :: iStruct
  integer(i4b)                         :: num_gru

  ! Change the C pointer to a fortran pointer
  call c_f_pointer(handle_ncid, ncid)
  call f_c_string_ptr(trim(message), message_r)

  num_gru = max_gru-start_gru + 1
  
  ! find longest possible length
  maxLengthAll = max(nSpecBand,maxLayers+1)
  if(allowRoutingOutput) maxLengthAll = max(maxLengthAll, nTimeDelay)

  ! Write the parameters if first write
  if (write_param_flag) then
    do iStruct=1,size(structInfo)
      do iGRU=start_gru, max_gru
        do iHRU=1,size(gru_struc(iGRU)%hruInfo)
          select case(trim(structInfo(iStruct)%structName))
          case('attr')
            call writeParam(ncid,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix,     &
                           summa_struct(1)%attrStruct%gru(iGRU)%hru(iHRU), &
                           attr_meta,err,cmessage)
          case('type')
            call writeParam(ncid,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix,     &
                           summa_struct(1)%typeStruct%gru(iGRU)%hru(iHRU), &
                           type_meta,err,cmessage)
          case('mpar')
            call writeParam(ncid,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix,     &
                           summa_struct(1)%mparStruct%gru(iGRU)%hru(iHRU), & 
                           mpar_meta,err,cmessage)
          end select
          if(err/=0)then 
            message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
            call f_c_string_ptr(trim(message), message_r)
            return 
          endif
        end do ! HRU
        select case(trim(structInfo(iStruct)%structName))
          case('bpar')
            call writeParam(ncid,iGRU,summa_struct(1)%bparStruct%gru(iGRU), &
                            bpar_meta,err,cmessage)
        end select
        if(err/=0)then
          message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']' 
          call f_c_string_ptr(trim(message), message_r)
          return 
        endif
      end do ! GRU
    end do ! structInfo
  end if
  
  ! ****************************************************************************
  ! *** write time, SUMMA buffered write option turned off as Actors handles buffering
  ! ****************************************************************************
  do iGRU=start_gru, max_gru
    call writeTime(ncid,outputTimeStep(iGRU)%dat(:),output_step,time_meta,  &
                   summa_struct(1)%timeStruct%gru(iGRU)%hru(indxHRU)%var,&
                   err,cmessage)
    if(err/=0)then 
      message=trim(message)//trim(cmessage)//'[time]'
      call f_c_string_ptr(trim(message), message_r)
      return
    endif
  end do ! iGRU

  ! ****************************************************************************
  ! *** write data
  ! ****************************************************************************
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc')
        call writeData(.false., ncid,outputTimeStep(start_gru)%dat(:),maxLengthAll,output_step,&
                        start_gru, max_gru, num_gru, & 
                        forc_meta,summa_struct(1)%forcStat,summa_struct(1)%forcStruct,'forc', &
                        forcChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('prog')
        call writeData(.false., ncid,outputTimeStep(start_gru)%dat(:),maxLengthAll,output_step,&
                        start_gru, max_gru, num_gru, &
                        prog_meta,summa_struct(1)%progStat,summa_struct(1)%progStruct,'prog', &
                        progChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('diag')
        call writeData(.false., ncid,outputTimeStep(start_gru)%dat(:),maxLengthAll,output_step,&
                        start_gru, max_gru, num_gru, &
                        diag_meta,summa_struct(1)%diagStat,summa_struct(1)%diagStruct,'diag', &
                        diagChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('flux')
        call writeData(.false., ncid,outputTimeStep(start_gru)%dat(:),maxLengthAll,output_step,&
                        start_gru, max_gru, num_gru, &
                        flux_meta,summa_struct(1)%fluxStat,summa_struct(1)%fluxStruct,'flux', &
                        fluxChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('indx')
        call writeData(.false., ncid,outputTimeStep(start_gru)%dat(:),maxLengthAll,output_step,&
                        start_gru, max_gru, num_gru, &
                        indx_meta,summa_struct(1)%indxStat,summa_struct(1)%indxStruct,'indx', &
                        indxChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('bvar')
        call writeData(.true., ncid,outputTimeStep(start_gru)%dat(:),maxLengthAll,output_step,&
                        start_gru, max_gru, num_gru, &
                        bvar_meta,summa_struct(1)%bvarStat,summa_struct(1)%bvarStruct,'bvar', &
                        bvarChild_map,summa_struct(1)%indxStruct,err,cmessage)
    end select
    if(err/=0)then
      message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
      call f_c_string_ptr(trim(message), message_r)
      return
    endif
  end do  ! (looping through structures)

  ! *****************************************************************************
  ! *** update counters
  ! *****************************************************************************

  ! increment output file timestep
  do iFreq = 1,maxvarFreq
    outputTimeStep(start_gru)%dat(iFreq) = outputTimeStep(start_gru)%dat(iFreq) + 1
  end do ! iFreq

end subroutine f_writeOutputDA

! **********************************************************************************************************
! private subroutine writeParam: write model parameters
! **********************************************************************************************************
subroutine writeParam(ncid,iSpatial,struct,meta,err,message)
 USE data_types,only:var_info                    ! metadata info
 USE var_lookup,only:iLookStat                   ! index in statistics vector
 USE var_lookup,only:iLookFreq                   ! index in vector of model output frequencies
 implicit none

 ! declare input variables
 type(var_i)   ,intent(in)   :: ncid             ! file ids
 integer(i4b)  ,intent(in)   :: iSpatial         ! hydrologic response unit
 class(*)      ,intent(in)   :: struct           ! data structure
 type(var_info),intent(in)   :: meta(:)          ! metadata structure
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
  message=trim(message)//trim(meta(iVar)%varName)//':'

  select type (struct)
   class is (var_i)
    err = nf90_put_var(ncid%var(iLookFREQ%timestep),meta(iVar)%ncVarID(iLookFREQ%timestep),(/struct%var(iVar)/),start=(/iSpatial/),count=(/1/))
   class is (var_i8)
    err = nf90_put_var(ncid%var(iLookFREQ%timestep),meta(iVar)%ncVarID(iLookFREQ%timestep),(/struct%var(iVar)/),start=(/iSpatial/),count=(/1/))
   class is (var_d)
    err = nf90_put_var(ncid%var(iLookFREQ%timestep),meta(iVar)%ncVarID(iLookFREQ%timestep),(/struct%var(iVar)/),start=(/iSpatial/),count=(/1/))
   class is (var_dlength)
    err = nf90_put_var(ncid%var(iLookFREQ%timestep),meta(iVar)%ncVarID(iLookFREQ%timestep),(/struct%var(iVar)%dat/),start=(/iSpatial,1/),count=(/1,size(struct%var(iVar)%dat)/))
   class default; err=20; message=trim(message)//'parameter type must be var_i, var_i8, var_d, or var_dlength'; return
  end select
  call netcdf_err(err,message); if (err/=0) return

  ! re-initialize message
  message="writeParam/"
 end do  ! looping through local column model parameters

end subroutine writeParam

! **************************************************************************************
! public subroutine writeData: write model time-dependent data
! **************************************************************************************
subroutine writeData(isBvar, ncid,outputTimestep,maxLengthAll,output_step,&
                     minGRU, maxGRU, numGRU, meta,stat,datt,structName,map,indx,&
                     err,message)
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
  logical(lgt),intent(in)          :: isBvar            ! flag for bvar
  type(var_i)   ,intent(in)        :: ncid              ! file ids
  integer(i4b)  ,intent(inout)     :: outputTimestep(:) ! output time step
  integer(i4b)  ,intent(in)        :: maxLengthAll      ! maxLength all data
  integer(i4b)  ,intent(in)        :: output_step       ! Current step in output_buffer
  integer(i4b)  ,intent(in)        :: minGRU            ! minGRU index to write
  integer(i4b)  ,intent(in)        :: maxGRU            ! maxGRU index to write - probably not needed
  integer(i4b)  ,intent(in)        :: numGRU            ! number of GRUs to write 
  type(var_info),intent(in)        :: meta(:)           ! meta data
  class(*)      ,intent(in)        :: stat              ! stats data
  class(*)      ,intent(in)        :: datt              ! timestep data
  character(*)  ,intent(in)        :: structName
  integer(i4b)  ,intent(in)        :: map(:)            ! map into stats child struct
  type(gru_hru_time_intVec) ,intent(in) :: indx         ! index data
  integer(i4b)  ,intent(out)       :: err               ! error code
  character(*)  ,intent(out)       :: message           ! error message
  ! local variables
  integer(i4b)                     :: iVar              ! variable index
  integer(i4b)                     :: iStat             ! statistics index
  integer(i4b)                     :: iFreq             ! frequency index
  integer(i4b)                     :: ncVarID           ! used only for time
  character(LEN=256)               :: cmessage          ! error message of downwind routine
  ! output arrays
  real(rkind)                      :: timeVec(1)        ! timeVal to copy
  integer(i4b)                     :: iStep
  integer(i4b)                     :: iGRU
  real(rkind)                      :: val
  integer(i4b)                     :: nHRUrun

  ! initialize error control
  err=0

  ! loop through output frequencies
  do iFreq=1,maxvarFreq

    ! skip frequencies that are not needed
    if(.not.outFreq(iFreq)) cycle

    ! check that we have finalized statistics for a given frequency
    if(.not.summa_struct(1)%finalizeStats%gru(minGRU)%hru(1)%tim(output_step)%dat(iFreq)) cycle

    ! loop through model variables
    do iVar = 1,size(meta)

      ! initialize message
      message="writeData/"//trim(meta(iVar)%varName)

      ! *** write time information
      if (meta(iVar)%varName=='time' .and. structName == 'forc')then
        message=trim(message)//':' ! add statistic (none) to message 

        ! check that we have finalized statistics for a given frequency
        if(.not.summa_struct(1)%finalizeStats%gru(minGRU)%hru(1)%tim(output_step)%dat(iFreq)) cycle

        ! get variable index
        err = nf90_inq_varid(ncid%var(iFreq),trim(meta(iVar)%varName),ncVarID)
        call netcdf_err(err,message); if (err/=0) return

        ! write time
        timeVec(1) = summa_struct(1)%forcStruct%gru(minGRU)%hru(1)%var(iVar)%tim(output_step)
        err = nf90_put_var(ncid%var(iFreq),ncVarID,timeVec(1),start=(/outputTimestep(iFreq)/))
        call netcdf_err(err,message); if (err/=0)then; return; endif
        cycle
      end if  ! id time

      ! Calculate the number of HRUs to write
      nHRUrun = 0
      do iGRU=minGRU, maxGRU
        nHRUrun = nHRUrun + gru_struc(iGRU)%hruCount
      end do ! iGRU

      ! define the statistics index
      iStat = meta(iVar)%statIndex(iFreq)
      message=trim(message)//'_'//trim(get_statName(iStat))//':' ! add statistic to message

      ! check that the variable is desired, currently do not write large variables (unknown and routing) as they are large and slow things down a lot
      if (iStat==integerMissing .or. meta(iVar)%varType==iLookVarType%unknown .or. meta(iVar)%varType==integerMissing) cycle
      if (meta(iVar)%varType==iLookVarType%routing .and. .not.allowRoutingOutput) cycle ! routing variable write can be turned on with the allowRoutingOutput flag

      ! stats output: only scalar variable type
      if(meta(iVar)%varType==iLookVarType%scalarv) then
        call writeScalar(isBvar, ncid, outputTimeStep, output_step, &
                         minGRU, maxGRU, nHRUrun, iFreq, iVar, meta, stat,   &
                         map, err, cmessage)
      else ! non-scalar variables: regular data structures
        call writeVector(isBvar, ncid, outputTimeStep, maxLengthAll, output_step, minGRU, &
                         maxGRU, nHRUrun, iFreq, iVar, meta, datt, indx,   &
                         err, cmessage)
      end if 
      if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

    end do ! iVar
  end do ! iFreq

end subroutine writeData

! **********************************************************************************************************
! private subroutine writeScalar: write scalar variables from data structures 
! **********************************************************************************************************
subroutine writeScalar(isBvar, ncid, outputTimestep, output_step, minGRU, maxGRU, &
  nHRUrun, iFreq, iVar, meta, stat, map, err, message)
  USE data_types,only:var_info                       ! metadata type
  implicit none

  ! declare dummy variables
  logical(lgt)  ,intent(in)         :: isBvar              ! flag to indicate if we are writing bvar data, which has a different structure than the other data structures
  type(var_i)   ,intent(in)         :: ncid                ! fileid
  integer(i4b)  ,intent(inout)      :: outputTimestep(:)   ! output time step
  integer(i4b)  ,intent(in)         :: output_step         ! index in output_buffer
  integer(i4b)  ,intent(in)         :: minGRU              ! minGRU index to write
  integer(i4b)  ,intent(in)         :: maxGRU              ! maxGRU index to write - probably not needed
  integer(i4b)  ,intent(in)         :: nHRUrun
  integer(i4b)  ,intent(in)         :: iFreq               ! output file index (year, month, day, timesteps)
  integer(i4b)  ,intent(in)         :: iVar                ! netcdf variable we are writing data for
  type(var_info),intent(in)         :: meta(:)             ! meta data
  class(*)      ,intent(in)         :: stat                ! stats data
  integer(i4b)  ,intent(in)         :: map(:)              ! map into stats child struct
  integer(i4b)  ,intent(inout)      :: err
  character(*)  ,intent(inout)      :: message
  ! local variables
  integer(i4b)                      :: hruCounter
  integer(i4b)                      :: iGRU,iHRU
  integer(i4b)                      :: nSpace              ! number of spatial points to write
  ! output array
  real(rkind)                       :: realVec(nHRUrun, 1) ! real vector max size for all HRUs in the run domain

  err=0; message="writeScalar/"

  select type(stat)
    class is (gru_hru_time_doubleVec)
      ! initialize the data vectors
      realVec = realMissing
      nSpace = nHRUrun
      hruCounter = 0
      if(isBvar) nSpace =  maxGRU - minGRU + 1 ! for bvar we have one value per GRU, not one value per HRU

      ! loop thru GRUs and HRUs
      do iGRU = minGRU, maxGRU
        do iHRU = 1, gru_struc(iGRU)%hruCount
          hruCounter = hruCounter + 1  ! will be iGRU if bvar
          if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(iHRU)%tim(output_step)%dat(iFreq)) cycle
          realVec(hruCounter, 1) = stat%gru(iGRU)%hru(iHRU)%var(map(iVar))%tim(output_step)%dat(iFreq)
          if(isBvar .and. iHRU==1) exit ! only need to get the GRU-level data once
        end do ! iHRU
      end do ! iGRU   

      ! write the data vectors
      err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq), &
                              realVec(1:nSpace, 1),                        &
                              start=(/minGRU,outputTimestep(iFreq)/),      & 
                              count=(/nSpace,1/))

    class default; err=20; message=trim(message)//'stats must be scalarv and of type gru_hru_time_doubleVec'; return
  end select  ! stat type

end subroutine writeScalar

! **********************************************************************************************************
! private subroutine writeVector: write vector variables from data structures 
! **********************************************************************************************************
subroutine writeVector(isBvar, ncid, outputTimestep, maxLengthAll, output_step, minGRU, maxGRU, &
  nHRUrun, iFreq, iVar, meta, datt, indx, err, message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:iLookIndex                     ! index into index structure
  USE var_lookup,only:iLookVarType                   ! index into type structure
  implicit none

  ! declare dummy variables
  logical(lgt)  ,intent(in)             :: isBvar            ! flag to indicate if we are writing bvar data, which has a different structure than the other data structures
  type(var_i)   ,intent(in)             :: ncid              ! fileid
  integer(i4b)  ,intent(inout)          :: outputTimestep(:) ! output time step
  integer(i4b)  ,intent(in)             :: maxLengthAll      ! maxLength all data
  integer(i4b)  ,intent(in)             :: output_step       ! index in output_buffer  
  integer(i4b)  ,intent(in)             :: minGRU            ! minGRU index to write
  integer(i4b)  ,intent(in)             :: maxGRU            ! maxGRU index to write - probably not needed
  integer(i4b)  ,intent(in)             :: nHRUrun
  integer(i4b)  ,intent(in)             :: iFreq             ! output file index (year, month, day, timesteps)
  integer(i4b)  ,intent(in)             :: iVar              ! netcdf variable we are writing data for
  type(var_info),intent(in)             :: meta(:)           ! meta data
  class(*)      ,intent(in)             :: datt              ! timestep data
  type(gru_hru_time_intVec) ,intent(in) :: indx              ! index data
  integer(i4b)  ,intent(inout)          :: err
  character(*)  ,intent(inout)          :: message
  ! local variables
  integer(i4b)                          :: hruCounter
  integer(i4b)                          :: iGRU,iHRU
  integer(i4b)                          :: nSoil
  integer(i4b)                          :: nSnow
  integer(i4b)                          :: nLayers
  integer(i4b)                          :: nSpace            ! number of spatial points to write
  ! output array
  integer(i4b)                          :: datLength         ! length of each data vector
  integer(i4b)                          :: maxLength         ! maximum length of each data vector
  integer(i4b)                          :: dataType          ! type of data
  integer(i4b),parameter                :: ixInteger=1001    ! named variable for integer
  integer(i4b),parameter                :: ixReal=1002       ! named variable for real
  real(rkind)                           :: realArray(nHRUrun,maxLengthAll) ! real array for all HRUs in the run domain
  integer(i4b)                          :: intArray(nHRUrun,maxLengthAll)  ! integer array for all HRUs in the run domain
  err=0; message="writeVector/"

  ! set the number of spatial points to write
  nSpace = nHRUrun
  if(isBvar) nSpace = maxGRU - minGRU + 1 ! for bvar we have one value per GRU, not one value per HRU

  ! initialize the data vectors
  select type (datt)
    class is (gru_hru_time_doubleVec); realArray(:,:) = realMissing;    dataType=ixReal
    class is (gru_hru_time_intVec);     intArray(:,:) = integerMissing; dataType=ixInteger
    class default; message=trim(message)//'data is not scalarv so should be either of type gru_hru_time_[double or int]Vec';err=20; return
  end select

  ! loop thru GRUs and HRUs
  hruCounter = 0
  do iGRU = minGRU, maxGRU
    do iHRU=1,gru_struc(iGRU)%hruCount
      hruCounter = hruCounter + 1  ! will be iGRU if bvar
      if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(iHRU)%tim(output_step)%dat(iFreq)) cycle

      ! get the model layers
      nSoil   = indx%gru(iGRU)%hru(iHRU)%var(iLookIndex%nSoil)%tim(output_step)%dat(1)
      nSnow   = indx%gru(iGRU)%hru(iHRU)%var(iLookIndex%nSnow)%tim(output_step)%dat(1)
      nLayers = indx%gru(iGRU)%hru(iHRU)%var(iLookIndex%nLayers)%tim(output_step)%dat(1)

      select case (meta(iVar)%varType)
        case(iLookVarType%wLength); datLength = nSpecBand
        case(iLookVarType%midToto); datLength = nLayers
        case(iLookVarType%midSnow); datLength = nSnow
        case(iLookVarType%midSoil); datLength = nSoil
        case(iLookVarType%ifcToto); datLength = nLayers+1
        case(iLookVarType%ifcSnow); datLength = nSnow+1
        case(iLookVarType%ifcSoil); datLength = nSoil+1
        case(iLookVarType%routing); datLength = nTimeDelay
        case default; return ! if not a vector variable type, skip to next variable
      end select ! varType

      ! get the data vectors
      select type (datt)
        class is (gru_hru_time_doubleVec); realArray(hruCounter,1:datLength) = datt%gru(iGRU)%hru(iHRU)%var(iVar)%tim(output_step)%dat(1:datLength)
        class is (gru_hru_time_intVec);     intArray(hruCounter,1:datLength) = datt%gru(iGRU)%hru(iHRU)%var(iVar)%tim(output_step)%dat(1:datLength)
      end select
      if (isBvar .and. iHRU == 1)exit ! for bvar we have one value per GRU, not one value per HRU, so only get the data for the first HRU in each GRU

    end do  ! HRU loop
   end do  ! GRU loop

   ! get the maximum length of each data vector
   select case (meta(iVar)%varType)
     case(iLookVarType%wLength); maxLength = nSpecBand
     case(iLookVarType%midToto); maxLength = maxLayers
     case(iLookVarType%midSnow); maxLength = maxSnowLayers
     case(iLookVarType%midSoil); maxLength = maxSoilLayers
     case(iLookVarType%ifcToto); maxLength = maxLayers+1
     case(iLookVarType%ifcSnow); maxLength = maxSnowLayers+1
     case(iLookVarType%ifcSoil); maxLength = maxSoilLayers+1
     case(iLookVarType%routing); maxLength = nTimeDelay
     case default; return ! if not a vector variable type, skip to next variable
   end select ! varType

  ! write the data vectors
  select case(dataType)
    case(ixReal);    err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),realArray(1:nSpace,1:maxLength),start=(/minGRU,1,outputTimestep(iFreq)/),count=(/nSpace,maxLength,1/))
    case(ixInteger); err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),intArray(1:nSpace,1:maxLength),start=(/minGRU,1,outputTimestep(iFreq)/),count=(/nSpace,maxLength,1/))
  end select ! data type
  if(err/=0)then; print*, "ERROR: with nf90_put_var in data vector"; return; endif
  
end subroutine writeVector

! **************************************************************************************
! public subroutine writeTime: write current time to all files
! **************************************************************************************
subroutine writeTime(ncid,outputTimestep,output_step,meta,datt,err,message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:iLookStat                      ! index into stat structure
  implicit none

  ! declare dummy variables
  type(var_i)   ,intent(in)     :: ncid              ! file ids
  integer(i4b)  ,intent(inout)  :: outputTimestep(:) ! output time step
  integer(i4b)  ,intent(in)     :: output_step
  type(var_info),intent(in)     :: meta(:)           ! meta data
  type(time_i)  ,intent(in)     :: datt(:)           ! timestep data
  integer(i4b)  ,intent(out)    :: err               ! error code
  character(*)  ,intent(out)    :: message           ! error message
  ! local variables
  integer(i4b)                  :: iVar              ! variable index
  integer(i4b)                  :: iFreq             ! frequency index
  integer(i4b)                  :: ncVarID           ! used only for time
 
  ! initialize error control
  err=0;message="writeTime/"
  ! loop through output frequencies
  do iFreq=1,maxvarFreq

    ! check that we have finalized statistics for a given frequency
    if(.not.summa_struct(1)%finalizeStats%gru(1)%hru(1)%tim(output_step)%dat(iFreq)) cycle

    ! loop through model variables
    do iVar = 1,size(meta)

      ! check instantaneous
      if (meta(iVar)%statIndex(iFreq)/=iLookStat%inst) cycle
      ! get variable id in file
      err = nf90_inq_varid(ncid%var(iFreq),trim(meta(iVar)%varName),ncVarID)
      if (err/=0) message=trim(message)//trim(meta(iVar)%varName)
      call netcdf_err(err,message)
      if (err/=0) then; err=20; return; end if

      ! add to file
      err = nf90_put_var(ncid%var(iFreq),ncVarID,(/datt(iVar)%tim(output_step)/),start=(/outputTimestep(iFreq)/),count=(/1/))
      if (err/=0) message=trim(message)//trim(meta(iVar)%varName)
      call netcdf_err(err,message)
      if (err/=0) then; err=20; return; end if

    end do ! iVar
  end do ! iFreq

end subroutine writeTime 

end module output_buffer_write