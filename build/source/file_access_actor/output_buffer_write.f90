module output_buffer_write
  USE, intrinsic :: iso_c_binding
  ! NetCDF types
  USE netcdf
  USE netcdf_util_module,only:netcdf_err  ! netcdf error handling function
  ! top-level data types
  USE nrtype
  USE globalData,only: integerMissing, realMissing

  USE globalData,only:gru_struc  ! gru->hru mapping structure
  
  USE output_buffer,only:summa_struct
  USE output_buffer,only:outputTimeStep

  USE data_types
  USE actor_data_types
  ! vector lengths
  USE var_lookup, only: maxvarFreq ! number of output frequencies
  USE var_lookup, only: maxvarStat ! number of statistics

  implicit none
  private
  public::f_writeOutputDA
  private::writeParm
  private::writeTime
  private::writeBasin
  private::writeData
  private::writeScalar
  private::writeVector
  
  integer(i4b),parameter  :: maxSpectral=2
  contains

! ******************************************************************************
! public subroutine writeParm: write model parameters
! ******************************************************************************
subroutine f_writeOutputDA(handle_ncid, output_step, start_gru, max_gru, &
    write_parm_flag, err, message_r) bind(C, name="f_writeOutputDA")
  USE var_lookup,only:maxVarFreq
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
  integer(c_int),intent(in)            :: output_step         ! number of steps to write
  integer(c_int),intent(in)            :: start_gru         ! index of GRU we are currently writing for
  integer(c_int),intent(in)            :: max_gru           ! index of HRU we are currently writing for
  logical(c_bool),intent(in)           :: write_parm_flag   ! flag to write parameters
  integer(c_int),intent(out)           :: err               ! Error code
  type(c_ptr),intent(out)              :: message_r ! message to return to the caller
  ! local variables
  type(var_i),pointer                  :: ncid
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
  
  ! Write the Parameters if first write
  if (write_parm_flag) then
    do iStruct=1,size(structInfo)
      do iGRU=start_gru, max_gru
        do iHRU=1,size(gru_struc(iGRU)%hruInfo)
          select case(trim(structInfo(iStruct)%structName))
          case('attr')
            call writeParm(ncid,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix,      &
                           summa_struct(1)%attrStruct%gru(iGRU)%hru(iHRU), &
                           attr_meta,err,cmessage)
          case('type')
            call writeParm(ncid,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix,      &
                           summa_struct(1)%typeStruct%gru(iGRU)%hru(iHRU), &
                           type_meta,err,cmessage)
          case('mpar')
            call writeParm(ncid,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix,      &
                           summa_struct(1)%mparStruct%gru(iGRU)%hru(iHRU), & 
                           mpar_meta,err,cmessage)
          end select
          if(err/=0)then 
            message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
            call f_c_string_ptr(trim(message), message_r)
            return 
          endif
          call writeParm(ncid,iGRU,summa_struct(1)%bparStruct%gru(iGRU), &
                         bpar_meta,err,cmessage)
          if(err/=0)then
            message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']' 
            call f_c_string_ptr(trim(message), message_r)
            return 
          endif
        end do ! HRU
      end do ! GRU
    end do ! structInfo
  end if
  
  ! ****************************************************************************
  ! *** write time data
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
  ! *** write basin data
  ! ****************************************************************************
  call writeBasin(ncid, outputTimeStep(start_gru)%dat(:), output_step, & 
                  start_gru, max_gru, num_gru, bvar_meta, &
                  summa_struct(1)%bvarStat, summa_struct(1)%bvarStruct, &
                  bvarChild_map,err,cmessage)
  if(err/=0)then
    message=trim(message)//trim(cmessage)//'[bvar]' 
    call f_c_string_ptr(trim(message), message_r)
    return 
  endif

  ! ****************************************************************************
  ! *** write data
  ! ****************************************************************************
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),maxLayers,output_step,&
                        start_gru, max_gru, num_gru, & 
                        forc_meta,summa_struct(1)%forcStat,summa_struct(1)%forcStruct,'forc', &
                        forcChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('prog')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),maxLayers,output_step,&
                        start_gru, max_gru, num_gru, &
                        prog_meta,summa_struct(1)%progStat,summa_struct(1)%progStruct,'prog', &
                        progChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('diag')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),maxLayers,output_step,&
                        start_gru, max_gru, num_gru, &
                        diag_meta,summa_struct(1)%diagStat,summa_struct(1)%diagStruct,'diag', &
                        diagChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('flux')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),maxLayers,output_step,&
                        start_gru, max_gru, num_gru, &
                        flux_meta,summa_struct(1)%fluxStat,summa_struct(1)%fluxStruct,'flux', &
                        fluxChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('indx')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),maxLayers,output_step,&
                        start_gru, max_gru, num_gru, &
                        indx_meta,summa_struct(1)%indxStat,summa_struct(1)%indxStruct,'indx', &
                        indxChild_map,summa_struct(1)%indxStruct,err,cmessage)
    end select
    if(err/=0)then
      message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
      call f_c_string_ptr(trim(message), message_r)
      return
    endif
  end do  ! (looping through structures)


  do iFreq = 1,maxvarFreq
    outputTimeStep(start_gru)%dat(iFreq) = outputTimeStep(start_gru)%dat(iFreq) + 1
  end do ! ifreq
end subroutine f_writeOutputDA

! **********************************************************************************************************
! public subroutine writeParm: write model parameters
! **********************************************************************************************************
subroutine writeParm(ncid,ispatial,struct,meta,err,message)
  USE data_types,only:var_info                    ! metadata info
  USE var_lookup,only:iLookStat                   ! index in statistics vector
  USE var_lookup,only:iLookFreq                   ! index in vector of model output frequencies
  USE globalData,only:outputTimeStep              ! vector of model output time steps
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
  err=0;message="writeParm/"
  ! loop through local column model parameters
  do iVar = 1,size(meta)

    ! check that the variable is desired
    if (meta(iVar)%statIndex(iLookFREQ%timestep)==integerMissing) cycle

    ! initialize message
    message=trim(message)//trim(meta(iVar)%varName)//'/'

    ! HRU data
    if (iSpatial/=integerMissing) then
      select type (struct)
        class is (var_i)
          err = nf90_put_var(ncid%var(iLookFreq%timestep),meta(iVar)%ncVarID(iLookFreq%timestep),(/struct%var(iVar)/),start=(/iSpatial/),count=(/1/))
        class is (var_i8)
          err = nf90_put_var(ncid%var(iLookFreq%timestep),meta(iVar)%ncVarID(iLookFreq%timestep),(/struct%var(iVar)/),start=(/iSpatial/),count=(/1/))
        class is (var_d)
          err = nf90_put_var(ncid%var(iLookFreq%timestep),meta(iVar)%ncVarID(iLookFreq%timestep),(/struct%var(iVar)/),start=(/iSpatial/),count=(/1/))
        class is (var_dlength)
          err = nf90_put_var(ncid%var(iLookFreq%timestep),meta(iVar)%ncVarID(iLookFreq%timestep),(/struct%var(iVar)%dat/),start=(/iSpatial,1/),count=(/1,size(struct%var(iVar)%dat)/))
        class default; err=20; message=trim(message)//'unknown variable type (with HRU)'; return
      end select
      call netcdf_err(err,message); if (err/=0) return

      ! GRU data
    else
      select type (struct)
        class is (var_d)
          err = nf90_put_var(ncid%var(iLookFreq%timestep),meta(iVar)%ncVarID(iLookFreq%timestep),(/struct%var(iVar)/),start=(/1/),count=(/1/))
        class is (var_i8)
          err = nf90_put_var(ncid%var(iLookFreq%timestep),meta(iVar)%ncVarID(iLookFreq%timestep),(/struct%var(iVar)/),start=(/1/),count=(/1/))
        class default; err=20; message=trim(message)//'unknown variable type (no HRU)'; return
      end select
    end if
    call netcdf_err(err,message); if (err/=0) return

    ! re-initialize message
    message="writeParm/"
  end do  ! looping through local column model parameters

end subroutine writeParm

! **************************************************************************************
! public subroutine writeTime: write current time to all files
! **************************************************************************************
subroutine writeTime(ncid,outputTimestep,output_step,meta,dat,err,message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:iLookStat                      ! index into stat structure
  implicit none

  ! declare dummy variables
  type(var_i)   ,intent(in)     :: ncid              ! file ids
  integer(i4b)  ,intent(inout)  :: outputTimestep(:) ! output time step
  integer(i4b)  ,intent(in)     :: output_step
  type(var_info),intent(in)     :: meta(:)           ! meta data
  type(time_i)  ,intent(in)     :: dat(:)            ! timestep data
  integer(i4b)  ,intent(out)    :: err               ! error code
  character(*)  ,intent(out)    :: message           ! error message
  ! local variables
  integer(i4b)                  :: iVar              ! variable index
  integer(i4b)                  :: iFreq             ! frequency index
  integer(i4b)                  :: ncVarID           ! used only for time
  ! initialize error control
  err=0;message="f-writeTime/"
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
      if (err/=0) message=trim(message)//trim(meta(iVar)%varName); call netcdf_err(err,message)
      if (err/=0) then; err=20; return; end if

      ! add to file
      err = nf90_put_var(ncid%var(iFreq),ncVarID,(/dat(iVar)%tim(output_step)/),start=(/outputTimestep(iFreq)/),count=(/1/))
      if (err/=0) message=trim(message)//trim(meta(iVar)%varName);call netcdf_err(err,message)
      if (err/=0) then; err=20; return; end if

    end do ! iVar
  end do ! iFreq


end subroutine writeTime 


! **************************************************************************************
! public subroutine writeBasin: write basin-average variables
! **************************************************************************************
subroutine writeBasin(ncid,outputTimestep,output_step,minGRU, maxGRU, numGRU, &
                      meta,stat,dat,map,err,message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:maxVarStat                     ! index into stats structure
  USE var_lookup,only:iLookVarType                   ! index into type structure
  USE globalData,only:outFreq                        ! output file information
  USE get_ixName_module,only:get_varTypeName         ! to access type strings for error messages
  USE get_ixName_module,only:get_statName            ! to access type strings for error messages
  implicit none

  ! declare dummy variables
  type(var_i)   ,intent(in)     :: ncid              ! file ids
  integer(i4b)  ,intent(inout)  :: outputTimestep(:) ! output time step
  integer(i4b)  ,intent(in)     :: output_step            ! number of timeSteps
  integer(i4b)  ,intent(in)     :: minGRU            ! minGRU index to write
  integer(i4b)  ,intent(in)     :: maxGRU            ! maxGRU index to write - probably not needed
  integer(i4b)  ,intent(in)     :: numGRU            ! number of GRUs to write
  type(var_info),intent(in)     :: meta(:)           ! meta data
  class(*)      ,intent(in)     :: stat              ! stats data
  class(*)      ,intent(in)     :: dat               ! timestep data
  integer(i4b)  ,intent(in)     :: map(:)            ! map into stats child struct
  integer(i4b)  ,intent(out)    :: err               ! error code
  character(*)  ,intent(out)    :: message           ! error message
  ! local variables
  integer(i4b)                  :: iVar              ! variable index
  integer(i4b)                  :: iStat             ! statistics index
  integer(i4b)                  :: iFreq             ! frequency index
  integer(i4b)                  :: gru_counter
  integer(i4b)                  :: iGRU
  real(rkind)                   :: realVec(numGRU, 1)! real vector for all HRUs in the run domain

  ! initialize error control
  err=0;message="f-writeBasin/"

  ! loop through output frequencies
  do iFreq=1,maxvarFreq
    ! skip frequencies that are not needed
    if(.not.outFreq(iFreq)) cycle
    ! loop through model variables
    do iVar = 1,size(meta)
      ! define the statistics index
      iStat = meta(iVar)%statIndex(iFreq)
      ! check that the variable is desired
      if (iStat==integerMissing.or.trim(meta(iVar)%varName)=='unknown') cycle
      select case (meta(iVar)%varType)
        case (iLookVarType%scalarv)
          select type (stat)
            class is (gru_hru_time_doubleVec)
              gru_counter = 0
              do iGRU = minGRU, maxGRU
                gru_counter = gru_counter + 1
                if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(1)%tim(output_step)%dat(iFreq)) cycle
                realVec(gru_counter, 1) = stat%gru(iGRU)%hru(1)%var(map(iVar))%tim(output_step)%dat(iFreq)
              end do ! iGRU
              err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),  &
                                 realVec(1:numGRU,1),           &
                                 start=(/minGRU,outputTimestep(iFreq)/),     & 
                                 count=(/numGRU,1/))
            class default; err=20; message=trim(message)//'stats must be scalarv and of type gru_hru_doubleVec'; return
          end select ! stat
        case (iLookVarType%routing)
          select type (dat)
            class is (gru_hru_time_doubleVec)
              if (iFreq==1 .and. outputTimestep(iFreq)==1) then
                do iGRU = minGRU, maxGRU
                  err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq), &
                                     dat%gru(iGRU)%hru(1)%var(iVar)%tim(1)%dat,        &
                                     start=(/1/), count=(/1000/))
                end do
              end if
            class default; err=20; message=trim(message)//'data must not be scalarv and either of type gru_hru_doubleVec or gru_hru_intVec'; return
          end select
        case default
          err=40; message=trim(message)//"unknownVariableType[name='"//trim(meta(iVar)%varName)//"';type='"//trim(get_varTypeName(meta(iVar)%varType))//    "']"; return
      end select ! variable type

      ! process error code
      if (err.ne.0) message=trim(message)//trim(meta(iVar)%varName)//'_'//trim(get_statName(iStat))
      call netcdf_err(err,message); if (err/=0) return
    end do ! iVar
  end do ! iFreq

end subroutine writeBasin


! **************************************************************************************
! public subroutine writeData: write model time-dependent data
! **************************************************************************************
subroutine writeData(ncid,outputTimestep,maxLayers,output_step,&
                     minGRU, maxGRU, numGRU, meta,stat,dat,structName,map,indx,&
                     err,message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:maxVarStat                     ! index into stats structure
  USE var_lookup,only:iLookVarType                   ! index into type structure
  USE var_lookup,only:iLookIndex                     ! index into index structure
  USE var_lookup,only:iLookStat                      ! index into stat structure
  USE globalData,only:outFreq                        ! output file information
  USE get_ixName_module,only:get_varTypeName         ! to access type strings for error messages
  USE get_ixName_module,only:get_statName            ! to access type strings for error messages

  implicit none
  ! declare dummy variables
  type(var_i)   ,intent(in)        :: ncid              ! file ids
  integer(i4b)  ,intent(inout)     :: outputTimestep(:) ! output time step
  integer(i4b)  ,intent(in)        :: maxLayers         ! maximum number of layers
  integer(i4b)  ,intent(in)        :: output_step       ! Current step in output_buffer
  integer(i4b)  ,intent(in)        :: minGRU            ! minGRU index to write
  integer(i4b)  ,intent(in)        :: maxGRU            ! maxGRU index to write - probably not needed
  integer(i4b)  ,intent(in)        :: numGRU            ! number of GRUs to write 
  type(var_info),intent(in)        :: meta(:)           ! meta data
  class(*)      ,intent(in)        :: stat              ! stats data
  class(*)      ,intent(in)        :: dat               ! timestep data
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
  ! output arrays
  real(rkind)                      :: timeVec(1)   ! timeVal to copy
  integer(i4b),parameter           :: ixInteger=1001    ! named variable for integer
  integer(i4b),parameter           :: ixReal=1002       ! named variable for real
  integer(i4b)                     :: iStep
  integer(i4b)                     :: iGRU
  real(rkind)                      :: val
  integer(i4b)                     :: nHRUrun
  ! initialize error control
  err=0;message="writeData/"
  ! loop through output frequencies
  do iFreq=1,maxvarFreq
    ! skip frequencies that are not needed
    if(.not.outFreq(iFreq)) cycle
    ! loop through model variables
    do iVar = 1,size(meta)

      if (meta(iVar)%varName=='time' .and. structName == 'forc')then
        ! get variable index
        err = nf90_inq_varid(ncid%var(iFreq),trim(meta(iVar)%varName),ncVarID)
        call netcdf_err(err,message); if (err/=0) return
        if(.not.summa_struct(1)%finalizeStats%gru(minGRU)%hru(1)%tim(output_step)%dat(iFreq)) cycle
        timeVec(1) = summa_struct(1)%forcStruct%gru(minGRU)%hru(1)%var(iVar)%tim(output_step)
        err = nf90_put_var(ncid%var(iFreq),ncVarID,timeVec(1),start=(/outputTimestep(iFreq)/))
        call netcdf_err(err,message); if (err/=0)then; print*, "err"; return; endif
        cycle
      end if  ! id time

      ! Calculate the number of HRUs to write
      nHRUrun = 0
      do iGRU=minGRU, maxGRU
        nHRUrun = nHRUrun + size(gru_struc(iGRU)%hruInfo)
      end do ! iGRU


      ! define the statistics index
      iStat = meta(iVar)%statIndex(iFreq)
      ! check that the variable is desired
      if (iStat==integerMissing.or.trim(meta(iVar)%varName)=='unknown') cycle

        ! stats output: only scalar variable type
        if(meta(iVar)%varType==iLookVarType%scalarv) then
          call writeScalar(ncid, outputTimeStep, output_step, &
                           minGRU, maxGRU, nHRUrun, iFreq, iVar, meta, stat,   &
                           map, err, message)
        else ! non-scalar variables: regular data structures
          call writeVector(ncid, outputTimeStep, maxLayers, output_step, minGRU, &
                           maxGRU, nHRUrun, iFreq, iVar, meta, dat, indx,   &
                           err, message)
        end if 

      ! process error code
      if (err/=0) message=trim(message)//trim(meta(iVar)%varName)//'_'//trim(get_statName(iStat))
      call netcdf_err(err,message); if (err/=0) return

    end do ! iVar
  end do ! iFreq

end subroutine writeData

subroutine writeScalar(ncid, outputTimestep, output_step, minGRU, maxGRU, &
  nHRUrun, iFreq, iVar, meta, stat, map, err, message)
  USE data_types,only:var_info                       ! metadata type
  USE, intrinsic :: ieee_arithmetic
  implicit none
  ! declare dummy variables
  type(var_i)   ,intent(in)         :: ncid                    ! fileid
  integer(i4b)  ,intent(inout)      :: outputTimestep(:)       ! output time step
  integer(i4b)  ,intent(in)         :: output_step             ! index in output_buffer
  integer(i4b)  ,intent(in)         :: minGRU                  ! minGRU index to write
  integer(i4b)  ,intent(in)         :: maxGRU                  ! maxGRU index to write - probably not needed
  integer(i4b)  ,intent(in)         :: nHRUrun
  integer(i4b)  ,intent(in)         :: iFreq                   ! output file index (year, month, day, timesteps)
  integer(i4b)  ,intent(in)         :: iVar                    ! netcdf variable we are writing data for
  type(var_info),intent(in)         :: meta(:)                 ! meta data
  class(*)      ,intent(in)         :: stat                    ! stats data
  integer(i4b)  ,intent(in)         :: map(:)                  ! map into stats child struct
  integer(i4b)  ,intent(inout)      :: err
  character(*)  ,intent(inout)      :: message

  ! local variables
  integer(i4b)                      :: hru_counter=0
  integer(i4b)                      :: iGRU,iHRU
  ! output array
  real(rkind)                       :: realVec(nHRUrun, 1)! real vector for all HRUs in the run domain
  real(rkind)                       :: val

  err=0; message="writeOutput.f90-writeScalar/"

  select type(stat)
    class is (gru_hru_time_doubleVec)
      hru_counter=0
      do iGRU = minGRU, maxGRU
        do iHRU = 1, size(gru_struc(iGRU)%hruInfo)
          hru_counter = hru_counter + 1
          if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(iHRU)%tim(output_step)%dat(iFreq)) cycle
          realVec(hru_counter, 1) = stat%gru(iGRU)%hru(iHRU)%var(map(iVar))%tim(output_step)%dat(iFreq)
        end do ! iHRU
      end do ! iGRU 

      err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq), &
                         realVec(1:hru_counter, 1),                 &
                         start=(/minGRU,outputTimestep(iFreq)/),    & 
                         count=(/nHRUrun,1/))
    class default; err=20; message=trim(message)//'stats must be scalarv and of type gru_hru_doubleVec'; return
  end select  ! stat

end subroutine writeScalar

subroutine writeVector(ncid, outputTimestep, maxLayers, output_step, minGRU, maxGRU, &
  nHRUrun, iFreq, iVar, meta, dat, indx, err, message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:iLookIndex                     ! index into index structure
  USE var_lookup,only:iLookVarType                   ! index into type structure
  implicit none
  type(var_i)   ,intent(in)             :: ncid                    ! fileid
  integer(i4b)  ,intent(inout)          :: outputTimestep(:)       ! output time step
  integer(i4b)  ,intent(in)             :: maxLayers               ! maximum number of layers
  integer(i4b)  ,intent(in)             :: output_step             ! index in output_buffer  
  integer(i4b)  ,intent(in)             :: minGRU                  ! minGRU index to write
  integer(i4b)  ,intent(in)             :: maxGRU                  ! maxGRU index to write - probably not needed
  integer(i4b)  ,intent(in)             :: nHRUrun
  integer(i4b)  ,intent(in)             :: iFreq                   ! output file index (year, month, day, timesteps)
  integer(i4b)  ,intent(in)             :: iVar                    ! netcdf variable we are writing data for
  type(var_info),intent(in)             :: meta(:)                 ! meta data
  class(*)      ,intent(in)             :: dat               ! timestep data
  type(gru_hru_time_intVec) ,intent(in) :: indx         ! index data
  integer(i4b)  ,intent(inout)          :: err
  character(*)  ,intent(inout)          :: message

  ! local variables
  integer(i4b)                          :: hru_counter
  integer(i4b)                          :: iGRU,iHRU
  integer(i4b)                          :: nSoil
  integer(i4b)                          :: nSnow
  integer(i4b)                          :: nLayers
  ! output array
  integer(i4b)                          :: datLength         ! length of each data vector
  integer(i4b)                          :: maxLength         ! maximum length of each data vector
  integer(i4b)                          :: dataType          ! type of data
  integer(i4b),parameter                :: ixInteger=1001    ! named variable for integer
  integer(i4b),parameter                :: ixReal=1002       ! named variable for real
  real(rkind)                           :: realArray(nHRUrun,maxLayers+1)  ! real array for all HRUs in the run domain
  integer(i4b)                          :: intArray(nHRUrun,maxLayers+1)   ! integer array for all HRUs in the run domain
  err=0; message="writeOutput.f90-writeVector/"

  ! initialize the data vectors
  select type (dat)
    class is (gru_hru_time_doubleVec); realArray(:,:) = realMissing;    dataType=ixReal
    class is (gru_hru_time_intVec);     intArray(:,:) = integerMissing; dataType=ixInteger
    class default; err=20; message=trim(message)//'data must not be scalarv and either of type gru_hru_doubleVec or gru_hru_intVec'; return
  end select
  ! Loop over GRUs
  hru_counter = 1
  do iGRU = minGRU, maxGRU
    do iHRU=1, size(gru_struc(iGRU)%hruInfo)
      ! get the model layers
      nSoil   = indx%gru(iGRU)%hru(iHRU)%var(iLookIndex%nSoil)%tim(output_step)%dat(1)
      nSnow   = indx%gru(iGRU)%hru(iHRU)%var(iLookIndex%nSnow)%tim(output_step)%dat(1)
      nLayers = indx%gru(iGRU)%hru(iHRU)%var(iLookIndex%nLayers)%tim(output_step)%dat(1)

      ! get the length of each data vector
      select case (meta(iVar)%varType)
          case(iLookVarType%wLength); datLength = maxSpectral
          case(iLookVarType%midToto); datLength = nLayers
          case(iLookVarType%midSnow); datLength = nSnow
          case(iLookVarType%midSoil); datLength = nSoil
          case(iLookVarType%ifcToto); datLength = nLayers+1
          case(iLookVarType%ifcSnow); datLength = nSnow+1
          case(iLookVarType%ifcSoil); datLength = nSoil+1
          case default; cycle
      end select ! vartype

      ! get the data vectors
      select type (dat)
          class is (gru_hru_time_doubleVec)
              if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(iHRU)%tim(output_step)%dat(iFreq)) cycle
              realArray(hru_counter,1:datLength) = dat%gru(iGRU)%hru(iHRU)%var(iVar)%tim(output_step)%dat(1:datLength)

          class is (gru_hru_time_intVec)
              if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(iHRU)%tim(output_step)%dat(iFreq)) cycle
              intArray(hru_counter,1:datLength) = dat%gru(iGRU)%hru(iHRU)%var(iVar)%tim(output_step)%dat(1:datLength)
          class default; err=20; message=trim(message)//'data must not be scalarv and either of type gru_hru_doubleVec or gru_hru_intVec'; return
      end select

      ! get the maximum length of each data vector
      select case (meta(iVar)%varType)
        case(iLookVarType%wLength); maxLength = maxSpectral
        case(iLookVarType%midToto); maxLength = maxLayers
        case(iLookVarType%midSnow); maxLength = maxLayers-nSoil
        case(iLookVarType%midSoil); maxLength = nSoil
        case(iLookVarType%ifcToto); maxLength = maxLayers+1
        case(iLookVarType%ifcSnow); maxLength = (maxLayers-nSoil)+1
        case(iLookVarType%ifcSoil); maxLength = nSoil+1
        case default; cycle
      end select ! vartype
      hru_counter = hru_counter + 1
    end do ! iHRU
  end do ! iGRU

  ! write the data vectors
  select case(dataType)

    case(ixReal)
      err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),realArray(1:nHRUrun,1:maxLength),start=(/minGRU,1,outputTimestep(iFreq)/),count=(/nHRUrun,maxLength,1/))
      if(err/=0)then; print*, "ERROR: with nf90_put_var in data vector (ixReal)"; return; endif
      realArray(:,:) = realMissing ! reset the realArray
    case(ixInteger)
      err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),intArray(1:nHRUrun,1:maxLength),start=(/minGRU,1,outputTimestep(iFreq)/),count=(/nHRUrun,maxLength,1/))
      if(err/=0)then; print*, "ERROR: with nf90_put_var in data vector (ixInteger)"; return; endif
      intArray(:,:) = integerMissing ! reset the intArray
    case default; err=20; message=trim(message)//'data must be of type integer or real'; return
  end select ! data type
end subroutine writeVector

end module output_buffer_write