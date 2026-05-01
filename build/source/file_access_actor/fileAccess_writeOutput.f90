! SUMMA - Structure for Unifying Multiple Modeling Alternatives
! Copyright (C) 2014-2020 NCAR/RAL; University of Saskatchewan; University of Washington
!
! This file is part of SUMMA
!
! For more information see: http://www.ral.ucar.edu/projects/summa
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module fileAccess_writeOutput
  USE, intrinsic :: iso_c_binding
  ! NetCDF types
  USE netcdf
  USE netcdf_util_module,only:netcdf_err  ! netcdf error handling function
  ! top-level data types
  USE nr_type
  ! missing values
  USE globalData,only: integerMissing, realMissing
  ! output constraints
  USE globalData,only:maxSnowLayers       ! maximum number of snow layers
  USE globalData,only:maxSoilLayers       ! maximum number of soil layers
  USE globalData,only:maxLayers           ! maximum number of layers
  USE globalData,only:nTimeDelay          ! number of timesteps in the time delay histogram
  USE globalData,only:nSpecBand           ! maximum number of spectral bands
  USE globalData,only:allowRoutingOutput  ! flag to allow routing variable output
  ! provide access to global data
  USE globalData,only:gru_struc           ! gru->hru mapping structure
  USE output_buffer,only:summa_struct
  USE output_buffer,only:outputTimeStep
  ! provide access to the derived types to define the data structures
  USE data_types,only:&
                      var_i,            & ! x%var(:)            (i4b)
                      var_i8,           & ! x%var(:)            integer(8)
                      var_d,            & ! x%var(:)            (dp)
                      var_ilength,      & ! x%var(:)%dat        (i4b)
                      var_dlength         ! x%var(:)%dat        (dp)

  USE actor_data_types,only:time_i,                & ! var(:)%tim(:)                     (i4b)
                            gru_hru_time_intVec,   & ! x%gru(:)%hru(:)%var(:)%tim(:)%dat (i4b)
                            gru_hru_time_doubleVec   ! x%gru(:)%hru(:)%var(:)%tim(:)%dat (dp)

  ! vector lengths
  USE var_lookup, only: maxvarFreq ! number of output frequencies
  USE var_lookup, only: maxvarStat ! number of statistics

  implicit none
  public::writeOutput_fortran
  public::writeRestart_fortran
  private::writeParam
  private::writeData
  private::writeForcTime
  private::writeScalar
  private::writeVector
  private::writeTime
  private::writeRestart

  contains

! **********************************************************************************************************
! public subroutine writeOutput_fortran: write model output to file
! **********************************************************************************************************
subroutine writeOutput_fortran(handle_ncid, num_steps, start_gru, max_gru, &
    write_param_flag, err, message_r) bind(C, name="writeOutput_fortran")
  USE var_lookup,only:maxvarFreq                               ! # of available output frequencies
  USE globalData,only:structInfo
  USE globalData,only:bvarChild_map,forcChild_map,progChild_map,diagChild_map,fluxChild_map,indxChild_map             ! index of the child data structure: stats bvar
  USE globalData,only:attr_meta,bvar_meta,type_meta,time_meta,forc_meta,prog_meta,diag_meta,flux_meta,indx_meta,bpar_meta,mpar_meta
  USE C_interface_module,only:f_c_string_ptr
  implicit none

  ! dummy variables
  type(c_ptr),intent(in), value        :: handle_ncid       ! ncid of the output file
  integer(c_int),intent(in)            :: num_steps         ! number of steps to write
  integer(c_int),intent(in)            :: start_gru         ! index of GRU we are currently writing for
  integer(c_int),intent(in)            :: max_gru           ! index of HRU we are currently writing for
  logical(c_bool),intent(in)           :: write_param_flag  ! flag to write parameters
  integer(c_int),intent(out)           :: err               ! Error code
  type(c_ptr),intent(out)              :: message_r         ! message to return to the caller
  ! local variables
  type(var_i),pointer                  :: ncid
  integer(i4b)                         :: maxLengthAll      ! maxLength all data writing
  integer(i4b)                         :: iGRU,iHRU         ! loop through GRUs
  integer(i4b)                         :: iStep             ! loop through time steps
  integer(i4b)                         :: iFreq             ! loop through output frequencies
  integer(i4b)                         :: indxHRU=1         ! index of HRU to write
  integer(i4b), dimension(maxvarFreq)  :: outputTimestepUpdate
  integer(i4b), dimension(maxvarFreq)  :: stepCounter
  character(LEN=256)                   :: message = ""
  character(LEN=256)                   :: cmessage
  integer(i4b)                         :: iStruct
  integer(i4b)                         :: numGRU
  
  ! Change the C pointer to a fortran pointer
  call c_f_pointer(handle_ncid, ncid)
  call f_c_string_ptr(trim(message), message_r)

  ! find longest possible length
  maxLengthAll = max(nSpecBand,maxLayers+1)
  if(allowRoutingOutput) maxLengthAll = max(maxLengthAll, nTimeDelay)
  
  ! Write the Parameters if first write
  if (write_param_flag)then
    do iStruct=1,size(structInfo)
      do iGRU=start_gru, max_gru
        do iHRU=1,size(gru_struc(iGRU)%hruInfo)
          select case(trim(structInfo(iStruct)%structName))
            case('attr'); call writeParam(ncid,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix, &
              summa_struct(1)%attrStruct%gru(iGRU)%hru(iHRU),attr_meta,err,cmessage)
            case('type'); call writeParam(ncid,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix, &
              summa_struct(1)%typeStruct%gru(iGRU)%hru(iHRU),type_meta,err,cmessage)
            case('mpar'); call writeParam(ncid,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix, &
              summa_struct(1)%mparStruct%gru(iGRU)%hru(iHRU),mpar_meta,err,cmessage)
          end select
          if(err/=0)then 
            message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
            call f_c_string_ptr(trim(message), message_r)
            return 
          endif
        end do ! HRU
        select case(trim(structInfo(iStruct)%structName))
          case('bpar'); call writeParam(ncid,iGRU,&
            summa_struct(1)%bparStruct%gru(iGRU),bpar_meta,err,cmessage)
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
    stepCounter(:) = outputTimeStep(iGRU)%dat(:) ! We want to avoid updating outputTimeStep
    do iStep=1, num_steps
      call writeTime(ncid,outputTimeStep(iGRU)%dat(:),iStep,time_meta,  &
                     summa_struct(1)%timeStruct%gru(iGRU)%hru(indxHRU)%var,&
                     err,cmessage)
      if(err/=0)then 
        message=trim(message)//trim(cmessage)//'[time]'
        call f_c_string_ptr(trim(message), message_r)
        return
      endif
    end do ! istep
  end do ! iGRU

  ! ****************************************************************************
  ! *** write data
  ! ****************************************************************************
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc')
        call writeData(.false.,ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLengthAll, &
                        num_steps,start_gru, max_gru, numGRU, & 
                        forc_meta,summa_struct(1)%forcStat,summa_struct(1)%forcStruct,'forc', &
                        forcChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('prog')
        call writeData(.false.,ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLengthAll,&
                        num_steps,start_gru, max_gru, numGRU, &
                        prog_meta,summa_struct(1)%progStat,summa_struct(1)%progStruct,'prog', &
                        progChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('diag')
        call writeData(.false.,ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLengthAll,&
                        num_steps,start_gru, max_gru, numGRU, &
                        diag_meta,summa_struct(1)%diagStat,summa_struct(1)%diagStruct,'diag', &
                        diagChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('flux')
        call writeData(.false.,ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLengthAll,&
                        num_steps,start_gru, max_gru, numGRU, &
                        flux_meta,summa_struct(1)%fluxStat,summa_struct(1)%fluxStruct,'flux', &
                        fluxChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('indx')
        call writeData(.false.,ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLengthAll,&
                        num_steps,start_gru, max_gru, numGRU, &
                        indx_meta,summa_struct(1)%indxStat,summa_struct(1)%indxStruct,'indx', &
                        indxChild_map,summa_struct(1)%indxStruct,err,cmessage)
      case('bvar')
        call writeData(.true.,ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLengthAll,&
                        num_steps,start_gru, max_gru, numGRU, &
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
    outputTimeStep(start_gru)%dat(iFreq) = outputTimeStep(start_gru)%dat(iFreq) + outputTimeStepUpdate(iFreq) 
  end do ! iFreq

end subroutine writeOutput_fortran

! **********************************************************************************************************
! public subroutine writeRestart_fortran: write restart data to the output structure
! **********************************************************************************************************
subroutine writeRestart_fortran(handle_ncid,  start_gru, num_gru, checkpoint, year, month, day, hour, err) bind(C, name="writeRestart_fortran")
  USE var_lookup,only:maxvarFreq                               ! # of available output frequencies
  USE globalData,only:structInfo
  USE globalData,only:bvarChild_map,forcChild_map,progChild_map,diagChild_map,fluxChild_map,indxChild_map             ! index of the child data structure: stats bvar
  USE globalData,only:attr_meta,bvar_meta,type_meta,time_meta,forc_meta,prog_meta,diag_meta,flux_meta,indx_meta,bpar_meta,mpar_meta
  USE summaFileManager,only:OUTPUT_PATH,OUTPUT_PREFIX         ! define output file
  USE summaFileManager,only:STATE_PATH                        ! optional path to state output files (defaults to OUTPUT_PATH)
  implicit none

  ! dummy variables
  type(c_ptr),intent(in), value        :: handle_ncid       ! ncid of the output file
  integer(c_int),intent(in)            :: start_gru         ! index of GRU we are currently writing for
  integer(c_int),intent(in)            :: num_gru           ! index of GRU we are currently writing for
  integer(c_int),intent(in)            :: checkpoint        ! slowest timestep of all grus in job
  integer(c_int),intent(in)            :: year 
  integer(c_int),intent(in)            :: month
  integer(c_int),intent(in)            :: day
  integer(c_int),intent(in)            :: hour
  integer(c_int),intent(out)           :: err               ! Error code
  ! local variables
  type(var_i),pointer                  :: ncid
  integer(i4b)                         :: iGRU              ! loop through GRUs
  integer(i4b)                         :: iStep             ! loop through time steps
  integer(i4b)                         :: iFreq             ! loop through output frequencies
  integer(i4b)                         :: indxHRU=1         ! index of HRU to write
  integer(i4b), dimension(maxvarFreq)  :: outputTimestepUpdate
  integer(i4b), dimension(maxvarFreq)  :: stepCounter
  character(LEN=256)                   :: message
  character(LEN=256)                   :: cmessage
  character (len = 11)                 :: output_fileSuffix
  character(len=256)                   :: restartFile       ! restart file name
  character(len=256)                   :: timeString        ! portion of restart file name that contains the write-out time
  integer(i4b)                         :: restart_flag
  integer(i4b)                         :: iStruct
  integer(i4b)                         :: numGRU
  
  ! Change the C pointer to a fortran pointer
  call c_f_pointer(handle_ncid, ncid)

  ! *****************************************************************************
  ! *** write restart file
  ! *****************************************************************************
  restart_flag = 1 ! temp
  ! print a restart file if requested
  if( restart_flag == 1 )then ! temp bare bones check
    write(timeString,        '(I4.4,I2.2,I2.2,I2.2)') year,month,day,hour
    write(output_fileSuffix, '(I5.5,"-",I5.5)') start_gru, start_gru + num_gru - 1

    if(STATE_PATH == '') then
      restartFile=trim(OUTPUT_PATH)//trim(OUTPUT_PREFIX)//'_restart_'//trim(timeString)//"_G"//output_fileSuffix//'.nc'
    else
      restartFile= trim(STATE_PATH)//trim(OUTPUT_PREFIX)//'_restart_'//trim(timeString)//"_G"//trim(output_fileSuffix)//'.nc'
    endif

    call writeRestart(restartFile,                   &  ! filename
                      num_gru,                       &  ! nHRU
                      checkpoint,                    &  ! checkpoint
                      prog_meta,                     &  ! prog_meta
                      summa_struct(1)%progStruct,    &  ! prog_data
                      bvar_meta,                     &  ! bvar_meta
                      summa_struct(1)%bvarStruct,    &  ! bvar_data
                      indx_meta,                     &  ! indx_meta
                      summa_struct(1)%indxStruct,    &  ! indx_data
                      err,                           &  ! err
                      cmessage)                         ! message 
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  end if

end subroutine writeRestart_fortran


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
subroutine writeData(isBvar, ncid,outputTimestep,outputTimestepUpdate,maxLengthAll, nSteps, &
            minGRU, maxGRU, numGRU, & 
            meta,stat,datt,structName,map,indx,err,message)
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
  logical(lgt)  ,intent(in)        :: isBvar            ! flag to indicate if we are writing bvar data, which has a different structure than the other data structures
  type(var_i)   ,intent(in)        :: ncid              ! file ids
  integer(i4b)  ,intent(inout)     :: outputTimestep(:) ! output time step
  integer(i4b)  ,intent(inout)     :: outputTimestepUpdate(:) ! number of HRUs in the run domain
  integer(i4b)  ,intent(in)        :: maxLengthAll      ! maxLength all data
  integer(i4b)  ,intent(in)        :: nSteps            ! number of timeSteps
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
  real(rkind)                      :: timeVec(nSteps)   ! timeVal to copy
  integer(i4b)                     :: stepCounter       ! counter to know how much data we have to write, needed because we do not always write nSteps
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

    ! loop through model variables
    do iVar = 1,size(meta)
      stepCounter = 0
      ! initialize message
      message="writeData/"//trim(meta(iVar)%varName)

      ! *** write time information
      if (meta(iVar)%varName=='time' .and. structName == 'forc')then
        message=trim(message)//':' ! add statistic (none) to message 
        ! get variable index
        call writeForcTime(ncid, minGRU, maxGRU, outputTimestep, &
                           outputTimestepUpdate, nSteps, iFreq, iVar, meta, &
                           err, cmessage)
        if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
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
        call writeScalar(isBvar, ncid, outputTimeStep, outputTimeStepUpdate, nSteps, &
                         minGRU, maxGRU, nHRUrun, iFreq, iVar, meta, stat,   &
                         map, err, cmessage)
      else ! non-scalar variables: regular data structures
        call writeVector(isBvar, ncid, outputTimeStep, maxLengthAll, nSteps, minGRU, &
                         maxGRU, nHRUrun, iFreq, iVar, meta, datt, indx,   &
                         err, cmessage)
      end if ! not scalarv
      if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

    end do ! iVar
  end do ! iFreq

end subroutine writeData

! **********************************************************************************************************
! private subroutine writeForcTime: Write the time var from the forcStruct
! **********************************************************************************************************
subroutine writeForcTime(ncid, minGRU, maxGRU, outputTimestep, &
    outputTimestepUpdate, nSteps, iFreq, iVar, meta, err, message)
  USE data_types,only:var_info ! metadata type
  implicit none

  ! dummy variables
  type(var_i),   intent(in)        :: ncid
  integer(i4b),  intent(in)        :: minGRU
  integer(i4b),  intent(in)        :: maxGRU
  integer(i4b)  ,intent(inout)     :: outputTimestep(:) 
  integer(i4b)  ,intent(inout)     :: outputTimestepUpdate(:) 
  integer(i4b),  intent(in)        :: nSteps
  integer(i4b),  intent(in)        :: iFreq
  integer(i4b),  intent(in)        :: iVar
  type(var_info),intent(in)        :: meta(:)
  integer(i4b),  intent(out)       :: err
  character(*),  intent(out)       :: message
  ! local variables
  integer(i4b)                     :: iGRU
  integer(i4b)                     :: vGRU ! verified GRU (i.e. not a gru that has failed)
  integer(i4b)                     :: iStep
  integer(i4b)                     :: stepCounter
  real(rkind)                      :: timeVec(nSteps)
  integer(i4b)                     :: ncVarID
  
  message = "writeForcTime/"
  stepCounter = 0
  vGRU = -9999

  do iGRU = minGRU, maxGRU
    if (.not. summa_struct(1)%failedGrus(iGRU)) then
      vGRU = iGRU
      exit
    end if
  end do

  if (vGRU == -9999) then; message = message // " All GRUs have failed"; err = 1; return; end if
  
  err = nf90_inq_varid(ncid%var(iFreq),trim(meta(iVar)%varName),ncVarID)
  call netcdf_err(err,message); if (err/=0) return

  do iStep = 1, nSteps
    if(.not.summa_struct(1)%finalizeStats%gru(vGRU)%hru(1)%tim(iStep)%dat(iFreq)) cycle
    stepCounter = stepCounter+1
    timeVec(stepCounter) = summa_struct(1)%forcStruct%gru(vGRU)%hru(1)%var(iVar)%tim(iStep)
  end do ! iStep

  err = nf90_put_var(ncid%var(iFreq),ncVarID,timeVec(1:stepCounter),start=(/outputTimestep(iFreq)/),count=(/stepCounter/))
  call netcdf_err(err,message); if (err/=0)then; return; endif
  ! save the value of the number of steps to update outputTimestep at the end of the function
  outputTimeStepUpdate(iFreq) = stepCounter

end subroutine writeForcTime

! **********************************************************************************************************
! private subroutine writeScalar: write scalar variables from data structures 
! **********************************************************************************************************
subroutine writeScalar(isBvar, ncid, outputTimestep, outputTimestepUpdate, nSteps, minGRU, maxGRU, &
  nHRUrun, iFreq, iVar, meta, stat, map, err, message)
  USE data_types,only:var_info                       ! metadata type
  USE, intrinsic :: ieee_arithmetic
  implicit none

  ! declare dummy variables
  logical(lgt)  ,intent(in)         :: isBvar                  ! flag to indicate if we are writing bvar data, which has a different structure than the other data structures
  type(var_i)   ,intent(in)         :: ncid                    ! fileid
  integer(i4b)  ,intent(inout)      :: outputTimestep(:)       ! output time step
  integer(i4b)  ,intent(inout)      :: outputTimestepUpdate(:) ! number of HRUs in the run domain
  integer(i4b)  ,intent(in)         :: nSteps                  ! number of timeSteps
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
  integer(i4b)                      :: hruCounter
  integer(i4b)                      :: iStep                   ! counter for looping over nSteps
  integer(i4b)                      :: stepCounter             ! counter for the realVec
  integer(i4b)                      :: maxStepCounter          ! counter for the realVec
  integer(i4b)                      :: iGRU,iHRU
  integer(i4b)                      :: nSpace                  ! number of spatial points to write
  ! output array
  real(rkind)                       :: realVec(nHRUrun, nSteps)! real vector for all HRUs in the run domain
  real(rkind)                       :: val

  err=0; message="writeScalar/"

  select type(stat)
    class is (gru_hru_time_doubleVec)
      ! initialize the data vectors
      realVec = realMissing
      nSpace = nHRUrun
      hruCounter = 0
      maxStepCounter = 0
      if(isBvar) nSpace =  maxGRU - minGRU + 1 ! for bvar we have one value per GRU, not one value per HRU

      ! loop thru GRUs and HRUs and time
      do iGRU = minGRU, maxGRU
        do iHRU = 1, gru_struc(iGRU)%hruCount
          hruCounter = hruCounter + 1  ! will be iGRU if bvar
          stepCounter = 0
          do iStep = 1, nSteps
            if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(iHRU)%tim(iStep)%dat(iFreq)) cycle
            stepCounter = stepCounter + 1 
            val = stat%gru(iGRU)%hru(iHRU)%var(map(iVar))%tim(iStep)%dat(iFreq)
            ! Handle missing values
            if (ieee_is_nan(val)) then
              val = realMissing
            end if
            ! Handle numeric conversion issues
            if (val < -1.0e37 .or. val > 1.0e37) then
              print *, "Warning: Value out of range for NetCDF variable: ", val
              val = realMissing
            end if
            outputTimeStepUpdate(iFreq) = stepCounter
            realVec(hruCounter, stepCounter) = val
          end do ! iStep
          ! We need to output the farthest time step achieved within the batch
          if (stepCounter .gt. maxStepCounter) maxStepCounter = stepCounter
          if(isBvar .and. iHRU==1) exit ! only need to get the GRU-level data once
        end do ! iHRU
      end do ! iGRU
      
      ! write the data vectors
      err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),&
                             realVec(1:nSpace, 1:stepCounter),    &
                             start=(/minGRU,outputTimestep(iFreq)/),   & 
                             count=(/nSpace,maxStepCounter/))
  
      if(err/=0)then
        print*, trim(nf90_strerror(err))
        print *, "Variable: ", trim(meta(iVar)%varName)
        print*,iFreq,meta(iVar)%ncVarID(iFreq),ncid%var(iFreq),minGRU
        print*,outputTimestep(iFreq),stepCounter,nSteps
        print*,size(gru_struc(iGRU)%hruInfo),nHRUrun,nSpace
        ! Print size and mean of realVec
        print *, "Size of realVec: ", size(realVec)
        print *, "Mean of realVec: ", sum(realVec(1:nSpace, 1:stepCounter)) / (nSpace * stepCounter)
      endif
      
    class default; err=20; message=trim(message)//'stats must be scalarv and of type gru_hru_time_doubleVec'; return
  end select  ! stat type

end subroutine writeScalar

! **********************************************************************************************************
! private subroutine writeVector: write vector variables from data structures 
! **********************************************************************************************************
subroutine writeVector(isBvar, ncid, outputTimestep, maxLengthAll, nSteps, minGRU, maxGRU, &
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
  integer(i4b)  ,intent(in)             :: nSteps            ! number of timeSteps
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
  integer(i4b)                          :: iStep             ! counter for looping over nSteps
  integer(i4b)                          :: stepCounter       ! counter for the realVec
  integer(i4b)                          :: iGRU,iHRU
  integer(i4b)                          :: nSoil
  integer(i4b)                          :: nSnow
  integer(i4b)                          :: nLayers
  integer(i4b)                          :: nSpace             ! number of spatial points to write
  ! output array
  integer(i4b)                          :: datLength         ! length of each data vector
  integer(i4b)                          :: maxLength         ! maximum length of each data vector
  integer(i4b)                          :: dataType          ! type of data
  integer(i4b),parameter                :: ixInteger=1001    ! named variable for integer
  integer(i4b),parameter                :: ixReal=1002       ! named variable for real
  real(rkind)                           :: realArray(nHRUrun,maxLengthAll)  ! real array for all HRUs in the run domain
  integer(i4b)                          :: intArray(nHRUrun,maxLengthAll)   ! integer array for all HRUs in the run domain
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

  ! loop time
  stepCounter = outputTimeStep(iFreq)
  do iStep = 1, nSteps
    hruCounter = 0

    ! loop thru GRUs and HRUs
    do iGRU = minGRU, maxGRU
      do iHRU=1,gru_struc(iGRU)%hruCount
        hruCounter = hruCounter + 1  ! will be iGRU if bvar
        if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(iHRU)%tim(iStep)%dat(iFreq)) cycle

        ! get the model layers
        nSoil   = indx%gru(iGRU)%hru(iHRU)%var(iLookIndex%nSoil)%tim(iStep)%dat(1)
        nSnow   = indx%gru(iGRU)%hru(iHRU)%var(iLookIndex%nSnow)%tim(iStep)%dat(1)
        nLayers = indx%gru(iGRU)%hru(iHRU)%var(iLookIndex%nLayers)%tim(iStep)%dat(1)

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
            class is (gru_hru_time_doubleVec); realArray(hruCounter,1:datLength) = datt%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iStep)%dat(1:datLength)
            class is (gru_hru_time_intVec);     intArray(hruCounter,1:datLength) = datt%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iStep)%dat(1:datLength)
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
      case(ixReal)
        err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),realArray(1:nSpace,1:maxLength),start=(/minGRU,1,stepCounter/),count=(/nSpace,maxLength,1/))
        realArray(:,:) = realMissing ! reset the realArray
      case(ixInteger)
        err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),intArray(1:nSpace,1:maxLength),start=(/minGRU,1,stepCounter/),count=(/nSpace,maxLength,1/))
        intArray(:,:) = integerMissing ! reset the intArray
     end select ! data type
    if(err/=0)then; print*, "ERROR: with nf90_put_var in data vector"; return; endif

    stepCounter = stepCounter + 1
  end do ! iStep

end subroutine writeVector

! **************************************************************************************
! public subroutine writeTime: write current time to all files
! **************************************************************************************
subroutine writeTime(ncid,outputTimestep,iStep,meta,datt,err,message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:iLookStat                      ! index into stat structure
  implicit none

  ! declare dummy variables
  type(var_i)   ,intent(in)     :: ncid              ! file ids
  integer(i4b)  ,intent(inout)  :: outputTimestep(:) ! output time step
  integer(i4b)  ,intent(in)     :: iStep
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
    if(.not.summa_struct(1)%finalizeStats%gru(1)%hru(1)%tim(iStep)%dat(iFreq)) cycle

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
      err = nf90_put_var(ncid%var(iFreq),ncVarID,(/datt(iVar)%tim(iStep)/),start=(/outputTimestep(iFreq)/),count=(/1/))
      if (err/=0) message=trim(message)//trim(meta(iVar)%varName)
      call netcdf_err(err,message)
      if (err/=0) then; err=20; return; end if

    end do ! iVar
  end do ! iFreq

end subroutine writeTime   

! *********************************************************************************************************
! public subroutine writeRestart: print a re-start file
! *********************************************************************************************************
subroutine writeRestart(filename,         & ! intent(in): name of restart file
                         nGRU,             & ! intent(in): number of GRUs
                         checkpoint,       &
                         prog_meta,        & ! intent(in): prognostics metadata
                         prog_data,        & ! intent(in): prognostics data
                         bvar_meta,        & ! intent(in): basin (gru) variable metadata
                         bvar_data,        & ! intent(in): basin (gru) variable data
                         indx_meta,        & ! intent(in): index metadata
                         indx_data,        & ! intent(in): index data
                         err,message)        ! intent(out): error control
 ! --------------------------------------------------------------------------------------------------------
 ! --------------------------------------------------------------------------------------------------------
 ! access the derived types to define the data structures
 USE data_types,only:var_info               ! metadata
 ! access named variables defining elements in the data structures
 USE var_lookup,only:iLookINDEX             ! named variables for structure elements
 USE var_lookup,only:iLookVarType           ! named variables for structure elements
 USE var_lookup,only:iLookBVAR              ! named variables for structure elements
 ! constants
 USE globalData,only:gru_struc              ! gru-hru mapping structures
 ! external routines
 USE netcdf_util_module,only:nc_file_close  ! close netcdf file
 USE netcdf_util_module,only:nc_file_open   ! open netcdf file
 USE def_output_module,only: write_hru_info ! write HRU information to netcdf file
 
 implicit none
 ! --------------------------------------------------------------------------------------------------------
 ! input
 character(len=256),intent(in)           :: filename      ! name of the restart file
 integer(i4b),intent(in)                 :: nGRU          ! number of GRUs
 integer(i4b),intent(in)                 :: checkpoint    ! checkpoint the restart file is based on
 type(var_info),intent(in)               :: prog_meta(:)  ! prognostic variable metadata
 type(gru_hru_time_doubleVec),intent(in) :: prog_data     ! prognostic vars
 type(var_info),intent(in)               :: bvar_meta(:)  ! basin variable metadata
 type(gru_hru_time_doubleVec),intent(in) :: bvar_data     ! basin variables
 type(var_info),intent(in)               :: indx_meta(:)  ! metadata
 type(gru_hru_time_intVec),intent(in)    :: indx_data     ! indexing vars
 ! output: error control
 integer(i4b),intent(out)                :: err           ! error code
 character(*),intent(out)                :: message       ! error message
 ! --------------------------------------------------------------------------------------------------------
 ! dummy variables
 
 ! local variables
 integer(i4b)                       :: ncid          ! netcdf file id
 integer(i4b),allocatable           :: ncVarID(:)    ! netcdf variable id
 integer(i4b)                       :: ncSnowID      ! index variable id
 integer(i4b)                       :: ncSoilID      ! index variable id
 integer(i4b)                       :: nSoil         ! number of soil layers
 integer(i4b)                       :: nSnow         ! number of snow layers
 integer(i4b)                       :: nLayers       ! number of total layers
 integer(i4b),parameter             :: nScalar=1     ! size of a scalar
 integer(i4b)                       :: nProgVars     ! number of prognostic variables written to state file
 integer(i4b)                       :: scalar_val
 integer(i4b)                       :: varID
 integer(i4b)                       :: hruDimID      ! variable dimension ID
 integer(i4b)                       :: gruDimID      ! variable dimension ID
 integer(i4b)                       :: tdhDimID      ! variable dimension ID
 integer(i4b)                       :: scalDimID     ! variable dimension ID
 integer(i4b)                       :: specDimID     ! variable dimension ID
 integer(i4b)                       :: midSnowDimID  ! variable dimension ID
 integer(i4b)                       :: midSoilDimID  ! variable dimension ID
 integer(i4b)                       :: midTotoDimID  ! variable dimension ID
 integer(i4b)                       :: ifcSnowDimID  ! variable dimension ID
 integer(i4b)                       :: ifcSoilDimID  ! variable dimension ID
 integer(i4b)                       :: ifcTotoDimID  ! variable dimension ID
 character(len=32),parameter        :: hruDimName    ='hru'      ! dimension name for HRUs
 character(len=32),parameter        :: gruDimName    ='gru'      ! dimension name for GRUs
 character(len=32),parameter        :: tdhDimName    ='tdh'      ! dimension name for time-delay basin variables
 character(len=32),parameter        :: scalDimName   ='scalarv'  ! dimension name for scalar data
 character(len=32),parameter        :: specDimName   ='spectral' ! dimension name for spectral bands
 character(len=32),parameter        :: midSnowDimName='midSnow'  ! dimension name for snow-only layers
 character(len=32),parameter        :: midSoilDimName='midSoil'  ! dimension name for soil-only layers
 character(len=32),parameter        :: midTotoDimName='midToto'  ! dimension name for layered varaiables
 character(len=32),parameter        :: ifcSnowDimName='ifcSnow'  ! dimension name for snow-only layers
 character(len=32),parameter        :: ifcSoilDimName='ifcSoil'  ! dimension name for soil-only layers
 character(len=32),parameter        :: ifcTotoDimName='ifcToto'  ! dimension name for layered variables
 integer(i4b)                       :: cHRU          ! count of HRUs
 integer(i4b)                       :: iHRU          ! index of HRUs
 integer(i4b)                       :: iGRU          ! index of GRUs
 integer(i4b)                       :: iVar          ! variable index
 character(len=256)                 :: cmessage      ! downstream error message
 ! --------------------------------------------------------------------------------------------------------
 
 ! initialize error control
 err=0; message='writeRestart/'
 
 ! size of prognostic variable vector
 nProgVars = size(prog_meta)
 allocate(ncVarID(nProgVars+1))     ! include 1 additional basin variable in ID array (possibly more later)
 
 ! create file
 err = nf90_create(trim(filename),nf90_classic_model,ncid)
 message='iCreate[create]'; call netcdf_err(err,message); if(err/=0)return
 
 ! create file
  err = nf90_create(trim(filename),NF90_NETCDF4,ncid)
  message='iCreate[create]'; call netcdf_err(err,message); if(err/=0)return
 
  ! define dimensions, NOTE, we only write one HRU per GRU for now
                      err = nf90_def_dim(ncid,trim(gruDimName)    ,nGRU             ,    gruDimID); message='iCreate[gru]'     ; call netcdf_err(err,message); if(err/=0)return
                      err = nf90_def_dim(ncid,trim(hruDimName)    ,nGRU             ,    hruDimID); message='iCreate[hru]'     ; call netcdf_err(err,message); if(err/=0)return
                      err = nf90_def_dim(ncid,trim(tdhDimName)    ,nTimeDelay       ,    tdhDimID); message='iCreate[tdh]'     ; call netcdf_err(err,message); if(err/=0)return
                      err = nf90_def_dim(ncid,trim(scalDimName)   ,nScalar          ,   scalDimID); message='iCreate[scalar]'  ; call netcdf_err(err,message); if(err/=0)return
                      err = nf90_def_dim(ncid,trim(specDimName)   ,nSpecBand        ,   specDimID); message='iCreate[spectral]'; call netcdf_err(err,message); if(err/=0)return
                      err = nf90_def_dim(ncid,trim(midTotoDimName),maxLayers        ,midTotoDimID); message='iCreate[midToto]' ; call netcdf_err(err,message); if(err/=0)return
                      err = nf90_def_dim(ncid,trim(ifcTotoDimName),maxLayers+1      ,ifcTotoDimID); message='iCreate[ifcToto]' ; call netcdf_err(err,message); if(err/=0)return
  if(maxSoilLayers>0) err = nf90_def_dim(ncid,trim(midSoilDimName),maxSoilLayers    ,midSoilDimID); message='iCreate[midSoil]' ; call netcdf_err(err,message); if(err/=0)return
  if(maxSoilLayers>0) err = nf90_def_dim(ncid,trim(ifcSoilDimName),maxSoilLayers+1  ,ifcSoilDimID); message='iCreate[ifcSoil]' ; call netcdf_err(err,message); if(err/=0)return
  if(maxSnowLayers>0) err = nf90_def_dim(ncid,trim(midSnowDimName),maxSnowLayers    ,midSnowDimID); message='iCreate[midSnow]' ; call netcdf_err(err,message); if(err/=0)return
  if(maxSnowLayers>0) err = nf90_def_dim(ncid,trim(ifcSnowDimName),maxSnowLayers+1  ,ifcSnowDimID); message='iCreate[ifcSnow]' ; call netcdf_err(err,message); if(err/=0)return
  ! re-initialize error control
  err=0; message='writeRestart/'
 
 ! define prognostic variables
 do iVar = 1,nProgVars
   if (prog_meta(iVar)%varType==iLookvarType%unknown) cycle
 
   ! define variable
   select case(prog_meta(iVar)%varType)
    case(iLookVarType%scalarv);                      err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/hruDimID,  scalDimID /),ncVarID(iVar))
    case(iLookVarType%wLength);                      err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/hruDimID,  specDimID /),ncVarID(iVar))
    case(iLookVarType%midToto);                      err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/hruDimID,midTotoDimID/),ncVarID(iVar))
    case(iLookVarType%ifcToto);                      err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/hruDimID,ifcTotoDimID/),ncVarID(iVar))
    case(iLookVarType%midSoil); if (maxSoilLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/hruDimID,midSoilDimID/),ncVarID(iVar))
    case(iLookVarType%ifcSoil); if (maxSoilLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/hruDimID,ifcSoilDimID/),ncVarID(iVar))
    case(iLookVarType%midSnow); if (maxSnowLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/hruDimID,midSnowDimID/),ncVarID(iVar))
    case(iLookVarType%ifcSnow); if (maxSnowLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/hruDimID,ifcSnowDimID/),ncVarID(iVar))
   end select
 
  ! check errors
   if(err/=0)then
    message=trim(message)//trim(cmessage)//' [variable '//trim(prog_meta(iVar)%varName)//']'
    return
   end if
 
   ! add parameter description
   err = nf90_put_att(ncid,ncVarID(iVar),'long_name',trim(prog_meta(iVar)%vardesc))
   call netcdf_err(err,message)
 
   ! add parameter units
   err = nf90_put_att(ncid,ncVarID(iVar),'units',trim(prog_meta(iVar)%varunit))
   call netcdf_err(err,message)
 
  end do ! iVar
  
  ! define selected basin variables (derived) -- e.g., hillslope routing
  err = nf90_def_var(ncid, trim(bvar_meta(iLookBVAR%routingRunoffFuture)%varName), nf90_double, (/gruDimID, tdhDimID /), ncVarID(nProgVars+1))
  err = nf90_put_att(ncid,ncVarID(nProgVars+1),'long_name',trim(bvar_meta(iLookBVAR%routingRunoffFuture)%vardesc));   call netcdf_err(err,message)
  err = nf90_put_att(ncid,ncVarID(nProgVars+1),'units'    ,trim(bvar_meta(iLookBVAR%routingRunoffFuture)%varunit));   call netcdf_err(err,message)
 
  ! define index variables - snow
  err = nf90_def_var(ncid,trim(indx_meta(iLookINDEX%nSnow)%varName),nf90_int,(/hruDimID/),ncSnowID); call netcdf_err(err,message)
  err = nf90_put_att(ncid,ncSnowID,'long_name',trim(indx_meta(iLookINDEX%nSnow)%vardesc));           call netcdf_err(err,message)
  err = nf90_put_att(ncid,ncSnowID,'units'    ,trim(indx_meta(iLookINDEX%nSnow)%varunit));           call netcdf_err(err,message)
 
  ! define index variables - soil
  err = nf90_def_var(ncid,trim(indx_meta(iLookINDEX%nSoil)%varName),nf90_int,(/hruDimID/),ncSoilID); call netcdf_err(err,message)
  err = nf90_put_att(ncid,ncSoilID,'long_name',trim(indx_meta(iLookINDEX%nSoil)%vardesc));           call netcdf_err(err,message)
  err = nf90_put_att(ncid,ncSoilID,'units'    ,trim(indx_meta(iLookINDEX%nSoil)%varunit));           call netcdf_err(err,message)
 
  ! end definition phase
  err = nf90_enddef(ncid); call netcdf_err(err,message); if (err/=0) return
 
 ! end definition phase
 err = nf90_enddef(ncid); call netcdf_err(err,message); if (err/=0) return
 
 ! write variables
 do iGRU = 1,nGRU
  do iHRU = 1,1 ! NOTE: only writing the first HRU for now since that is the only one we are using for restart, but may need to loop through all HRUs in the future if we want to use more than 1 HRU for restart
   cHRU = iGRU !gru_struc(iGRU)%hruInfo(iHRU)%hru_ix
    do iVar = 1,size(prog_meta)
     ! excape if this variable is not used
     if (prog_meta(iVar)%varType==iLookvarType%unknown) cycle
 
     ! actual number of layers
     nSnow = summa_struct(1)%indxStruct%gru(iGRU)%hru(iHRU)%var(iLookINDEX%nSnow)%tim(checkpoint)%dat(1)
     nSoil = gru_struc(iGRU)%hruInfo(iHRU)%nSoil
     nLayers = nSoil + nSnow
 
     ! write data
     select case (prog_meta(iVar)%varType)
     case(iLookVarType%scalarv);               err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%tim(checkpoint)%dat/),start=(/cHRU,1/),count=(/1,nScalar  /))
     case(iLookVarType%wlength);               err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%tim(checkpoint)%dat/),start=(/cHRU,1/),count=(/1,nSpecBand/))
     case(iLookVarType%midSoil);               err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%tim(checkpoint)%dat/),start=(/cHRU,1/),count=(/1,nSoil    /))
     case(iLookVarType%midToto);               err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%tim(checkpoint)%dat/),start=(/cHRU,1/),count=(/1,nLayers  /))
     case(iLookVarType%ifcSoil);  if (nSoil>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%tim(checkpoint)%dat/),start=(/cHRU,1/),count=(/1,nSoil+1  /))
     case(iLookVarType%ifcToto);  if (nSoil>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%tim(checkpoint)%dat/),start=(/cHRU,1/),count=(/1,nLayers +1/))
     case(iLookVarType%midSnow);  if (nSnow>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%tim(checkpoint)%dat/),start=(/cHRU,1/),count=(/1,nSnow    /))
     case(iLookVarType%ifcSnow);  if (nSnow>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%tim(checkpoint)%dat/),start=(/cHRU,1/),count=(/1,nSnow+1  /))
     case default; err=20; message=trim(message)//'unknown var type'; return
     end select
     
     ! error check
     if (err/=0) message=trim(message)//'writing variable:'//trim(prog_meta(iVar)%varName)
     call netcdf_err(err,message); if (err/=0) return
     err=0; message='writeRestart/'
     
    end do ! iVar loop

    ! write index variables
    err=nf90_put_var(ncid,ncSnowID,(/summa_struct(1)%indxStruct%gru(iGRU)%hru(iHRU)%var(iLookINDEX%nSnow)%tim(checkpoint)%dat(1)/),start=(/cHRU/),count=(/1/))
    err=nf90_put_var(ncid,ncSoilID,(/gru_struc(iGRU)%hruInfo(iHRU)%nSoil/),start=(/cHRU/),count=(/1/))
 
  end do ! iHRU loop
  ! write selected basin variables
  err=nf90_put_var(ncid,ncVarID(nProgVars+1),(/bvar_data%gru(iGRU)%hru(iHRU)%var(iLookBVAR%routingRunoffFuture)%tim(checkpoint)%dat/),  start=(/iGRU/),count=(/1,nTimeDelay/))

 end do  ! iGRU loop

 ! write HRU dimension and ID for file
 call write_hru_info(ncid, gruDimID, hruDimID, err, cmessage); if(err/=0) then; message=trim(message)//trim(cmessage); return; end if

 ! close file
 call nc_file_close(ncid,err,cmessage)
 if(err/=0)then;message=trim(message)//trim(cmessage);return;end if

 ! cleanup
 deallocate(ncVarID)

end subroutine writeRestart


end module fileAccess_writeOutput