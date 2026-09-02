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

  USE actor_data_types,only:time_i,                    & ! var(:)%tim(:)                            (i4b)
                            gru_hru_time_intVec,       & ! x%gru(:)%hru(:)%var(:)%tim(:)%dat        (i4b)
                            gru_hru_time_doubleVec,    & ! x%gru(:)%hru(:)%var(:)%tim(:)%dat        (dp)
                            gru_hru_dom_time_intVec,   & ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat (i4b)
                            gru_hru_dom_time_doubleVec   ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat (dp)

  ! output constraints
  USE globalData,only:maxLakeLayers       ! maximum number of lake layers
  USE globalData,only:maxGlceLayers       ! maximum number of glacier ice layers
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
  integer(i4b)                         :: iDOM
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
            case('attr'); call writeParam(ncid,0,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix, &
              summa_struct(1)%attrStruct%gru(iGRU)%hru(iHRU),attr_meta,err,cmessage)
            case('type'); call writeParam(ncid,0,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix, &
              summa_struct(1)%typeStruct%gru(iGRU)%hru(iHRU),type_meta,err,cmessage)
            case('mpar')
              do iDOM=1,gru_struc(iGRU)%hruInfo(iHRU)%domCount
                call writeParam(ncid,iDOM,gru_struc(iGRU)%hruInfo(iHRU)%hru_ix, &
                  summa_struct(1)%mparStruct%gru(iGRU)%hru(iHRU)%dom(iDOM),mpar_meta,err,cmessage)
                if(err/=0) exit
              end do
          end select
          if(err/=0)then
            message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
            call f_c_string_ptr(trim(message), message_r)
            return
          endif
        end do ! HRU
        select case(trim(structInfo(iStruct)%structName))
          case('bpar'); call writeParam(ncid,0,iGRU,&
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
  character (len = 15)                 :: output_fileSuffix
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
    write(output_fileSuffix, '(I7.7,"-",I7.7)') start_gru, start_gru + num_gru - 1

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
subroutine writeParam(ncid,iDOM,iSpatial,struct,meta,err,message)
 USE data_types,only:var_info                    ! metadata info
 USE var_lookup,only:iLookStat                   ! index in statistics vector
 USE var_lookup,only:iLookFreq                   ! index in vector of model output frequencies
 implicit none

 ! declare input variables
 type(var_i)   ,intent(in)   :: ncid             ! file ids
 integer(i4b)  ,intent(in)   :: iDOM             ! domain index (0 = no domain dimension)
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
    if (iDOM==0) err = nf90_put_var(ncid%var(iLookFREQ%timestep),meta(iVar)%ncVarID(iLookFREQ%timestep),(/struct%var(iVar)%dat/),start=(/iSpatial,1/),count=(/1,size(struct%var(iVar)%dat)/))
    if (iDOM>0)  err = nf90_put_var(ncid%var(iLookFREQ%timestep),meta(iVar)%ncVarID(iLookFREQ%timestep),(/struct%var(iVar)%dat/),start=(/iDOM,iSpatial,1/),count=(/1,1,size(struct%var(iVar)%dat)/))
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
  type(gru_hru_dom_time_intVec) ,intent(in) :: indx     ! index data
  integer(i4b)  ,intent(out)       :: err               ! error code
  character(*)  ,intent(out)       :: message           ! error message
   ! local variables
  integer(i4b)                     :: iVar              ! variable index
  integer(i4b)                     :: iStat             ! statistics index
  integer(i4b)                     :: iFreq             ! frequency index
  integer(i4b)                     :: iDOM              ! domain index
  integer(i4b)                     :: nDOMwrite         ! number of domain slices to write (maxDOM for dom structs, 1 otherwise)
  logical(lgt)                     :: hasDom            ! .true. if this structure carries a domain dimension
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
  hasDom = (structName=='prog' .or. structName=='diag' .or. structName=='flux' .or. structName=='indx')
  nDOMwrite = 1
  if(hasDom) nDOMwrite = summa_struct(1)%nDOM

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
      do iDOM = 1, nDOMwrite
        if(meta(iVar)%varType==iLookVarType%scalarv) then
          call writeScalar(isBvar, hasDom, iDOM, ncid, outputTimeStep, outputTimeStepUpdate, nSteps, &
                           minGRU, maxGRU, nHRUrun, iFreq, iVar, meta, stat,   &
                           map, err, cmessage)
        else ! non-scalar variables: regular data structures
          call writeVector(isBvar, hasDom, iDOM, ncid, outputTimeStep, maxLengthAll, nSteps, minGRU, &
                           maxGRU, nHRUrun, iFreq, iVar, meta, datt, indx,   &
                           err, cmessage)
        end if ! not scalarv
        if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
      end do ! iDOM

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
subroutine writeScalar(isBvar, hasDom, iDOM, ncid, outputTimestep, outputTimestepUpdate, nSteps, minGRU, maxGRU, &
  nHRUrun, iFreq, iVar, meta, stat, map, err, message)
  USE data_types,only:var_info                       ! metadata type
  USE, intrinsic :: ieee_arithmetic
  implicit none

  ! declare dummy variables
  logical(lgt)  ,intent(in)         :: isBvar                  ! flag to indicate if we are writing bvar data, which has a different structure than the other data structures
  logical(lgt)  ,intent(in)         :: hasDom                  ! .true. if this structure carries a domain dimension
  integer(i4b)  ,intent(in)         :: iDOM                    ! domain index to write (1 if hasDom is .false.)
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
    class is (gru_hru_dom_time_doubleVec)
      ! per-domain structure (prog/diag/flux/indx)
      realVec = realMissing
      nSpace = nHRUrun
      hruCounter = 0
      maxStepCounter = 0
      do iGRU = minGRU, maxGRU
        do iHRU = 1, gru_struc(iGRU)%hruCount
          hruCounter = hruCounter + 1
          if (iDOM > gru_struc(iGRU)%hruInfo(iHRU)%domCount) cycle   ! this HRU does not have this domain
          stepCounter = 0
          do iStep = 1, nSteps
            if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(iHRU)%tim(iStep)%dat(iFreq)) cycle
            stepCounter = stepCounter + 1
            val = stat%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(map(iVar))%tim(iStep)%dat(iFreq)
            if (ieee_is_nan(val)) val = realMissing
            if (val < -1.0e37 .or. val > 1.0e37) val = realMissing
            outputTimeStepUpdate(iFreq) = stepCounter
            realVec(hruCounter, stepCounter) = val
          end do ! iStep
          if (stepCounter .gt. maxStepCounter) maxStepCounter = stepCounter
        end do ! iHRU
      end do ! iGRU
      err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),        &
                             realVec(1:nSpace, 1:maxStepCounter),          &
                             start=(/iDOM,minGRU,outputTimestep(iFreq)/),  &
                             count=(/1,nSpace,maxStepCounter/))

    class is (gru_hru_time_doubleVec)
      ! HRU-level (forc) or GRU-level (bvar) structure
      realVec = realMissing
      nSpace = nHRUrun
      hruCounter = 0
      maxStepCounter = 0
      if(isBvar) nSpace =  maxGRU - minGRU + 1 ! for bvar we have one value per GRU, not one value per HRU

      do iGRU = minGRU, maxGRU
        do iHRU = 1, gru_struc(iGRU)%hruCount
          hruCounter = hruCounter + 1  ! will be iGRU if bvar
          stepCounter = 0
          do iStep = 1, nSteps
            if(.not.summa_struct(1)%finalizeStats%gru(iGRU)%hru(iHRU)%tim(iStep)%dat(iFreq)) cycle
            stepCounter = stepCounter + 1
            val = stat%gru(iGRU)%hru(iHRU)%var(map(iVar))%tim(iStep)%dat(iFreq)
            if (ieee_is_nan(val)) val = realMissing
            if (val < -1.0e37 .or. val > 1.0e37) val = realMissing
            outputTimeStepUpdate(iFreq) = stepCounter
            realVec(hruCounter, stepCounter) = val
          end do ! iStep
          if (stepCounter .gt. maxStepCounter) maxStepCounter = stepCounter
          if(isBvar .and. iHRU==1) exit ! only need to get the GRU-level data once
        end do ! iHRU
      end do ! iGRU

      err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),&
                             realVec(1:nSpace, 1:maxStepCounter),  &
                             start=(/minGRU,outputTimestep(iFreq)/),   &
                             count=(/nSpace,maxStepCounter/))

    class default; err=20; message=trim(message)//'stats must be scalarv and of type gru_hru_[dom_]time_doubleVec'; return
  end select  ! stat type
  if(err/=0)then
    print*, trim(nf90_strerror(err))//" Variable: "//trim(meta(iVar)%varName)
  endif

end subroutine writeScalar

! **********************************************************************************************************
! private subroutine writeVector: write vector variables from data structures 
! **********************************************************************************************************
subroutine writeVector(isBvar, hasDom, iDOM, ncid, outputTimestep, maxLengthAll, nSteps, minGRU, maxGRU, &
  nHRUrun, iFreq, iVar, meta, datt, indx, err, message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:iLookIndex                     ! index into index structure
  USE var_lookup,only:iLookVarType                   ! index into type structure
  implicit none

  ! declare dummy variables
  logical(lgt)  ,intent(in)             :: isBvar            ! flag to indicate if we are writing bvar data, which has a different structure than the other data structures
  logical(lgt)  ,intent(in)             :: hasDom            ! .true. if this structure carries a domain dimension
  integer(i4b)  ,intent(in)             :: iDOM              ! domain index to write (1 if hasDom is .false.)
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
  type(gru_hru_dom_time_intVec) ,intent(in) :: indx          ! index data
  integer(i4b)  ,intent(inout)          :: err
  character(*)  ,intent(inout)          :: message
  ! local variables
  integer(i4b)                          :: hruCounter
  integer(i4b)                          :: iStep             ! counter for looping over nSteps
  integer(i4b)                          :: stepCounter       ! counter for the realVec
  integer(i4b)                          :: iGRU,iHRU
  integer(i4b)                          :: nSnow,nLake,nSoil,nGlce
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
    class is (gru_hru_dom_time_doubleVec); realArray(:,:) = realMissing;    dataType=ixReal
    class is (gru_hru_dom_time_intVec);     intArray(:,:) = integerMissing; dataType=ixInteger
    class is (gru_hru_time_doubleVec);      realArray(:,:) = realMissing;    dataType=ixReal
    class is (gru_hru_time_intVec);         intArray(:,:) = integerMissing;  dataType=ixInteger
    class default; message=trim(message)//'data is not scalarv so should be either of type gru_hru_[dom_]time_[double or int]Vec';err=20; return
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
        if(hasDom .and. iDOM > gru_struc(iGRU)%hruInfo(iHRU)%domCount) cycle  ! this HRU does not have this domain

        ! get the model layers (per domain when hasDom, else no vector index data is needed)
        if(hasDom)then
          nSnow   = indx%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iLookIndex%nSnow)%tim(iStep)%dat(1)
          nLake   = indx%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iLookIndex%nLake)%tim(iStep)%dat(1)
          nSoil   = indx%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iLookIndex%nSoil)%tim(iStep)%dat(1)
          nGlce   = indx%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iLookIndex%nGlce)%tim(iStep)%dat(1)
          nLayers = indx%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iLookIndex%nLayers)%tim(iStep)%dat(1)
        else
          nSnow = 0; nLake = 0; nSoil = 0; nGlce = 0; nLayers = 0
        end if

        select case (meta(iVar)%varType)
          case(iLookVarType%wLength); datLength = nSpecBand
          case(iLookVarType%midToto); datLength = nLayers
          case(iLookVarType%midSnow); datLength = nSnow
          case(iLookVarType%midLake); datLength = nLake
          case(iLookVarType%midSoil); datLength = nSoil
          case(iLookVarType%midGlce); datLength = nGlce
          case(iLookVarType%ifcToto); datLength = nLayers+1
          case(iLookVarType%ifcSnow); datLength = nSnow+1
          case(iLookVarType%ifcLake); datLength = nLake+1
          case(iLookVarType%ifcSoil); datLength = nSoil+1
          case(iLookVarType%ifcGlce); datLength = nGlce+1
          case(iLookVarType%routing); datLength = nTimeDelay
          case default; return ! if not a vector variable type, skip to next variable
        end select ! varType

        ! get the data vectors
        select type (datt)
          class is (gru_hru_dom_time_doubleVec); realArray(hruCounter,1:datLength) = datt%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(iStep)%dat(1:datLength)
          class is (gru_hru_dom_time_intVec);     intArray(hruCounter,1:datLength) = datt%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(iStep)%dat(1:datLength)
          class is (gru_hru_time_doubleVec);     realArray(hruCounter,1:datLength) = datt%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iStep)%dat(1:datLength)
          class is (gru_hru_time_intVec);         intArray(hruCounter,1:datLength) = datt%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iStep)%dat(1:datLength)
        end select
        if (isBvar .and. iHRU == 1)exit ! for bvar we have one value per GRU, not one value per HRU, so only get the data for the first HRU in each GRU

      end do  ! HRU loop
    end do  ! GRU loop

    ! get the maximum length of each data vector
    select case (meta(iVar)%varType)
      case(iLookVarType%wLength); maxLength = nSpecBand
      case(iLookVarType%midToto); maxLength = maxLayers
      case(iLookVarType%midSnow); maxLength = maxSnowLayers
      case(iLookVarType%midLake); maxLength = maxLakeLayers
      case(iLookVarType%midSoil); maxLength = maxSoilLayers
      case(iLookVarType%midGlce); maxLength = maxGlceLayers
      case(iLookVarType%ifcToto); maxLength = maxLayers+1
      case(iLookVarType%ifcSnow); maxLength = maxSnowLayers+1
      case(iLookVarType%ifcLake); maxLength = maxLakeLayers+1
      case(iLookVarType%ifcSoil); maxLength = maxSoilLayers+1
      case(iLookVarType%ifcGlce); maxLength = maxGlceLayers+1
      case(iLookVarType%routing); maxLength = nTimeDelay
      case default; return ! if not a vector variable type, skip to next variable
    end select ! varType

   ! write the data vectors
    if(hasDom)then
      select case(dataType)
        case(ixReal)
          err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),realArray(1:nSpace,1:maxLength),start=(/iDOM,minGRU,1,stepCounter/),count=(/1,nSpace,maxLength,1/))
          realArray(:,:) = realMissing
        case(ixInteger)
          err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),intArray(1:nSpace,1:maxLength),start=(/iDOM,minGRU,1,stepCounter/),count=(/1,nSpace,maxLength,1/))
          intArray(:,:) = integerMissing
      end select
    else
      select case(dataType)
        case(ixReal)
          err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),realArray(1:nSpace,1:maxLength),start=(/minGRU,1,stepCounter/),count=(/nSpace,maxLength,1/))
          realArray(:,:) = realMissing
        case(ixInteger)
          err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),intArray(1:nSpace,1:maxLength),start=(/minGRU,1,stepCounter/),count=(/nSpace,maxLength,1/))
          intArray(:,:) = integerMissing
      end select
    end if
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
                         nGRU,             & ! intent(in): number of GRUs in this batch
                         checkpoint,       & ! intent(in): output-buffer slice to write
                         prog_meta,        & ! intent(in): prognostics metadata
                         prog_data,        & ! intent(in): prognostics data (per gru/hru/dom, time-buffered)
                         bvar_meta,        & ! intent(in): basin (gru) variable metadata
                         bvar_data,        & ! intent(in): basin (gru) variable data (per gru/hru, time-buffered)
                         indx_meta,        & ! intent(in): index metadata
                         indx_data,        & ! intent(in): index data (per gru/hru/dom, time-buffered)
                         err,message)        ! intent(out): error control
 ! --------------------------------------------------------------------------------------------------------
 ! Writes a restart file with the same schema SUMMA's read_icond expects: a `dom` dimension plus a
 ! `domType(dom,hru)` variable, prognostic variables shaped (dom,hru,<layer>), and per-domain layer
 ! counts nSnow/nLake/nSoil/nGlce shaped (dom,hru).  ALL HRUs of each GRU and ALL domains of each HRU
 ! are written.  The `hru` dimension is indexed by hru_nc (the HRU's position in the attributes file),
 ! matching read_icond; positions not in this batch are left as fill and are never read back.
 ! --------------------------------------------------------------------------------------------------------
 USE data_types,only:var_info               ! metadata
 USE var_lookup,only:iLookINDEX             ! named variables for structure elements
 USE var_lookup,only:iLookVarType           ! named variables for structure elements
 USE var_lookup,only:iLookBVAR              ! named variables for structure elements
 USE globalData,only:gru_struc              ! gru-hru-dom mapping structures
 USE globalData,only:maxDOM                 ! maximum number of domains in any HRU
 USE globalData,only:maxGlaciers            ! maximum number of glaciers in any GRU
 USE netcdf_util_module,only:nc_file_close  ! close netcdf file

 implicit none
 ! input
 character(len=256),intent(in)               :: filename      ! name of the restart file
 integer(i4b),intent(in)                     :: nGRU          ! number of GRUs in this batch
 integer(i4b),intent(in)                     :: checkpoint    ! output-buffer slice to write
 type(var_info),intent(in)                   :: prog_meta(:)  ! prognostic variable metadata
 type(gru_hru_dom_time_doubleVec),intent(in) :: prog_data     ! prognostic vars
 type(var_info),intent(in)                   :: bvar_meta(:)  ! basin variable metadata
 type(gru_hru_time_doubleVec),intent(in)     :: bvar_data     ! basin variables
 type(var_info),intent(in)                   :: indx_meta(:)  ! index metadata
 type(gru_hru_dom_time_intVec),intent(in)    :: indx_data     ! index vars
 integer(i4b),intent(out)                    :: err           ! error code
 character(*),intent(out)                    :: message       ! error message
 ! local variables
 integer(i4b)                       :: ncid          ! netcdf file id
 integer(i4b),allocatable           :: ncVarID(:)    ! netcdf variable id (prog + routing + optional glacier bvars)
 integer(i4b)                       :: ncDomTypeID   ! domType variable id
 integer(i4b)                       :: ncGruIdID     ! gruId variable id
 integer(i4b)                       :: ncHruIdID     ! hruId variable id
 integer(i4b)                       :: ncIdxID(4)    ! nSnow/nLake/nSoil/nGlce variable ids
 integer(i4b)                       :: nSnow,nLake,nSoil,nGlce,nLayers
 integer(i4b),parameter             :: nScalar=1     ! size of a scalar
 integer(i4b)                       :: nProgVars     ! number of prognostic variables
 integer(i4b)                       :: hruDimID,gruDimID,domDimID,tdhDimID,nglDimID
 integer(i4b)                       :: scalDimID,specDimID
 integer(i4b)                       :: midSnowDimID,midLakeDimID,midSoilDimID,midGlceDimID,midTotoDimID
 integer(i4b)                       :: ifcSnowDimID,ifcLakeDimID,ifcSoilDimID,ifcGlceDimID,ifcTotoDimID
 integer(i4b)                       :: cHRU          ! HRU position in the restart file (== hru_nc)
 integer(i4b)                       :: hruFileDim    ! length of the hru dimension
 integer(i4b)                       :: nGlac         ! number of glaciers in the GRU
 integer(i4b)                       :: iHRU,iGRU,iDOM,iVar,i
 integer(i4b),dimension(4)          :: nidx          ! index-var lookup ids
 integer(i4b),dimension(7)          :: ngdx          ! glacier bvar lookup ids
 integer(i4b)                       :: size_prog     ! ncVarID slots for prog + routing (+ glacier bvars)
 character(len=256)                 :: cmessage      ! downstream error message
 ! --------------------------------------------------------------------------------------------------------
 err=0; message='writeRestart/'
 nProgVars = size(prog_meta)
 nidx = (/iLookINDEX%nSnow, iLookINDEX%nLake, iLookINDEX%nSoil, iLookINDEX%nGlce/)

 ! compact hru dimension: one slot per HRU in this batch, in iteration order.
 ! gruId/hruId variables are written so read_icond matches by identity, not by slot.
 hruFileDim = sum(gru_struc(1:nGRU)%hruCount)

 size_prog = nProgVars + 1  ! +1 for the routing future runoff variable
 if(maxGlaciers > 0)then
   ngdx = (/iLookBVAR%basin__GlacierStorage, iLookBVAR%updateJulDay, iLookBVAR%glacierAblArea, &
            iLookBVAR%glacierAccArea, iLookBVAR%glacIceRunoffFuture, iLookBVAR%glacSnowRunoffFuture, &
            iLookBVAR%glacFirnRunoffFuture/)
   size_prog = size_prog + size(ngdx)
 endif
 allocate(ncVarID(size_prog))

 ! create file
 err = nf90_create(trim(filename),NF90_NETCDF4,ncid)
 message='iCreate[create]'; call netcdf_err(err,message); if(err/=0)return

 ! define dimensions
                     err = nf90_def_dim(ncid,'gru'    ,nGRU           ,    gruDimID); message='iCreate[gru]'    ; call netcdf_err(err,message); if(err/=0)return
                     err = nf90_def_dim(ncid,'hru'    ,hruFileDim     ,    hruDimID); message='iCreate[hru]'    ; call netcdf_err(err,message); if(err/=0)return
                     err = nf90_def_dim(ncid,'dom'    ,max(maxDOM,1)  ,    domDimID); message='iCreate[dom]'    ; call netcdf_err(err,message); if(err/=0)return
                     err = nf90_def_dim(ncid,'tdh'    ,nTimeDelay     ,    tdhDimID); message='iCreate[tdh]'    ; call netcdf_err(err,message); if(err/=0)return
 if(  maxGlaciers>0) err = nf90_def_dim(ncid,'glac'   ,maxGlaciers    ,    nglDimID); message='iCreate[glac]'   ; call netcdf_err(err,message); if(err/=0)return
                     err = nf90_def_dim(ncid,'scalarv',nScalar        ,   scalDimID); message='iCreate[scalar]' ; call netcdf_err(err,message); if(err/=0)return
                     err = nf90_def_dim(ncid,'spectral',nSpecBand     ,   specDimID); message='iCreate[spec]'   ; call netcdf_err(err,message); if(err/=0)return
                     err = nf90_def_dim(ncid,'midToto',maxLayers      ,midTotoDimID); message='iCreate[midToto]'; call netcdf_err(err,message); if(err/=0)return
                     err = nf90_def_dim(ncid,'ifcToto',maxLayers+1    ,ifcTotoDimID); message='iCreate[ifcToto]'; call netcdf_err(err,message); if(err/=0)return
 if(maxSnowLayers>0) err = nf90_def_dim(ncid,'midSnow',maxSnowLayers  ,midSnowDimID); message='iCreate[midSnow]'; call netcdf_err(err,message); if(err/=0)return
 if(maxSnowLayers>0) err = nf90_def_dim(ncid,'ifcSnow',maxSnowLayers+1,ifcSnowDimID); message='iCreate[ifcSnow]'; call netcdf_err(err,message); if(err/=0)return
 if(maxLakeLayers>0) err = nf90_def_dim(ncid,'midLake',maxLakeLayers  ,midLakeDimID); message='iCreate[midLake]'; call netcdf_err(err,message); if(err/=0)return
 if(maxLakeLayers>0) err = nf90_def_dim(ncid,'ifcLake',maxLakeLayers+1,ifcLakeDimID); message='iCreate[ifcLake]'; call netcdf_err(err,message); if(err/=0)return
 if(maxSoilLayers>0) err = nf90_def_dim(ncid,'midSoil',maxSoilLayers  ,midSoilDimID); message='iCreate[midSoil]'; call netcdf_err(err,message); if(err/=0)return
 if(maxSoilLayers>0) err = nf90_def_dim(ncid,'ifcSoil',maxSoilLayers+1,ifcSoilDimID); message='iCreate[ifcSoil]'; call netcdf_err(err,message); if(err/=0)return
 if(maxGlceLayers>0) err = nf90_def_dim(ncid,'midGlce',maxGlceLayers  ,midGlceDimID); message='iCreate[midGlce]'; call netcdf_err(err,message); if(err/=0)return
 if(maxGlceLayers>0) err = nf90_def_dim(ncid,'ifcGlce',maxGlceLayers+1,ifcGlceDimID); message='iCreate[ifcGlce]'; call netcdf_err(err,message); if(err/=0)return
 err=0; message='writeRestart/'

 ! define id / type variables read by read_icond
 err = nf90_def_var(ncid,'gruId'  ,nf90_int64,(/gruDimID/)          ,ncGruIdID);   message='iCreate[gruId]'  ; call netcdf_err(err,message); if(err/=0)return
 err = nf90_def_var(ncid,'hruId'  ,nf90_int64,(/hruDimID/)          ,ncHruIdID);   message='iCreate[hruId]'  ; call netcdf_err(err,message); if(err/=0)return
 err = nf90_def_var(ncid,'domType',nf90_int  ,(/domDimID,hruDimID/) ,ncDomTypeID); message='iCreate[domType]'; call netcdf_err(err,message); if(err/=0)return

 ! define prognostic variables, shaped (dom,hru,<layer>)
 do iVar = 1,nProgVars
   if (prog_meta(iVar)%varType==iLookVarType%unknown) cycle
   select case(prog_meta(iVar)%varType)
    case(iLookVarType%scalarv);                      err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,  scalDimID /),ncVarID(iVar))
    case(iLookVarType%wLength);                      err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,  specDimID /),ncVarID(iVar))
    case(iLookVarType%midToto);                      err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,midTotoDimID/),ncVarID(iVar))
    case(iLookVarType%ifcToto);                      err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,ifcTotoDimID/),ncVarID(iVar))
    case(iLookVarType%midSnow); if (maxSnowLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,midSnowDimID/),ncVarID(iVar))
    case(iLookVarType%ifcSnow); if (maxSnowLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,ifcSnowDimID/),ncVarID(iVar))
    case(iLookVarType%midLake); if (maxLakeLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,midLakeDimID/),ncVarID(iVar))
    case(iLookVarType%ifcLake); if (maxLakeLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,ifcLakeDimID/),ncVarID(iVar))
    case(iLookVarType%midSoil); if (maxSoilLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,midSoilDimID/),ncVarID(iVar))
    case(iLookVarType%ifcSoil); if (maxSoilLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,ifcSoilDimID/),ncVarID(iVar))
    case(iLookVarType%midGlce); if (maxGlceLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,midGlceDimID/),ncVarID(iVar))
    case(iLookVarType%ifcGlce); if (maxGlceLayers>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varName),nf90_double,(/domDimID,hruDimID,ifcGlceDimID/),ncVarID(iVar))
    case default; cycle
   end select
   if(err/=0)then; message=trim(message)//' [variable '//trim(prog_meta(iVar)%varName)//']'; return; end if
   err = nf90_put_att(ncid,ncVarID(iVar),'long_name',trim(prog_meta(iVar)%vardesc)); call netcdf_err(err,message)
   err = nf90_put_att(ncid,ncVarID(iVar),'units',trim(prog_meta(iVar)%varunit));     call netcdf_err(err,message)
 end do ! iVar

 ! routing future runoff  (gru,tdh)
 err = nf90_def_var(ncid, trim(bvar_meta(iLookBVAR%routingRunoffFuture)%varName), nf90_double, (/gruDimID, tdhDimID /), ncVarID(nProgVars+1))
 if(err/=0)then; message=trim(message)//' [routingRunoffFuture]'; return; end if
 err = nf90_put_att(ncid,ncVarID(nProgVars+1),'long_name',trim(bvar_meta(iLookBVAR%routingRunoffFuture)%vardesc)); call netcdf_err(err,message)
 err = nf90_put_att(ncid,ncVarID(nProgVars+1),'units'    ,trim(bvar_meta(iLookBVAR%routingRunoffFuture)%varunit)); call netcdf_err(err,message)

 ! glacier basin variables
 if(maxGlaciers > 0)then
   do i = 1,size(ngdx)
     iVar = ngdx(i)
     select case(bvar_meta(iVar)%varType)
      case(iLookVarType%scalarv); err = nf90_def_var(ncid,trim(bvar_meta(iVar)%varName),nf90_double,(/gruDimID,scalDimID/),ncVarID(nProgVars+1+i))
      case(iLookVarType%glacier); err = nf90_def_var(ncid,trim(bvar_meta(iVar)%varName),nf90_double,(/gruDimID,nglDimID /),ncVarID(nProgVars+1+i))
      case default; err = nf90_def_var(ncid,trim(bvar_meta(iVar)%varName),nf90_double,(/gruDimID,nglDimID /),ncVarID(nProgVars+1+i))
     end select
     if(err/=0)then; message=trim(message)//' [variable '//trim(bvar_meta(iVar)%varName)//']'; return; end if
     err = nf90_put_att(ncid,ncVarID(nProgVars+1+i),'long_name',trim(bvar_meta(iVar)%vardesc)); call netcdf_err(err,message)
     err = nf90_put_att(ncid,ncVarID(nProgVars+1+i),'units',trim(bvar_meta(iVar)%varunit));     call netcdf_err(err,message)
   end do
 endif

 ! index variables nSnow/nLake/nSoil/nGlce  (dom,hru)
 do i=1,size(nidx)
   iVar = nidx(i)
   err = nf90_def_var(ncid,trim(indx_meta(iVar)%varName),nf90_int,(/domDimID,hruDimID/),ncIdxID(i))
   if(err/=0)then; message=trim(message)//' [variable '//trim(indx_meta(iVar)%varName)//']'; return; end if
   err = nf90_put_att(ncid,ncIdxID(i),'long_name',trim(indx_meta(iVar)%vardesc)); call netcdf_err(err,message)
   err = nf90_put_att(ncid,ncIdxID(i),'units'    ,trim(indx_meta(iVar)%varunit)); call netcdf_err(err,message)
 end do

 err = nf90_enddef(ncid); call netcdf_err(err,message); if (err/=0) return

 ! ----- write data -----
 cHRU = 0
 do iGRU = 1,nGRU

  ! gru identity (so read_icond can match this GRU by id regardless of file position)
  err = nf90_put_var(ncid,ncGruIdID,(/gru_struc(iGRU)%gru_id/),start=(/iGRU/))
  if(err/=0)then; message=trim(message)//'writing gruId'; call netcdf_err(err,message); return; endif

  do iHRU = 1,gru_struc(iGRU)%hruCount
   cHRU = cHRU + 1

   ! hru identity
   err = nf90_put_var(ncid,ncHruIdID,(/gru_struc(iGRU)%hruInfo(iHRU)%hru_id/),start=(/cHRU/))
   if(err/=0)then; message=trim(message)//'writing hruId'; call netcdf_err(err,message); return; endif

   do iDOM = 1,gru_struc(iGRU)%hruInfo(iHRU)%domCount

    err = nf90_put_var(ncid,ncDomTypeID,(/gru_struc(iGRU)%hruInfo(iHRU)%domInfo(iDOM)%dom_type/),start=(/iDOM,cHRU/),count=(/1,1/))
    if(err/=0)then; message=trim(message)//'writing domType'; call netcdf_err(err,message); return; endif

    nSnow = indx_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iLookINDEX%nSnow)%tim(checkpoint)%dat(1)
    nLake = indx_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iLookINDEX%nLake)%tim(checkpoint)%dat(1)
    nSoil = indx_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iLookINDEX%nSoil)%tim(checkpoint)%dat(1)
    nGlce = indx_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iLookINDEX%nGlce)%tim(checkpoint)%dat(1)
    nLayers = nSnow + nLake + nSoil + nGlce

    do iVar = 1,nProgVars
      if (prog_meta(iVar)%varType==iLookVarType%unknown) cycle
      select case (prog_meta(iVar)%varType)
       case(iLookVarType%scalarv);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nScalar  /))
       case(iLookVarType%wlength);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nSpecBand/))
       case(iLookVarType%midToto);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nLayers  /))
       case(iLookVarType%ifcToto);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nLayers+1/))
       case(iLookVarType%midSnow); if (nSnow>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nSnow    /))
       case(iLookVarType%ifcSnow); if (nSnow>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nSnow+1  /))
       case(iLookVarType%midLake); if (nLake>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nLake    /))
       case(iLookVarType%ifcLake); if (nLake>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nLake+1  /))
       case(iLookVarType%midSoil); if (nSoil>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nSoil    /))
       case(iLookVarType%ifcSoil); if (nSoil>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nSoil+1  /))
       case(iLookVarType%midGlce); if (nGlce>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nGlce    /))
       case(iLookVarType%ifcGlce); if (nGlce>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%dom(iDOM)%var(iVar)%tim(checkpoint)%dat/),start=(/iDOM,cHRU,1/),count=(/1,1,nGlce+1  /))
       case default; err=20; message=trim(message)//'unknown var type'; return
      end select
      if (err/=0)then; message=trim(message)//'writing variable:'//trim(prog_meta(iVar)%varName); call netcdf_err(err,message); return; endif
      err=0; message='writeRestart/'
    end do ! iVar

    ! index variables
    err=nf90_put_var(ncid,ncIdxID(1),(/nSnow/),start=(/iDOM,cHRU/),count=(/1,1/)); if(err/=0)goto 900
    err=nf90_put_var(ncid,ncIdxID(2),(/nLake/),start=(/iDOM,cHRU/),count=(/1,1/)); if(err/=0)goto 900
    err=nf90_put_var(ncid,ncIdxID(3),(/nSoil/),start=(/iDOM,cHRU/),count=(/1,1/)); if(err/=0)goto 900
    err=nf90_put_var(ncid,ncIdxID(4),(/nGlce/),start=(/iDOM,cHRU/),count=(/1,1/)); if(err/=0)goto 900

   end do ! iDOM
  end do ! iHRU

  ! basin (GRU) variables
  err=nf90_put_var(ncid,ncVarID(nProgVars+1),(/bvar_data%gru(iGRU)%hru(1)%var(iLookBVAR%routingRunoffFuture)%tim(checkpoint)%dat/),start=(/iGRU,1/),count=(/1,nTimeDelay/))
  if(err/=0)then; message=trim(message)//'writing routingRunoffFuture'; call netcdf_err(err,message); return; endif

  if(maxGlaciers > 0)then
    nGlac = gru_struc(iGRU)%nGlac
    do i=1,size(ngdx)
      iVar = ngdx(i)
      select case(bvar_meta(iVar)%varType)
       case(iLookVarType%scalarv); err=nf90_put_var(ncid,ncVarID(nProgVars+1+i),(/bvar_data%gru(iGRU)%hru(1)%var(iVar)%tim(checkpoint)%dat/),start=(/iGRU,1/),count=(/1,nScalar/))
       case default;               err=nf90_put_var(ncid,ncVarID(nProgVars+1+i),(/bvar_data%gru(iGRU)%hru(1)%var(iVar)%tim(checkpoint)%dat/),start=(/iGRU,1/),count=(/1,max(nGlac,1)/))
      end select
      if(err/=0)then; message=trim(message)//'writing '//trim(bvar_meta(iVar)%varName); call netcdf_err(err,message); return; endif
    end do
  endif

 end do  ! iGRU

 call nc_file_close(ncid,err,cmessage)
 if(err/=0)then;message=trim(message)//trim(cmessage);return;end if
 deallocate(ncVarID)
 return

 900 continue
 message=trim(message)//'writing index variables'; call netcdf_err(err,message)

end subroutine writeRestart


end module fileAccess_writeOutput