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
USE netcdf_util_module,only:netcdf_err                    ! netcdf error handling function

! top-level data types
USE nrtype

! missing values
USE globalData,only: integerMissing, realMissing

! provide access to global data
USE globalData,only:gru_struc                             ! gru->hru mapping structure

USE output_structure_module,only:outputStructure
USE output_structure_module,only:outputTimeStep


! provide access to the derived types to define the data structures
USE data_types,only:&
                    ! final data vectors
                    dlength,             & ! var%dat
                    ilength,             & ! var%dat
                    ! no spatial dimension
                    var_i,               & ! x%var(:)            (i4b)
                    var_i8,              & ! x%var(:)            integer(8)
                    var_d,               & ! x%var(:)            (dp)
                    var_ilength,         & ! x%var(:)%dat        (i4b)
                    var_dlength,         & ! x%var(:)%dat        (dp)
                    ! no variable dimension
                    hru_i,               & ! x%hru(:)            (i4b)
                    hru_d,               & ! x%hru(:)            (dp)
                    ! gru dimension
                    gru_int,             & ! x%gru(:)%var(:)     (i4b)
                    gru_double,          & ! x%gru(:)%var(:)     (dp)
                    gru_intVec,          & ! x%gru(:)%var(:)%dat (i4b)
                    gru_doubleVec,       & ! x%gru(:)%var(:)%dat (dp)
                    ! gru+hru dimension
                    gru_hru_int,         & ! x%gru(:)%hru(:)%var(:)     (i4b)
                    gru_hru_int8,        & ! x%gru(:)%hru(:)%var(:)     integer(8)
                    gru_hru_double,      & ! x%gru(:)%hru(:)%var(:)     (dp)
                    gru_hru_intVec,      & ! x%gru(:)%hru(:)%var(:)%dat (i4b)
                    gru_hru_doubleVec      ! x%gru(:)%hru(:)%var(:)%dat (dp)
    
USE actor_data_types,only:&
                    time_dlength,          & ! var(:)%tim(:)%dat (dp)
                    time_i,                &
                    gru_hru_time_double,   &
                    gru_hru_time_doubleVec,&
                    gru_hru_time_intVec

! vector lengths
USE var_lookup, only: maxvarFreq ! number of output frequencies
USE var_lookup, only: maxvarStat ! number of statistics

implicit none
private
public::writeOutput_fortran
public::writeParm
public::writeData
public::writeBasin
public::writeTime
private::writeScalar
private::writeVector
! define dimension lengths
integer(i4b),parameter      :: maxSpectral=2              ! maximum number of spectral bands
contains

! **********************************************************************************************************
! public subroutine writeParm: write model parameters
! **********************************************************************************************************
subroutine writeOutput_fortran(handle_ncid, num_steps, start_gru, max_gru, write_parm_flag, err) bind(C, name="writeOutput_fortran")
  USE var_lookup,only:maxVarFreq                               ! # of available output frequencies
  USE globalData,only:structInfo
  USE globalData,only:bvarChild_map,forcChild_map,progChild_map,diagChild_map,fluxChild_map,indxChild_map             ! index of the child data structure: stats bvar
  USE globalData,only:attr_meta,bvar_meta,type_meta,time_meta,forc_meta,prog_meta,diag_meta,flux_meta,indx_meta,bpar_meta,mpar_meta
  USE globalData,only:maxLayers
  implicit none
  ! dummy variables
  type(c_ptr),intent(in), value        :: handle_ncid       ! ncid of the output file
  integer(c_int),intent(in)            :: num_steps         ! number of steps to write
  integer(c_int),intent(in)            :: start_gru         ! index of GRU we are currently writing for
  integer(c_int),intent(in)            :: max_gru           ! index of HRU we are currently writing for
  logical(c_bool),intent(in)           :: write_parm_flag   ! flag to write parameters
  integer(c_int),intent(out)           :: err               ! Error code
  ! local variables
  type(var_i),pointer                  :: ncid
  integer(i4b)                         :: iGRU              ! loop through GRUs
  integer(i4b)                         :: iStep             ! loop through time steps
  integer(i4b)                         :: iFreq             ! loop through output frequencies
  integer(i4b)                         :: indxHRU=1         ! index of HRU to write
  integer(i4b), dimension(maxVarFreq)  :: outputTimestepUpdate
  integer(i4b), dimension(maxVarFreq)  :: stepCounter
  character(LEN=256)                   :: message
  character(LEN=256)                   :: cmessage
  integer(i4b)                         :: iStruct
  integer(i4b)                         :: numGRU
  
  ! Change the C pointer to a fortran pointer
  call c_f_pointer(handle_ncid, ncid)
  
  ! Write the Parameters if first write
  if (write_parm_flag)then
    do iStruct=1,size(structInfo)
      do iGRU=start_gru, max_gru
        select case(trim(structInfo(iStruct)%structName))
        case('attr'); call writeParm(ncid,gru_struc(iGRU)%hruInfo(indxHRU)%hru_ix, &
          outputStructure(1)%attrStruct%gru(iGRU)%hru(indxHRU),attr_meta,err,cmessage)
        case('type'); call writeParm(ncid,gru_struc(iGRU)%hruInfo(indxHRU)%hru_ix, &
          outputStructure(1)%typeStruct%gru(iGRU)%hru(indxHRU),type_meta,err,cmessage)
        case('mpar'); call writeParm(ncid,gru_struc(iGRU)%hruInfo(indxHRU)%hru_ix, &
          outputStructure(1)%mparStruct%gru(iGRU)%hru(indxHRU),mpar_meta,err,cmessage)
        end select
        if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
        call writeParm(ncid,iGRU,outputStructure(1)%bparStruct%gru(iGRU),bpar_meta,err,cmessage)
        if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
      end do ! GRU
    end do ! structInfo
  end if
  



  ! ****************************************************************************
  ! *** write basin data
  ! ****************************************************************************
  do iGRU=start_gru, max_gru
    stepCounter(:) = outputTimeStep(iGRU)%dat(:) ! We want to avoid updating outputTimeStep
    do iStep=1, num_steps
      call writeTime(ncid,outputTimeStep(iGRU)%dat(:),iStep,time_meta, &
              outputStructure(1)%timeStruct%gru(iGRU)%hru(indxHRU)%var,err,cmessage)
    end do ! istep
  end do ! iGRU


  numGRU = max_gru-start_gru + 1
  ! ****************************************************************************
  ! *** write basin data
  ! ****************************************************************************
  call writeBasin(ncid,outputTimeStep(start_gru)%dat(:),outputTimeStepUpdate,num_steps,&
                  start_gru, max_gru, numGRU, &
                  bvar_meta,outputStructure(1)%bvarStat,outputStructure(1)%bvarStruct, &
                  bvarChild_map,err,cmessage)

  ! ****************************************************************************
  ! *** write data
  ! ****************************************************************************
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLayers,num_steps,&
                        start_gru, max_gru, numGRU, & 
                        forc_meta,outputStructure(1)%forcStat,outputStructure(1)%forcStruct,'forc', &
                        forcChild_map,outputStructure(1)%indxStruct,err,cmessage)
      case('prog')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLayers,num_steps,&
                        start_gru, max_gru, numGRU, &
                        prog_meta,outputStructure(1)%progStat,outputStructure(1)%progStruct,'prog', &
                        progChild_map,outputStructure(1)%indxStruct,err,cmessage)
      case('diag')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLayers,num_steps,&
                        start_gru, max_gru, numGRU, &
                        diag_meta,outputStructure(1)%diagStat,outputStructure(1)%diagStruct,'diag', &
                        diagChild_map,outputStructure(1)%indxStruct,err,cmessage)
      case('flux')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLayers,num_steps,&
                        start_gru, max_gru, numGRU, &
                        flux_meta,outputStructure(1)%fluxStat,outputStructure(1)%fluxStruct,'flux', &
                        fluxChild_map,outputStructure(1)%indxStruct,err,cmessage)
      case('indx')
        call writeData(ncid,outputTimeStep(start_gru)%dat(:),outputTimestepUpdate,maxLayers,num_steps,&
                        start_gru, max_gru, numGRU, &
                        indx_meta,outputStructure(1)%indxStat,outputStructure(1)%indxStruct,'indx', &
                        indxChild_map,outputStructure(1)%indxStruct,err,cmessage)
    end select
    if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
  end do  ! (looping through structures)


  do iFreq = 1,maxvarFreq
    outputTimeStep(start_gru)%dat(iFreq) = outputTimeStep(start_gru)%dat(iFreq) + outputTimeStepUpdate(iFreq) 
  end do ! ifreq

end subroutine writeOutput_fortran


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
! public subroutine writeData: write model time-dependent data
! **************************************************************************************
subroutine writeData(ncid,outputTimestep,outputTimestepUpdate,maxLayers,nSteps, &
            minGRU, maxGRU, numGRU, & 
            meta,stat,dat,structName,map,indx,err,message)
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
  integer(i4b)  ,intent(inout)     :: outputTimestepUpdate(:) ! number of HRUs in the run domain
  integer(i4b)  ,intent(in)        :: maxLayers         ! maximum number of layers
  integer(i4b)  ,intent(in)        :: nSteps            ! number of timeSteps
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
  real(rkind)                      :: timeVec(nSteps)   ! timeVal to copy
  integer(i4b),parameter           :: ixInteger=1001    ! named variable for integer
  integer(i4b),parameter           :: ixReal=1002       ! named variable for real
  integer(i4b)                     :: stepCounter       ! counter to know how much data we have to write, needed because we do not always write nSteps
  integer(i4b)                     :: iStep
  integer(i4b)                     :: iGRU
  ! initialize error control
  err=0;message="writeData/"
  ! loop through output frequencies
  do iFreq=1,maxvarFreq
    ! skip frequencies that are not needed
    if(.not.outFreq(iFreq)) cycle

    ! loop through model variables
    do iVar = 1,size(meta)
      stepCounter = 0

      if (meta(iVar)%varName=='time' .and. structName == 'forc')then
        ! get variable index
        err = nf90_inq_varid(ncid%var(iFreq),trim(meta(iVar)%varName),ncVarID)
        call netcdf_err(err,message); if (err/=0) return
        
        do iStep = 1, nSteps
          ! check if we want this timestep
          if(.not.outputStructure(1)%finalizeStats%gru(minGRU)%hru(1)%tim(iStep)%dat(iFreq)) cycle
          stepCounter = stepCounter+1
          timeVec(stepCounter) = outputStructure(1)%forcStruct%gru(minGRU)%hru(1)%var(iVar)%tim(iStep)
        end do ! iStep
        err = nf90_put_var(ncid%var(iFreq),ncVarID,timeVec(1:stepCounter),start=(/outputTimestep(iFreq)/),count=(/stepCounter/))
        call netcdf_err(err,message); if (err/=0)then; print*, "err"; return; endif
        ! save the value of the number of steps to update outputTimestep at the end of the function
        outputTimeStepUpdate(iFreq) = stepCounter
        cycle
      end if  ! id time

      ! define the statistics index
      iStat = meta(iVar)%statIndex(iFreq)
      ! check that the variable is desired
      if (iStat==integerMissing.or.trim(meta(iVar)%varName)=='unknown') cycle

        ! stats output: only scalar variable type
        if(meta(iVar)%varType==iLookVarType%scalarv) then
          call writeScalar(ncid, outputTimeStep, outputTimeStepUpdate, nSteps, minGRU, maxGRU, numGRU, iFreq, iVar, meta, stat, map, err, message)
        else ! non-scalar variables: regular data structures
          call writeVector(ncid, outputTimeStep, maxLayers, nSteps, minGRU, maxGRU, numGRU, iFreq, iVar, meta, dat, &
            indx, err, message)
        end if ! not scalarv

      ! process error code
      if (err/=0) message=trim(message)//trim(meta(iVar)%varName)//'_'//trim(get_statName(iStat))
      call netcdf_err(err,message); if (err/=0) return

    end do ! iVar
  end do ! iFreq

end subroutine writeData

subroutine writeScalar(ncid, outputTimestep, outputTimestepUpdate, nSteps, minGRU, maxGRU, &
  numGRU, iFreq, iVar, meta, stat, map, err, message)
  USE data_types,only:var_info                       ! metadata type

  implicit none
  ! declare dummy variables
  type(var_i)   ,intent(in)         :: ncid                    ! fileid
  integer(i4b)  ,intent(inout)      :: outputTimestep(:)       ! output time step
  integer(i4b)  ,intent(inout)      :: outputTimestepUpdate(:) ! number of HRUs in the run domain
  integer(i4b)  ,intent(in)         :: nSteps                  ! number of timeSteps
  integer(i4b)  ,intent(in)         :: minGRU                  ! minGRU index to write
  integer(i4b)  ,intent(in)         :: maxGRU                  ! maxGRU index to write - probably not needed
  integer(i4b)  ,intent(in)         :: numGRU
  integer(i4b)  ,intent(in)         :: iFreq                   ! output file index (year, month, day, timesteps)
  integer(i4b)  ,intent(in)         :: iVar                    ! netcdf variable we are writing data for
  type(var_info),intent(in)         :: meta(:)                 ! meta data
  class(*)      ,intent(in)         :: stat                    ! stats data
  integer(i4b)  ,intent(in)         :: map(:)                  ! map into stats child struct
  integer(i4b)  ,intent(inout)      :: err
  character(*)  ,intent(inout)      :: message

  ! local variables
  integer(i4b)                      :: gruCounter=0             ! counter for the realVecs
  integer(i4b)                      :: iStep=1                  ! counter for looping over nSteps
  integer(i4b)                      :: stepCounter=0            ! counter for the realVec
  integer(i4b)                      :: iGRU
  ! output array
  real(rkind)                       :: realVec(numGRU, nSteps)! real vector for all HRUs in the run domain

  err=0; message="writeOutput.f90-writeScalar/"

  select type(stat)
    class is (gru_hru_time_doubleVec)
      gruCounter=0
      do iGRU = minGRU, maxGRU
        stepCounter = 0
        gruCounter = gruCounter + 1
        do iStep = 1, nSteps
          if(.not.outputStructure(1)%finalizeStats%gru(iGRU)%hru(1)%tim(iStep)%dat(iFreq)) cycle
          stepCounter = stepCounter + 1
          realVec(gruCounter, stepCounter) = stat%gru(iGRU)%hru(1)%var(map(iVar))%tim(iStep)%dat(iFreq)
          outputTimeStepUpdate(iFreq) = stepCounter
        end do ! iStep
      end do ! iGRU 

      if (outputTimeStepUpdate(iFreq) /= stepCounter ) then
        print*, "ERROR Missmatch in Steps - stat doubleVec"
        print*, "   outputTimeStepUpdate(iFreq) = ", outputTimeStepUpdate(iFreq)
        print*, "   outputTimestep(iFreq) = ", outputTimestep(iFreq)
        print*, "   stepCounter = ", stepCounter
        print*, "   iFreq = ", iFreq
        print*, "   minGRU = ", minGRU
        print*, "   maxGRU = ", maxGRU
        print*, "   nSteps = ", nSteps
        print*, "   gruCounter = ", gruCounter
        print*, "   realVec = ", realVec
        print*, "   iStep = ", iStep
        err = 20
        return
      endif
      err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),realVec(1:gruCounter, 1:stepCounter),start=(/minGRU,outputTimestep(iFreq)/),count=(/numGRU,stepCounter/))
    class default; err=20; message=trim(message)//'stats must be scalarv and of type gru_hru_doubleVec'; return
  end select  ! stat

end subroutine

subroutine writeVector(ncid, outputTimestep, maxLayers, nSteps, minGRU, maxGRU, &
  numGRU, iFreq, iVar, meta, dat, indx, err, message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:iLookIndex                     ! index into index structure
  USE var_lookup,only:iLookVarType                   ! index into type structure
  implicit none
  type(var_i)   ,intent(in)             :: ncid                    ! fileid
  integer(i4b)  ,intent(inout)          :: outputTimestep(:)       ! output time step
  integer(i4b)  ,intent(in)             :: maxLayers         ! maximum number of layers
  integer(i4b)  ,intent(in)             :: nSteps                  ! number of timeSteps
  integer(i4b)  ,intent(in)             :: minGRU                  ! minGRU index to write
  integer(i4b)  ,intent(in)             :: maxGRU                  ! maxGRU index to write - probably not needed
  integer(i4b)  ,intent(in)             :: numGRU
  integer(i4b)  ,intent(in)             :: iFreq                   ! output file index (year, month, day, timesteps)
  integer(i4b)  ,intent(in)             :: iVar                    ! netcdf variable we are writing data for
  type(var_info),intent(in)             :: meta(:)                 ! meta data
  class(*)      ,intent(in)             :: dat               ! timestep data
  type(gru_hru_time_intVec) ,intent(in) :: indx         ! index data
  integer(i4b)  ,intent(inout)          :: err
  character(*)  ,intent(inout)          :: message

  ! local variables
  integer(i4b)                          :: gruCounter             ! counter for the realVecs
  integer(i4b)                          :: iStep                  ! counter for looping over nSteps
  integer(i4b)                          :: stepCounter            ! counter for the realVec
  integer(i4b)                          :: iGRU
  integer(i4b)                          :: nSoil
  integer(i4b)                          :: nSnow
  integer(i4b)                          :: nLayers
  ! output array
  integer(i4b)                          :: datLength         ! length of each data vector
  integer(i4b)                          :: maxLength         ! maximum length of each data vector
  integer(i4b)                          :: dataType          ! type of data
  integer(i4b),parameter                :: ixInteger=1001    ! named variable for integer
  integer(i4b),parameter                :: ixReal=1002       ! named variable for real
  real(rkind)                           :: realArray(numGRU,maxLayers+1)  ! real array for all HRUs in the run domain
  integer(i4b)                          :: intArray(numGRU,maxLayers+1)   ! integer array for all HRUs in the run domain
  err=0; message="writeOutput.f90-writeVector/"

  ! initialize the data vectors
  select type (dat)
    class is (gru_hru_time_doubleVec); realArray(:,:) = realMissing;    dataType=ixReal
    class is (gru_hru_time_intVec);     intArray(:,:) = integerMissing; dataType=ixInteger
    class default; err=20; message=trim(message)//'data must not be scalarv and either of type gru_hru_doubleVec or gru_hru_intVec'; return
  end select

  ! Loop over GRUs
  
  stepCounter = outputTimeStep(iFreq)
  do iStep = 1, nSteps
    gruCounter = 1
    do iGRU = minGRU, maxGRU
      ! get the model layers
      nSoil   = indx%gru(iGRU)%hru(1)%var(iLookIndex%nSoil)%tim(iStep)%dat(1)
      nSnow   = indx%gru(iGRU)%hru(1)%var(iLookIndex%nSnow)%tim(iStep)%dat(1)
      nLayers = indx%gru(iGRU)%hru(1)%var(iLookIndex%nLayers)%tim(iStep)%dat(1)

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
              if(.not.outputStructure(1)%finalizeStats%gru(iGRU)%hru(1)%tim(iStep)%dat(iFreq)) cycle
              realArray(gruCounter,1:datLength) = dat%gru(iGRU)%hru(1)%var(iVar)%tim(iStep)%dat(1:datLength)

          class is (gru_hru_time_intVec)
              if(.not.outputStructure(1)%finalizeStats%gru(iGRU)%hru(1)%tim(iStep)%dat(iFreq)) cycle
              intArray(gruCounter,1:datLength) = dat%gru(iGRU)%hru(1)%var(iVar)%tim(iStep)%dat(1:datLength)
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
      gruCounter = gruCounter + 1
    end do ! iGRU

   ! write the data vectors
    select case(dataType)

      case(ixReal)
        err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),realArray(1:numGRU,1:maxLength),start=(/minGRU,1,stepCounter/),count=(/numGRU,maxLength,1/))
        if(err/=0)then; print*, "ERROR: with nf90_put_var in data vector (ixReal)"; return; endif
        realArray(:,:) = realMissing ! reset the realArray
      case(ixInteger)
        err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),intArray(1:numGRU,1:maxLength),start=(/minGRU,1,stepCounter/),count=(/numGRU,maxLength,1/))
        if(err/=0)then; print*, "ERROR: with nf90_put_var in data vector (ixInteger)"; return; endif
        intArray(:,:) = integerMissing ! reset the intArray
      case default; err=20; message=trim(message)//'data must be of type integer or real'; return
    end select ! data type
    stepCounter = stepCounter + 1
  end do ! iStep
end subroutine

! **************************************************************************************
! public subroutine writeBasin: write basin-average variables
! **************************************************************************************
subroutine writeBasin(ncid,outputTimestep,outputTimestepUpdate,nSteps,&
                      minGRU, maxGRU, numGRU, &
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
  integer(i4b)  ,intent(inout)  :: outputTimestepUpdate(:) ! number of HRUs in the run domain
  integer(i4b)  ,intent(in)     :: nSteps            ! number of timeSteps
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

      ! stats/data output - select data type
      select case (meta(iVar)%varType)

        case (iLookVarType%scalarv)
          call writeScalar(ncid, outputTimeStep, outputTimeStepUpdate, nSteps, &
                           minGRU, maxGRU, numGRU, iFreq, iVar, meta, stat, map, &
                           err, message)

        case (iLookVarType%routing)
          if (iFreq==1 .and. outputTimestep(iFreq)==1) then
            ! err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),(/dat(iVar)%tim(iStep)%dat/),start=(/1/),count=(/1000/))
          end if
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
! public subroutine writeTime: write current time to all files
! **************************************************************************************
subroutine writeTime(ncid,outputTimestep,iStep,meta,dat,err,message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:iLookStat                      ! index into stat structure
  implicit none

  ! declare dummy variables
  type(var_i)   ,intent(in)     :: ncid              ! file ids
  integer(i4b)  ,intent(inout)  :: outputTimestep(:) ! output time step
  integer(i4b)  ,intent(in)     :: iStep
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
    if(.not.outputStructure(1)%finalizeStats%gru(1)%hru(1)%tim(iStep)%dat(iFreq)) cycle

    ! loop through model variables
    do iVar = 1,size(meta)

      ! check instantaneous
    if (meta(iVar)%statIndex(iFreq)/=iLookStat%inst) cycle
      ! get variable id in file
      err = nf90_inq_varid(ncid%var(iFreq),trim(meta(iVar)%varName),ncVarID)
      if (err/=0) message=trim(message)//trim(meta(iVar)%varName); call netcdf_err(err,message)
      if (err/=0) then; err=20; return; end if

      ! add to file
      err = nf90_put_var(ncid%var(iFreq),ncVarID,(/dat(iVar)%tim(iStep)/),start=(/outputTimestep(iFreq)/),count=(/1/))
      if (err/=0) message=trim(message)//trim(meta(iVar)%varName);call netcdf_err(err,message)
      if (err/=0) then; err=20; return; end if

    end do ! iVar
  end do ! iFreq


end subroutine writeTime   
end module fileAccess_writeOutput