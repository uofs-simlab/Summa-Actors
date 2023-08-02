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

module modelwrite_module

! NetCDF types
USE netcdf
USE netcdf_util_module,only:netcdf_err                    ! netcdf error handling function

! top-level data types
USE nrtype

! missing values
USE globalData,only: integerMissing, realMissing

! provide access to global data
USE globalData,only:gru_struc                             ! gru->hru mapping structure

USE data_types,only:var_i

! provide access to the derived types to define the data structures
USE data_types,only:&
                    ! final data vectors
                    dlength,             & ! var%dat
                    ilength,             & ! var%dat
                    time_dlength,        & ! var(:)%tim(:)%dat (dp)
                    ! no spatial dimension
                    var_i,               & ! x%var(:)            (i4b)
                    var_i8,              & ! x%var(:)            integer(8)
                    var_d,               & ! x%var(:)            (dp)
                    var_ilength,         & ! x%var(:)%dat        (i4b)
                    var_dlength,         & ! x%var(:)%dat        (dp)
                    ! no variable dimension
                    hru_i,               & ! x%hru(:)            (i4b)
                    hru_d,               & ! x%hru(:)            (dp)
                    time_i,              &
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
                    gru_hru_doubleVec,   & ! x%gru(:)%hru(:)%var(:)%dat (dp)
                    gru_hru_time_double, &
                    gru_hru_time_doubleVec,&
                    gru_hru_time_intVec

! vector lengths
USE var_lookup, only: maxvarFreq ! number of output frequencies
USE var_lookup, only: maxvarStat ! number of statistics

implicit none
private
public::writeParm
public::writeData
public::writeBasin
public::writeTime
!public::writeRestart !Actors does not use

! define dimension lengths
integer(i4b),parameter      :: maxSpectral=2              ! maximum number of spectral bands
contains

    ! **********************************************************************************************************
    ! public subroutine writeParm: write model parameters
    ! **********************************************************************************************************
subroutine writeParm(ncid,ispatial,struct,meta,err,message)
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

! Change Name to writeData when done
subroutine writeData(ncid, finalize_stats, output_timestep, max_layers, index_gru, num_gru, meta, &
    stat, dat, struct_name, map, indx, err, message)
  USE netcdf
  USE data_types,only:var_info                       ! metadata type
  USE globalData,only:outFreq                        ! output file information
  USE var_lookup,only:iLookVarType                   ! index into type structure
  USE var_lookup,only:iLookIndex                     ! index into index structure
  USE var_lookup,only:iLookProg                    ! index into index structure
  USE get_ixName_module,only:get_statName            ! to access type strings for error messages
  implicit none
  ! dummy variables
  type(var_i),      intent(in)       :: ncid
  logical(lgt),     intent(in)       :: finalize_stats(:)  ! flags to finalize statistics
  integer(i4b),     intent(inout)    :: output_timestep(:) ! number of HRUs in the run domain
  integer(i4b),     intent(in)       :: max_layers
  integer(i4b),     intent(in)       :: index_gru
  integer(i4b),     intent(in)       :: num_gru
  type(var_info),   intent(in)       :: meta(:)           ! meta data
  class(*),         intent(in)       :: stat              ! stats data
  class(*),         intent(in)       :: dat               ! timestep data
  character(*),     intent(in)       :: struct_name
  integer(i4b),     intent(in)       :: map(:)            ! map into stats child struct
  type(var_ilength),intent(in)       :: indx
  integer(i4b),     intent(out)      :: err               ! error code
  character(*),     intent(out)      :: message           ! error message
  ! local variables
  integer(i4b)                       :: iVar              ! variable index
  integer(i4b)                       :: iStat             ! statistics index
  integer(i4b)                       :: iFreq             ! frequency index
  integer(i4b)                       :: ncVarID           ! used only for time
  integer(i4b)                       :: index_hru
  integer(i4b)                       :: nSnow             ! number of snow layers
  integer(i4b)                       :: nSoil             ! number of soil layers
  integer(i4b)                       :: nLayers           ! total number of layers
  ! output array
  integer(i4b)                       :: datLength         ! length of each data vector
  integer(i4b)                       :: maxLength         ! maximum length of each data vector
  real(rkind)                        :: realVec(num_gru)  ! real vector for all HRUs in the run domain
  real(rkind)                        :: realArray(num_gru,max_layers+1)  ! real array for all HRUs in the run domain
  integer(i4b)                       :: intArray(num_gru,max_layers+1)  ! real array for all HRUs in the run domain
  integer(i4b)                       :: dataType          ! type of data
  integer(i4b),parameter             :: ixInteger=1001    ! named variable for integer
  integer(i4b),parameter             :: ixReal=1002       ! named variable for real

  err=0;message="writeOutput.f90 - writeDataNew/"

  ! loop through output frequencies
  do iFreq=1,maxvarFreq
    if(.not.outFreq(iFreq)) cycle ! check if frequency is desired (timestep, day, month, year)

    if(.not.finalize_stats(iFreq)) cycle ! check we have statistics for a frequency

    do iVar = 1,size(meta)
      ! handle time
      if (meta(iVar)%varName=='time' .and. struct_name == 'forc')then

        ! get variable index
        err = nf90_inq_varid(ncid%var(iFreq),trim(meta(iVar)%varName),ncVarID)
        call netcdf_err(err,message)
        if (err/=0) then
          print*, message
          return
        endif

        select type(dat)
          class is(var_d)
            err = nf90_put_var(ncid%var(iFreq),ncVarID,dat%var(iVar),start=(/output_timestep(iFreq)/))
            call netcdf_err(err,message)
            if (err/=0) then
              print*, message
              return
            endif
            cycle
            class default
              err=20
              message=trim(message)//'time variable must be of type var_dlength (forcing data structure)'
              print*, message
              return
        end select

        call netcdf_err(err,message)
        if (err/=0) then
          print*, message
          return
        endif
      endif

      ! define the statistics index
      iStat = meta(iVar)%statIndex(iFreq)

      ! check that the variable is desired
      if (iStat==integerMissing.or.trim(meta(iVar)%varName)=='unknown') cycle

      ! check if we are writing a vector or a scalar
      if(meta(iVar)%varType==iLookVarType%scalarv) then
        select type(stat)
          class is (var_dlength)
            realVec(1) = stat%var(map(iVar))%dat(iFreq)
            err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),realVec,start=(/index_gru,output_timestep(iFreq)/),count=(/num_gru,1/))
            if (err/=0) then
              print*, message
              return
            endif
          class default
            err=20
            message=trim(message)//'stats must be scalarv and of type var_dlength'
            print*, message
            return
        end select ! stat

      else

         ! Write the data
        select type(dat)
          class is (var_dlength)
            realArray(:,:) = realMissing;    dataType=ixReal
          class is (var_ilength)
            intArray(:,:) = integerMissing; dataType=ixInteger
          class default
          err=20
          message=trim(message)//'data must be of type integer or real'
          print*, message
          return
      end select
        ! vector
        nSoil   = indx%var(iLookIndex%nSoil)%dat(1)
        nSnow   = indx%var(iLookIndex%nSnow)%dat(1)
        nLayers = indx%var(iLookIndex%nLayers)%dat(1)

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
          class is (var_dlength)
            realArray(1,1:datLength) = dat%var(iVar)%dat(:)
          class is (var_ilength);  intArray(1,1:datLength) = dat%var(iVar)%dat(:)
          class default; err=20; message=trim(message)//'data must not be scalarv and either of type gru_hru_doubleVec or gru_hru_intVec'; return
        end select

        ! get the maximum length of each data vector
        select case (meta(iVar)%varType)
          case(iLookVarType%wLength); maxLength = maxSpectral
          case(iLookVarType%midToto); maxLength = max_layers
          case(iLookVarType%midSnow); maxLength = max_layers-nSoil
          case(iLookVarType%midSoil); maxLength = nSoil
          case(iLookVarType%ifcToto); maxLength = max_layers+1
          case(iLookVarType%ifcSnow); maxLength = (max_layers-nSoil)+1
          case(iLookVarType%ifcSoil); maxLength = nSoil+1
          case default; cycle
        end select ! vartype

        ! Write the data
        select case(dataType)
          case(ixReal)
            err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),realArray(1:num_gru,1:maxLength),start=(/index_gru,1,output_timestep(iFreq)/),count=(/num_gru,maxLength,1/))
          case(ixInteger)
            err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),intArray(1:num_gru,1:maxLength),start=(/index_gru,1,output_timestep(iFreq)/),count=(/num_gru,maxLength,1/))
          case default
            err=20
            message=trim(message)//'data must be of type integer or real'
            print*, message
            return
        end select
      endif

         ! process error code
      if (err/=0)then
        message=trim(message)//trim(meta(iVar)%varName)//'_'//trim(get_statName(iStat))
        print*, message
      end if
      call netcdf_err(err,message)
      if (err/=0) then
        print*, message
        return
      endif

    end do ! iVar
  end do ! iFreq
end subroutine writeData

! **************************************************************************************
! public subroutine writeBasin: write basin-average variables
! **************************************************************************************
subroutine writeBasin(ncid,iGRU,finalizeStats,outputTimestep,meta,stat,dat,map,err,message)
  USE data_types,only:var_info                       ! metadata type
  USE var_lookup,only:maxVarStat                     ! index into stats structure
  USE var_lookup,only:iLookVarType                   ! index into type structure
  USE globalData,only:outFreq                        ! output file information
  USE get_ixName_module,only:get_varTypeName         ! to access type strings for error messages
  USE get_ixName_module,only:get_statName            ! to access type strings for error messages
  implicit none

  ! declare dummy variables
  type(var_i),   intent(in)     :: ncid              ! file ids
  integer(i4b),  intent(in)     :: iGRU              ! GRU index
  logical(lgt),  intent(in)     :: finalizeStats(:)
  integer(i4b),  intent(inout)  :: outputTimestep(:) ! output time step
  type(var_info),intent(in)     :: meta(:)           ! meta data
  type(dlength),intent(in)      :: stat(:)           ! stats data
  type(dlength),intent(in)      :: dat(:)            ! timestep data
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

    ! check that we have finalized statistics for a given frequency
    if(.not.finalizeStats(iFreq)) cycle

    ! loop through model variables
    do iVar = 1,size(meta)

      ! define the statistics index
      iStat = meta(iVar)%statIndex(iFreq)

      ! check that the variable is desired
      if (iStat==integerMissing.or.trim(meta(iVar)%varName)=='unknown') cycle

      ! stats/data output - select data type
      select case (meta(iVar)%varType)

        case (iLookVarType%scalarv)
          err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),(/stat(map(iVar))%dat(iFreq)/),start=(/iGRU,outputTimestep(iFreq)/),count=(/1,1/))

        case (iLookVarType%routing)
          if (iFreq==1 .and. outputTimestep(iFreq)==1) then
            err = nf90_put_var(ncid%var(iFreq),meta(iVar)%ncVarID(iFreq),(/dat(iVar)%dat/),start=(/1/),count=(/1000/))
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
subroutine writeTime(ncid,finalizeStats,outputTimestep,meta,dat,err,message)
USE data_types,only:var_info                       ! metadata type
USE var_lookup,only:iLookStat                      ! index into stat structure
implicit none

! declare dummy variables
type(var_i)   ,intent(in)     :: ncid              ! file ids
logical(lgt)  ,intent(in)     :: finalizeStats(:)  ! flags to finalize statistics
integer(i4b)  ,intent(inout)  :: outputTimestep(:) ! output time step
type(var_info),intent(in)     :: meta(:)           ! meta data
integer       ,intent(in)     :: dat(:)            ! timestep data
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
  if(.not.finalizeStats(iFreq)) cycle

  ! loop through model variables
  do iVar = 1,size(meta)

    ! check instantaneous
  if (meta(iVar)%statIndex(iFreq)/=iLookStat%inst) cycle
      ! get variable id in file
    err = nf90_inq_varid(ncid%var(iFreq),trim(meta(iVar)%varName),ncVarID)
    if (err/=0) message=trim(message)//trim(meta(iVar)%varName); call netcdf_err(err,message)
    if (err/=0) then; err=20; return; end if

    ! add to file
    err = nf90_put_var(ncid%var(iFreq),ncVarID,(/dat(iVar)/),start=(/outputTimestep(iFreq)/),count=(/1/))
    if (err/=0) message=trim(message)//trim(meta(iVar)%varName);call netcdf_err(err,message)
    if (err/=0) then; err=20; return; end if

  end do ! iVar
end do ! iFreq


end subroutine writeTime


end module modelwrite_module