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

module def_output_actors_module

USE data_types,only:var_i
USE actor_data_types,only:netcdf_gru_actor_info
USE netcdf
USE netcdf_util_module,only:netcdf_err        ! netcdf error handling function
USE netcdf_util_module,only:nc_file_close     ! close NetCDF files
USE f2008funcs_module,only:cloneStruc         ! used to "clone" data structures -- temporary replacement of the intrinsic allocate(a, source=b)
USE nrtype, integerMissing=>nr_integerMissing ! top-level data types
USE globalData, only: outputPrecision         ! data structure for output precision
USE globalData, only: chunkSize               ! size of chunks to write
implicit none
private
public :: def_output

! define dimension names
character(len=32),parameter :: gru_DimName      = 'gru'              ! dimension name for the GRUs
character(len=32),parameter :: hru_DimName      = 'hru'              ! dimension name for the HRUs
character(len=32),parameter :: depth_DimName    = 'depth'            ! dimension name for soil depth
character(len=32),parameter :: scalar_DimName   = 'scalar'           ! dimension name for scalar variables
character(len=32),parameter :: wLength_dimName  = 'spectral_bands'   ! dimension name for the number of spectral bands
character(len=32),parameter :: timestep_DimName = 'time'             ! dimension name for the time step
character(len=32),parameter :: routing_DimName  = 'timeDelayRouting' ! dimension name for the time delay routing vectors
character(len=32),parameter :: midSnow_DimName  = 'midSnow'          ! dimension name for midSnow
character(len=32),parameter :: midSoil_DimName  = 'midSoil'          ! dimension name for midSoil
character(len=32),parameter :: midToto_DimName  = 'midToto'          ! dimension name for midToto
character(len=32),parameter :: ifcSnow_DimName  = 'ifcSnow'          ! dimension name for ifcSnow
character(len=32),parameter :: ifcSoil_DimName  = 'ifcSoil'          ! dimension name for ifcSoil
character(len=32),parameter :: ifcToto_DimName  = 'ifcToto'          ! dimension name for ifcToto

! define the dimension IDs
integer(i4b)                :: gru_DimID                             ! dimension name for the GRUs
integer(i4b)                :: hru_DimID                             ! dimension name for the HRUs
integer(i4b)                :: depth_DimID                           ! dimension name for the soil depth
integer(i4b)                :: scalar_DimID                          ! dimension name for scalar variables
integer(i4b)                :: wLength_dimID                         ! dimension name for the number of spectral bands
integer(i4b)                :: timestep_DimID                        ! dimension name for the time step
integer(i4b)                :: routing_DimID                         ! dimension name for thetime delay routing vectors
integer(i4b)                :: midSnow_DimID                         ! dimension name for midSnow
integer(i4b)                :: midSoil_DimID                         ! dimension name for midSoil
integer(i4b)                :: midToto_DimID                         ! dimension name for midToto
integer(i4b)                :: ifcSnow_DimID                         ! dimension name for ifcSnow
integer(i4b)                :: ifcSoil_DimID                         ! dimension name for ifcSoil
integer(i4b)                :: ifcToto_DimID                         ! dimension name for ifcToto

! define named variables to specify dimensions
integer(i4b),parameter  :: needGRU=0,needHRU=1,noHRU=2    ! define if there is an HRU dimension
integer(i4b),parameter  :: needTime=1,noTime=2            ! define if there is a time dimension

contains

 ! **********************************************************************************************************
 ! public subroutine def_output: define model output file
 ! **********************************************************************************************************
subroutine def_output(ncid,startGRU,nGRU,nHRU,actor_info,err,message)
  USE globalData,only:structInfo                               ! information on the data structures
  USE globalData,only:forc_meta,attr_meta,type_meta            ! metaData structures
  USE globalData,only:prog_meta,diag_meta,flux_meta,deriv_meta ! metaData structures
  USE globalData,only:mpar_meta,indx_meta                      ! metaData structures
  USE globalData,only:bpar_meta,bvar_meta,time_meta            ! metaData structures
  USE globalData,only:model_decisions                          ! model decisions
  USE globalData,only:outFreq                                  ! output frequencies
  ! Some global variabels required in the writing process
  USE globalData,only:nHRUrun
  USE globalData,only:nGRUrun
  USE globalData,only:gru_struc
  USE globalData,only:fileout
  ! modules that are not globalData
  USE var_lookup,only:maxVarFreq                               ! # of available output frequencies
  USE get_ixname_module,only:get_freqName                      ! get name of frequency from frequency index
  USE summaFileManager,only:OUTPUT_PATH,OUTPUT_PREFIX ! define output file
  USE globalData,only:outputTimeStep              ! output time step

  ! ---------------------------------------------------------------------------------------
  ! * Dummy Variables
  ! ---------------------------------------------------------------------------------------
  type(var_i),pointer                    :: ncid              ! id of output file
  integer(i4b),intent(in)                :: startGRU          ! startGRU for the entire job (for file creation)
  integer(i4b),intent(in)                :: nGRU              ! number of GRUs
  integer(i4b),intent(in)                :: nHRU              ! number of HRUs
  type(netcdf_gru_actor_info),intent(out):: actor_info        ! netcdf actor information 
  character(*),intent(out)               :: message           ! error message
  integer(i4b),intent(out)               :: err               ! error code

  ! ---------------------------------------------------------------------------------------
  ! * Local Subroutine Variables
  ! ---------------------------------------------------------------------------------------
  integer(i4b)                         :: ivar                          ! loop through model decisions
  integer(i4b)                         :: iFreq                         ! loop through output frequencies
  integer(i4b)                         :: iStruct                       ! loop through structure types
  character(len=32)                    :: fstring                       ! string to hold model output freuqnecy
  character(len=256)                   :: cmessage                      ! temporary error message
  integer(i4b)                         :: iGRU
  character(LEN=256)                   :: startGRUString    ! String Variable to convert startGRU
  character(LEN=256)                   :: numGRUString      ! String Varaible to convert numGRU
  character(len=1024)                  :: fname                         ! temporary filename


  ! initialize errors
  err=0; message="def_output/"

  ! allocate space for the output file ID array
  if (.not.allocated(ncid%var))then
    allocate(ncid%var(maxVarFreq))
    ncid%var(:) = integerMissing
  endif
  

  ! Set the global variable for the number of HRU and GRU in run
  nGRUrun = nGRU
  nHRUrun = nGRU

  ! create the name of the new files
  write(unit=startGRUString,fmt=*)startGRU
  write(unit=numGRUString,fmt=*)  nGRU
  fileout = trim(OUTPUT_PATH)//trim(OUTPUT_PREFIX)//"GRU"&
              //trim(adjustl(startGRUString))//"-"//trim(adjustl(numGRUString))

  ! close files if already open
  do iFreq=1,maxvarFreq
    if (ncid%var(iFreq)/=integerMissing) then
    call nc_file_close(ncid%var(iFreq),err,cmessage)
    if(err/=0)then
      message=trim(message)//trim(cmessage)
      print*, message
      return
    end if
    endif
  end do




  ! create initial file
  ! each file will have a master name with a frequency appended at the end:
  ! e.g., xxxxxxxxx_timestep.nc  (for output at every model timestep)
  ! e.g., xxxxxxxxx_monthly.nc   (for monthly model output)
  do iFreq=1,maxvarFreq
    ! skip frequencies that are not needed

    if(.not.outFreq(iFreq)) cycle

    ! create file
    fstring = get_freqName(iFreq)
    fname   = trim(fileout)//'_'//trim(fstring)//'.nc'
    call ini_create(nGRU,nHRU,gru_struc(1)%hruInfo(1)%nSoil,trim(fname),ncid%var(iFreq),err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); print*, message; return; end if

    ! define model decisions
    do iVar = 1,size(model_decisions)
        if(model_decisions(iVar)%iDecision.ne.integerMissing)then
          call put_attrib(ncid%var(iFreq),model_decisions(iVar)%cOption,model_decisions(iVar)%cDecision,err,cmessage)
          if(err/=0)then
            message=trim(message)//trim(cmessage)
            print*, message
            return
          end if
        end if
    end do
    
    ! define variables
    do iStruct = 1,size(structInfo)
        select case (trim(structInfo(iStruct)%structName))
        case('attr'  ); call def_variab(ncid%var(iFreq),iFreq,needHRU,  noTime,attr_meta, outputPrecision, err,cmessage)  ! local attributes HRU
        case('type'  ); call def_variab(ncid%var(iFreq),iFreq,needHRU,  noTime,type_meta, nf90_int,   err,cmessage)       ! local classification
        case('mpar'  ); call def_variab(ncid%var(iFreq),iFreq,needHRU,  noTime,mpar_meta, outputPrecision, err,cmessage)  ! model parameters
        case('bpar'  ); call def_variab(ncid%var(iFreq),iFreq,needGRU,  noTime,bpar_meta, outputPrecision, err,cmessage)  ! basin-average param
        case('indx'  ); call def_variab(ncid%var(iFreq),iFreq,needHRU,needTime,indx_meta, nf90_int,   err,cmessage)       ! model variables
        case('deriv' ); call def_variab(ncid%var(iFreq),iFreq,needHRU,needTime,deriv_meta,outputPrecision, err,cmessage)  ! model derivatives
        case('time'  ); call def_variab(ncid%var(iFreq),iFreq,  noHRU,needTime,time_meta, nf90_int,   err,cmessage)       ! model derivatives
        case('forc'  ); call def_variab(ncid%var(iFreq),iFreq,needHRU,needTime,forc_meta, outputPrecision, err,cmessage)  ! model forcing data
        case('prog'  ); call def_variab(ncid%var(iFreq),iFreq,needHRU,needTime,prog_meta, outputPrecision, err,cmessage)  ! model prognostics
        case('diag'  ); call def_variab(ncid%var(iFreq),iFreq,needHRU,needTime,diag_meta, outputPrecision, err,cmessage)  ! model diagnostic variables
        case('flux'  ); call def_variab(ncid%var(iFreq),iFreq,needHRU,needTime,flux_meta, outputPrecision, err,cmessage)  ! model fluxes
        case('bvar'  ); call def_variab(ncid%var(iFreq),iFreq,needGRU,needTime,bvar_meta, outputPrecision, err,cmessage)  ! basin-average variables
        case('id'    ); cycle
        case('lookup'); cycle                                                                                         ! ids -- see write_hru_info()
        case default; err=20; message=trim(message)//'unable to identify lookup structure';
        end select
        ! error handling
        if(err/=0)then
          err=20
          message=trim(message)//trim(cmessage)//'[structure =  '//trim(structInfo(iStruct)%structName)
          print*, message
          return
        end if
    end do ! iStruct
    ! write HRU dimension and ID for each output file
    call write_hru_info(ncid%var(iFreq), err, cmessage) 
    if(err/=0) then 
      message=trim(message)//trim(cmessage)
      print*, message
      return
    end if

    ! define timing variables for actors code
    ! TODO: Add attributes to these variables
    err = nf90_def_var(ncid%var(iFreq),"run_time",outputPrecision,(/gru_DimID/),actor_info%run_time_var_id)
    err = nf90_def_var(ncid%var(iFreq),"init_duration",outputPrecision,(/gru_DimID/),actor_info%init_duration_var_id)
    err = nf90_def_var(ncid%var(iFreq),"forcing_duration",outputPrecision,(/gru_DimID/),actor_info%forcing_duration_var_id)
    err = nf90_def_var(ncid%var(iFreq),"run_physics_duration",outputPrecision,(/gru_DimID/),actor_info%run_physics_duration_var_id)
    err = nf90_def_var(ncid%var(iFreq),"write_output_duration",outputPrecision,(/gru_DimID/),actor_info%write_output_duration_var_id)
    err = nf90_def_var(ncid%var(iFreq),"successful",nf90_int,(/gru_DimID/),actor_info%state_var_id)
    err = nf90_def_var(ncid%var(iFreq),"num_attempts",nf90_int,(/gru_DimID/),actor_info%num_attempts_var_id)
    err = nf90_def_var(ncid%var(iFreq),"rel_tol",outputPrecision,(/gru_DimID/),actor_info%rel_tol_var_id)
    err = nf90_def_var(ncid%var(iFreq),"abs_tol",outputPrecision,(/gru_DimID/),actor_info%abs_tol_var_id)
    if(err/=0) then; message=trim(message)//trim(cmessage); print*, message; return; end if

  end do
end subroutine def_output

! **********************************************************************************************************
! private subroutine ini_create: initial create
! **********************************************************************************************************
subroutine ini_create(nGRU,nHRU,nSoil,infile,ncid,err,message)
 ! variables to define number of steps per file (total number of time steps, step length, etc.)
 USE multiconst,only:secprday           ! number of seconds per day
 ! model decisions
 USE globalData,only:model_decisions    ! model decision structure
 USE var_lookup,only:iLookDECISIONS     ! named variables for elements of the decision structure
 USE mDecisions_module,only:&
  sameRulesAllLayers, & ! SNTHERM option: same combination/sub-dividion rules applied to all layers
  rulesDependLayerIndex ! CLM option: combination/sub-dividion rules depend on layer index
 implicit none
 ! declare dummy variables
 integer(i4b),intent(in)     :: nGRU                       ! number of GRUs
 integer(i4b),intent(in)     :: nHRU                       ! number of HRUs
 integer(i4b),intent(in)     :: nSoil                      ! number of soil layers in the first HRU (used to define fixed length dimensions)
 character(*),intent(in)     :: infile                     ! filename
 integer(i4b),intent(out)    :: ncid                       ! netcdf file id
 integer(i4b),intent(out)    :: err                        ! error code
 character(*),intent(out)    :: message                    ! error message
 ! define local variables
 integer(i4b)                :: maxRouting=1000            ! maximum length of routing vector
 integer(i4b),parameter      :: maxSpectral=2              ! maximum number of spectral bands
 integer(i4b),parameter      :: scalarLength=1             ! length of scalar variable
 integer(i4b)                :: maxSnowLayers              ! maximum number of snow layers
 ! initialize error control
 err=0;message="f-iniCreate/"
 ! identify length of the variable vector
 select case(model_decisions(iLookDECISIONS%snowLayers)%iDecision)
  case(sameRulesAllLayers);    maxSnowLayers = 100
  case(rulesDependLayerIndex); maxSnowLayers = 5
  case default; err=20; message=trim(message)//'unable to identify option to combine/sub-divide snow layers'; return
 end select ! (option to combine/sub-divide snow layers)

 ! create output file
 !err = nf90_create(trim(infile),NF90_64BIT_OFFSET,ncid)
 err = nf90_create(trim(infile),NF90_NETCDF4,ncid)
 message='iCreate[create]'; call netcdf_err(err,message); if (err/=0) return

 ! create dimensions
 err = nf90_def_dim(ncid, trim(     gru_DimName), nGRU,                      gru_DimID); message='iCreate[GRU]';      call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim(     hru_DimName), nHRU,                      hru_DimID); message='iCreate[HRU]';      call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim(timestep_DimName), nf90_unlimited,       timestep_DimID); message='iCreate[time]';     call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim(   depth_DimName), nSoil,                   depth_DimID); message='iCreate[depth]';    call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim(  scalar_DimName), scalarLength,           scalar_DimID); message='iCreate[scalar]';   call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim( wLength_DimName), maxSpectral,           wLength_DimID); message='iCreate[spectral]'; call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim( routing_DimName), maxRouting,            routing_DimID); message='iCreate[routing]';  call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim( midSnow_DimName), maxSnowLayers,         midSnow_DimID); message='iCreate[midSnow]';  call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim( midSoil_DimName), nSoil,                 midSoil_DimID); message='iCreate[midSoil]';  call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim( midToto_DimName), nSoil+maxSnowLayers,   midToto_DimID); message='iCreate[midToto]';  call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim( ifcSnow_DimName), maxSnowLayers+1,       ifcSnow_DimID); message='iCreate[ifcSnow]';  call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim( ifcSoil_DimName), nSoil+1,               ifcSoil_DimID); message='iCreate[ifcSoil]';  call netcdf_err(err,message); if (err/=0) return
 err = nf90_def_dim(ncid, trim( ifcToto_DimName), nSoil+maxSnowLayers+1, ifcToto_DimID); message='iCreate[ifcToto]';  call netcdf_err(err,message); if (err/=0) return

 ! Leave define mode of NetCDF files
 err = nf90_enddef(ncid);  message='nf90_enddef'; call netcdf_err(err,message); if (err/=0) return
 
 end subroutine ini_create

 ! **********************************************************************************************************
 ! private subroutine put_attrib: put global attributes as character string
 ! **********************************************************************************************************
 subroutine put_attrib(ncid,attname,attvalue,err,message)
 USE data_types,only:var_info              ! derived type for metaData
 implicit none
 ! declare dummy variables
 integer(i4b), intent(in)   :: ncid        ! netcdf file ID
 character(*), intent(in)   :: attname     ! attribute name
 character(*), intent(in)   :: attvalue    ! attribute vaue
 integer(i4b),intent(out)   :: err         ! error code
 character(*),intent(out)   :: message     ! error message
 ! initialize error control
 err=0;message="put_attrib/"//trim(attname)//"/"//trim(attvalue)//"/"
 ! allow re-definition of variables
 err = nf90_redef(ncid); call netcdf_err(err,message); if (err/=0) return
 ! put the attribute
 err = nf90_put_att(ncid,nf90_global,trim(attname),trim(attvalue))
 call netcdf_err(err,message); if (err/=0) return
 ! close output file
 err = nf90_enddef(ncid); call netcdf_err(err,message); if (err/=0) return
 end subroutine put_attrib

 ! **********************************************************************************************************
 ! private subroutine def_variab: define variables
 ! **********************************************************************************************************
 subroutine def_variab(ncid,iFreq,spatialDesire,timeDesire,metaData,ivtype,err,message)
 USE var_lookup,only:iLookvarType                   ! look up structure for variable typed
 USE data_types,only:var_info                       ! derived type for metaData
 USE var_lookup,only:iLookStat                      ! index into stats structure
 USE var_lookup,only:maxVarFreq                     ! # of available output frequencies
 USE get_ixName_module,only:get_varTypeName         ! to access type strings for error messages
 USE get_ixname_module,only:get_statName            ! statistics names for variable defs in output file
 USE globalData,only:nHRUrun
 USE globalData,only:nGRUrun
 implicit none
 ! input
 integer(i4b)  ,intent(in)     :: ncid              ! netcdf file id
 integer(i4b)  ,intent(in)     :: iFreq             ! frequency of current file
 integer(i4b)  ,intent(in)     :: spatialDesire     ! variable to define if we desire the HRU dimension
 integer(i4b)  ,intent(in)     :: timeDesire        ! variable to define if we desire the time dimension
 type(var_info),intent(inout)  :: metaData(:)       ! metaData structure for a given variable
 integer(i4b)  ,intent(in)     :: ivtype            ! variable type
 ! output
 integer(i4b),intent(out)      :: err               ! error code
 character(*),intent(out)      :: message           ! error message
 ! local
 integer(i4b)                  :: iVar              ! variable index
 integer(i4b)                  :: iVarId            ! netcdf variable index
 integer(i4b)                  :: iStat             ! stat index
 integer(i4b),allocatable      :: dimensionIDs(:)   ! vector of dimension IDs
 integer(i4b),allocatable      :: writechunk(:)     ! size of chunks to be written
 integer(i4b)                  :: timePosition      ! extrinsic variable to hold substring index
 integer(i4b)                  :: timeChunk         ! size of time chunks to try to use
 integer(i4b)                  :: hruChunk          ! size of hru chunk to try to use
 integer(i4b)                  :: gruChunk          ! size of gru chunk to try to use
 integer(i4b)                  :: layerChunk        ! size of layer chunk to try to use
 character(LEN=256)            :: cmessage          ! error message of downwind routine
 character(LEN=256)            :: catName           ! full variable name
 ! initialize error control
 err=0; message='def_variab/'
 ! allow re-definition of variables
 err = nf90_redef(ncid); call netcdf_err(err,message); if (err/=0) return

 ! loop through metaData
 do iVar = 1,size(metaData)
  ! check that the variable is desired
  if (metaData(iVar)%varType==iLookvarType%unknown) cycle
  if (metaData(iVar)%statIndex(iFreq)==integerMissing .and. metaData(iVar)%varName/='time') cycle

  ! ---------- get the dimension IDs (use cloneStruc, given source) ----------
  gruChunk = min(nGRUrun, chunkSize)
  hruChunk = min(nHRUrun, chunkSize)
  timeChunk = chunkSize
  layerChunk = 1


  ! special case of the time variable
  if(metaData(iVar)%varName == 'time')then
   call cloneStruc(dimensionIDs, lowerBound=1, source=(/Timestep_DimID/),err=err,message=cmessage); writechunk=(/ timeChunk /)
   if(err/=0)then; message=trim(message)//trim(cmessage)//' [variable '//trim(metaData(iVar)%varName)//']'; return; end if

  ! standard case (not time)
  else
   select case(metaData(iVar)%varType)

    ! (scalar variable -- many different types)
    case(iLookvarType%scalarv)
     if(spatialDesire==needGRU .and. timeDesire==needTime) then; call cloneStruc(dimensionIDs, lowerBound=1, source=(/     gru_DimID,Timestep_DimID/), err=err, message=cmessage); writechunk=(/ gruChunk, int(timeChunk/gruChunk)+1 /); endif
     if(spatialDesire==needHRU .and. timeDesire==needTime) then; call cloneStruc(dimensionIDs, lowerBound=1, source=(/     hru_DimID,Timestep_DimID/), err=err, message=cmessage); writechunk=(/ hruChunk, int(timeChunk/hruChunk)+1 /); endif
     if(spatialDesire==needHRU .and. timeDesire==  noTime) then; call cloneStruc(dimensionIDs, lowerBound=1, source=(/     hru_DimID/)               , err=err, message=cmessage); writechunk=(/ hruChunk /); endif
     if(spatialDesire==  noHRU .and. timeDesire==needTime) then; call cloneStruc(dimensionIDs, lowerBound=1, source=(/Timestep_DimID/) , err=err, message=cmessage);               writechunk=(/ gruChunk /); endif
     if(spatialDesire==  noHRU .and. timeDesire==  noTime) then; call cloneStruc(dimensionIDs, lowerBound=1, source=(/  scalar_DimID/) , err=err, message=cmessage);               writechunk=(/ hruChunk, int(timeChunk/hruChunk)+1 /); endif

    ! (other variables)
    case(iLookvarType%wLength); call cloneStruc(dimensionIDs, lowerBound=1, source=(/hru_DimID, wLength_DimID, Timestep_DimID/), err=err, message=cmessage); writechunk=(/ hruChunk, layerChunk, int(timeChunk/hruChunk)+1 /)
    case(iLookvarType%midSnow); call cloneStruc(dimensionIDs, lowerBound=1, source=(/hru_DimID, midSnow_DimID, Timestep_DimID/), err=err, message=cmessage); writechunk=(/ hruChunk, layerChunk, int(timeChunk/hruChunk)+1 /)
    case(iLookvarType%midSoil); call cloneStruc(dimensionIDs, lowerBound=1, source=(/hru_DimID, midSoil_DimID, Timestep_DimID/), err=err, message=cmessage); writechunk=(/ hruChunk, layerChunk, int(timeChunk/hruChunk)+1 /)
    case(iLookvarType%midToto); call cloneStruc(dimensionIDs, lowerBound=1, source=(/hru_DimID, midToto_DimID, Timestep_DimID/), err=err, message=cmessage); writechunk=(/ hruChunk, layerChunk, int(timeChunk/hruChunk)+1 /)
    case(iLookvarType%ifcSnow); call cloneStruc(dimensionIDs, lowerBound=1, source=(/hru_DimID, ifcSnow_DimID, Timestep_DimID/), err=err, message=cmessage); writechunk=(/ hruChunk, layerChunk, int(timeChunk/hruChunk)+1 /)
    case(iLookvarType%ifcSoil); call cloneStruc(dimensionIDs, lowerBound=1, source=(/hru_DimID, ifcSoil_DimID, Timestep_DimID/), err=err, message=cmessage); writechunk=(/ hruChunk, layerChunk, int(timeChunk/hruChunk)+1 /)
    case(iLookvarType%ifcToto); call cloneStruc(dimensionIDs, lowerBound=1, source=(/hru_DimID, ifcToto_DimID, Timestep_DimID/), err=err, message=cmessage); writechunk=(/ hruChunk, layerChunk, int(timeChunk/hruChunk)+1 /)
    case(iLookvarType%parSoil); call cloneStruc(dimensionIDs, lowerBound=1, source=(/hru_DimID, depth_DimID                  /), err=err, message=cmessage); writechunk=(/ hruChunk, layerChunk/)
    case(iLookvarType%routing); call cloneStruc(dimensionIDs, lowerBound=1, source=(/routing_DimID                           /), err=err, message=cmessage); !writechunk=(/ layerChunk /)
   end select
   ! check errors
   if(err/=0)then
    message=trim(message)//trim(cmessage)//' [variable '//trim(metaData(iVar)%varName)//']'
    return
   end if
  end if  ! check if we are processing the time variable

  ! check that we got the shape
  if(.not.allocated(dimensionIDs))then
   message=trim(message)//'problem defining dimensions for variable '//trim(metaData(iVar)%varName)
   err=20; return
  end if

  ! ---------- create variables -----------------------------------------------------------------

  ! define statistics index
  iStat = metaData(iVar)%statIndex(iFreq)

  ! create full variable name (append statistics info)
  if(iStat==iLookStat%inst)then
   catName = trim(metaData(iVar)%varName)
  else
   catName = trim(metaData(iVar)%varName)//'_'//trim(get_statName(iStat))
  endif

  ! define variable
  err = nf90_def_var(ncid,trim(catName),ivtype,dimensionIDs,iVarId)
  call netcdf_err(err,message); if (err/=0) return

  err = nf90_def_var_chunking(ncid,iVarId,NF90_CHUNKED,writechunk)
  call netcdf_err(err,message); if (err/=0) return

  ! add parameter description
  catName = trim(metaData(iVar)%vardesc)//' ('//trim(get_statName(iStat))//')'
  err = nf90_put_att(ncid,iVarId,'long_name',trim(catName))
  call netcdf_err(err,message); if (err/=0) return

  ! modify units for the summation
  catName = trim(metaData(iVar)%varunit)
  if (iStat==iLookStat%totl) then

   ! make sure that the units of this variable allow for integration
   if ((index(catName,'s-1')<=0).and.(index(catName,'s-2')<=0).and.(index(catName,'W m-2')<=0)) then
    message=trim(message)//'trying to integrate a non-time variable: '//trim(metaData(iVar)%varName)//' - units: '//trim(catName)
    err=20; return
   endif

   ! change to integrated units
   if (index(catName,'s-1')>0)       then
    timePosition = index(catName,'s-1')
    catName(timePosition:(timePosition+3)) = '   '
   elseif (index(catName,'s-2')>0)   then
    timePosition = index(catName,'s-2')
    catName(timePosition:(timePosition+3)) = 's-1'
   elseif (index(catName,'W m-2')>0) then
    timePosition = index(catName,'W')
    catName(timePosition:(timePosition+1)) = 'J'
   end if

  end if  ! if an integrated flux

  ! add units attribute
  err = nf90_put_att(ncid,iVarId,'units',trim(catName))
  call netcdf_err(err,message); if (err/=0) return

  ! add NetCDF variable ID to metadata structure
  metaData(iVar)%ncVarID(iFreq) = iVarID

 end do  ! looping through variables

 ! close output file
 err = nf90_enddef(ncid); call netcdf_err(err,message); if (err/=0) return

 end subroutine def_variab

 ! **********************************************************************************************************
 ! internal subroutine write_hru_info: write HRU dimension and IDs
 ! **********************************************************************************************************
 subroutine write_hru_info(ncid, err, message)
 use globalData,only:gru_struc                    ! gru-hru mapping structures
 ! input
 integer(i4b),intent(in)     :: ncid              ! netcdf file id
 ! output
 integer(i4b),intent(out)    :: err               ! error code
 character(*),intent(out)    :: message           ! error message
 ! define local variables
 integer(i4b)                :: hruVarID          ! hru varID in netcdf file
 integer(i4b)                :: gruVarID          ! hru varID in netcdf file
 integer(i4b)                :: hruIdVarID        ! hruId varID in netcdf file
 integer(i4b)                :: gruIdVarID        ! gruId varID in netcdf file
 integer(i4b)                :: iGRU              ! counter for GRUs
 integer(i4b)                :: iHRU              ! counter for HRUs

 ! initialize error control
 err=0; message='write_hru_info/'

 ! allow re-definition of variables
 err = nf90_redef(ncid); call netcdf_err(err, message); if (err/=nf90_NoErr) return

 ! define HRU var
 err = nf90_def_var(ncid, trim(hru_DimName), nf90_int64, hru_DimID, hruVarID);     if (err/=nf90_NoErr) then; message=trim(message)//'nf90_define_hruVar'  ;  call netcdf_err(err,message); return; end if
 err = nf90_put_att(ncid, hruVarID, 'long_name', 'hruId in the input file'); if (err/=nf90_NoErr) then; message=trim(message)//'write_hruVar_longname'; call netcdf_err(err,message); return; end if
 err = nf90_put_att(ncid, hruVarID, 'units',     '-'                          ); if (err/=nf90_NoErr) then; message=trim(message)//'write_hruVar_unit';     call netcdf_err(err,message); return; end if

 ! define GRU var
 err = nf90_def_var(ncid, trim(gru_DimName), nf90_int64, gru_DimID, gruVarID);     if (err/=nf90_NoErr) then; message=trim(message)//'nf90_define_gruVar'  ;  call netcdf_err(err,message); return; end if
 err = nf90_put_att(ncid, gruVarID, 'long_name', 'gruId in the input file'); if (err/=nf90_NoErr) then; message=trim(message)//'write_gruVar_longname'; call netcdf_err(err,message); return; end if
 err = nf90_put_att(ncid, gruVarID, 'units',     '-'                          ); if (err/=nf90_NoErr) then; message=trim(message)//'write_gruVar_unit';     call netcdf_err(err,message); return; end if

! define hruId var
 err = nf90_def_var(ncid, 'hruId', nf90_int64, hru_DimID, hruIdVarID);     if (err/=nf90_NoErr) then; message=trim(message)//'nf90_define_hruIdVar' ; call netcdf_err(err,message); return; end if
 err = nf90_put_att(ncid, hruIdVarID, 'long_name', 'ID defining the hydrologic response unit'); if (err/=nf90_NoErr) then; message=trim(message)//'write_hruIdVar_longname'; call netcdf_err(err,message); return; end if
 err = nf90_put_att(ncid, hruIdVarID, 'units',     '-'                  ); if (err/=nf90_NoErr) then; message=trim(message)//'write_hruIdVar_unit';   call netcdf_err(err,message); return; end if

 ! define gruId var
 err = nf90_def_var(ncid, 'gruId', nf90_int64, gru_DimID, gruIdVarID);     if (err/=nf90_NoErr) then; message=trim(message)//'nf90_define_gruIdVar' ; call netcdf_err(err,message); return; end if
 err = nf90_put_att(ncid, gruIdVarID, 'long_name', 'ID defining the grouped (basin) response unit'); if (err/=nf90_NoErr) then; message=trim(message)//'write_gruIdVar_longname'; call netcdf_err(err,message); return; end if
 err = nf90_put_att(ncid, gruIdVarID, 'units',     '-'                  ); if (err/=nf90_NoErr) then; message=trim(message)//'write_gruIdVar_unit';   call netcdf_err(err,message); return; end if

 ! Leave define mode of NetCDF files
 err = nf90_enddef(ncid);  message=trim(message)//'nf90_enddef/'; call netcdf_err(err,message); if (err/=nf90_NoErr) return

 ! write the 'hru' and 'gru' records from the input netcdf file, and hruId and gruId
 do iGRU = 1, size(gru_struc)
 
   ! GRU info
   err = nf90_put_var(ncid, gruVarID, gru_struc(iGRU)%gru_id, start=(/iGRU/))
   if (err/=nf90_NoErr) then; message=trim(message)//'nf90_write_gruVar/'; call netcdf_err(err,message); return; end if
   err = nf90_put_var(ncid, gruIdVarID, gru_struc(iGRU)%gru_id, start=(/iGRU/))
   if (err/=nf90_NoErr) then; message=trim(message)//'nf90_write_gruIdVar/'; call netcdf_err(err,message); return; end if

!  ! HRU info
   do iHRU = 1, gru_struc(iGRU)%hruCount
      err = nf90_put_var(ncid, hruVarID, gru_struc(iGRU)%hruInfo(iHRU)%hru_id, start=(/gru_struc(iGRU)%hruInfo(iHRU)%hru_ix/))
      if (err/=nf90_NoErr) then; message=trim(message)//'nf90_write_hruVar/'; call netcdf_err(err,message); return; end if
      err = nf90_put_var(ncid, hruIdVarID, gru_struc(iGRU)%hruInfo(iHRU)%hru_id, start=(/gru_struc(iGRU)%hruInfo(iHRU)%hru_ix/))
      if (err/=nf90_NoErr) then; message=trim(message)//'nf90_write_hruIdVar/'; call netcdf_err(err,message); return; end if
   end do
 end do

 end subroutine

end module def_output_actors_module