module read_initcond_module
USE, intrinsic :: iso_c_binding
USE nrtype


implicit none
private
public :: openInitCondFile
public :: closeInitCondFile
public :: readInitCond_prog
public :: readInitCond_bvar

! define single HRU restart file
integer(i4b), parameter :: singleHRU=1001
integer(i4b), parameter :: multiHRU=1002
integer(i4b), parameter :: restartFileType=multiHRU
contains

subroutine openInitCondFile(init_cond_ncid, err) bind(C, name="openInitCondFile")
  USE netcdf_util_module,only:nc_file_open               ! open netcdf file
  USE netcdf
  ! file paths
  USE summaFileManager,only:SETTINGS_PATH           ! path to settings files (e.g., Noah vegetation tables)
  USE summaFileManager,only:STATE_PATH              ! optional path to state/init. condition files (defaults to SETTINGS_PATH)
  USE summaFileManager,only:MODEL_INITCOND          ! name of model initial conditions file
  implicit none

  integer(c_int),intent(out)          :: init_cond_ncid
  integer(c_int),intent(out)          :: err

  character(len=256)                  :: init_cond_fileName
  character(len=256)                  :: message          ! error message
  character(len=1024)                 :: cmessage         ! error message for downwind routine
  ! --------------------------------------------------------------------------------------------------------

  ! define restart file path/name
  if(STATE_PATH == '') then
    init_cond_fileName = trim(SETTINGS_PATH)//trim(MODEL_INITCOND)
  else
    init_cond_fileName = trim(STATE_PATH)//trim(MODEL_INITCOND)
  endif

  call nc_file_open(init_cond_fileName,nf90_nowrite,init_cond_ncid,err,cmessage)
  if (err/=0) then
    message=trim(message)//trim(cmessage)
    print(message)
    return
  end if

end subroutine openInitCondFile

subroutine closeInitCondFile(init_cond_ncid,err) bind(C, name="closeInitCondFile")
  USE netcdf_util_module,only:nc_file_close
  implicit none
  
  integer(c_int),intent(out)          :: init_cond_ncid
  integer(c_int),intent(out)          :: err
  
  ! local variables
  character(len=256)                  :: message
  ! --------------------------------------------------------------------------------------------------------
  err=0; message="read_initcond.f90 - closeInitCondFile/"

  call nc_file_close(init_cond_ncid,err,message)
  if (err/=0) then
    message=trim(message)
    print(message)
    return
  end if

end subroutine closeInitCondFile

subroutine readInitCond_prog(init_cond_ncid, start_gru, num_gru, err) bind(C, name="readInitCond_prog")
  ! Structures to populate
  USE globalData,only:init_cond_prog
  ! Netcdf
  USE netcdf
  USE netcdf_util_module,only:nc_file_close              ! close netcdf file
  USE netcdf_util_module,only:netcdf_err                 ! netcdf error handling

  ! metadata
  USE globalData,only:prog_meta                     ! metadata for prognostic variables
  USE globalData,only:bvar_meta                     ! metadata for basin (GRU) variables
  ! var_lookup
  USE var_lookup,only:iLookVarType                  ! variable lookup structure
  USE var_lookup,only:iLookPROG                     ! variable lookup structure
  USE var_lookup,only:iLookPARAM                    ! variable lookup structure
  USE var_lookup,only:iLookINDEX                    ! variable lookup structure
  ! access type string
  USE get_ixName_module,only:get_varTypeName        ! to access type strings for error messages
  implicit none

  integer(c_int), intent(in)             :: init_cond_ncid
  integer(c_int), intent(in)             :: start_gru
  integer(c_int), intent(in)             :: num_gru
  integer(c_int), intent(out)            :: err
 
  ! local variables
  integer(i4b)                           :: iVar
  integer(i4b)                           :: dimID        ! varible dimension ids
  integer(i4b)                           :: ncVarID      ! variable ID in netcdf file
  integer(i4b)                           :: dimLen       ! data dimensions
  character(256)                         :: dimName      ! not used except as a placeholder in call to inq_dim function
  integer(i4b)                           :: fileHRU      ! number of HRUs in file

  character(LEN=256)                     :: icond_file  ! restart/icond_file file name
  character(len=256)                     :: message
  character(len=256)                     :: cmessage

  character(len=32),parameter            :: scalDimName   ='scalarv'  ! dimension name for scalar data
  character(len=32),parameter            :: midSoilDimName='midSoil'  ! dimension name for soil-only layers
  character(len=32),parameter            :: midTotoDimName='midToto'  ! dimension name for layered varaiables
  character(len=32),parameter            :: ifcTotoDimName='ifcToto'  ! dimension name for layered varaiables
  ! --------------------------------------------------------------------------------------------------------

  err=0; message="read_initcond.f90 - readInitCond_prog"
  
  ! get number of HRUs in file
  err = nf90_inq_dimid(init_cond_ncid,"hru",dimID);
  if(err/=nf90_noerr)then; message=trim(message)//'problem finding hru dimension/'//trim(nf90_strerror(err)); return; end if
  err = nf90_inquire_dimension(init_cond_ncid,dimID,len=fileHRU);
  if(err/=nf90_noerr)then; message=trim(message)//'problem reading hru dimension/'//trim(nf90_strerror(err)); return; end if

  allocate(init_cond_prog(size(prog_meta)))

  ! loop through prognostic variables
  do iVar = 1,size(prog_meta)
    ! skip variables that are computed later
    if(prog_meta(iVar)%varName=='scalarCanopyWat'           .or. &
       prog_meta(iVar)%varName=='spectralSnowAlbedoDiffuse' .or. &
       prog_meta(iVar)%varName=='scalarSurfaceTemp'         .or. &
       prog_meta(iVar)%varName=='mLayerVolFracWat'          .or. &
       prog_meta(iVar)%varName=='mLayerHeight'                   )then 
       cycle
    endif

    ! get variable id
    err = nf90_inq_varid(init_cond_ncid,trim(prog_meta(iVar)%varName),ncVarID); call netcdf_err(err,message)
    if(err/=0)then
      message=trim(message)//': problem with getting variable id, var='//trim(prog_meta(iVar)%varName)
      print*,message
      return
    endif

    ! get variable dimension IDs
    select case (prog_meta(iVar)%varType)
      case (iLookVarType%scalarv); err = nf90_inq_dimid(init_cond_ncid,trim(scalDimName)   ,dimID)
        call netcdf_err(err,message)
      case (iLookVarType%midSoil); err = nf90_inq_dimid(init_cond_ncid,trim(midSoilDimName),dimID)
        call netcdf_err(err,message)
      case (iLookVarType%midToto); err = nf90_inq_dimid(init_cond_ncid,trim(midTotoDimName),dimID)
        call netcdf_err(err,message)
      case (iLookVarType%ifcToto); err = nf90_inq_dimid(init_cond_ncid,trim(ifcTotoDimName),dimID)
        call netcdf_err(err,message)
    case default
      message=trim(message)//"unexpectedVariableType[name='"//trim(prog_meta(iVar)%varName)//"';type='"//trim(get_varTypeName(prog_meta(iVar)%varType))//"']"
      print*, message
      err=20; return
    end select

    ! check errors
    if(err/=0)then
      message=trim(message)//': problem with dimension ids, var='//trim(prog_meta(iVar)%varName)
      print*, message
      return
    endif

    ! get the dimension length
    err = nf90_inquire_dimension(init_cond_ncid,dimID,dimName,dimLen); call netcdf_err(err,message)
    if(err/=0)then;message=trim(message)//': problem getting the dimension length';print*, message;return;endif

    ! initialize the variable data
    allocate(init_cond_prog(iVar)%var_data(num_gru,dimLen),stat=err)
    if(err/=0)then;message=trim(message)//'problem allocating HRU variable data';print*, message;return;endif

     ! get data
    err = nf90_get_var(init_cond_ncid,ncVarID,init_cond_prog(iVar)%var_data, start=(/start_gru,1/),count=(/num_gru,dimLen/)) 
    call netcdf_err(err,message)
    if(err/=0)then; message=trim(message)//': problem getting the data for variable '//trim(prog_meta(iVar)%varName); return; endif

  end do

end subroutine readInitCond_prog

subroutine readInitCond_bvar(init_cond_ncid, start_gru, num_gru, err) bind(C, name="readInitCond_bvar")
  USE globalData,only:init_cond_bvar
  USE globalData,only: nTimeDelay   ! number of hours in the time delay histogram
  ! var_lookup
  USE var_lookup,only:iLookBVAR                     ! variable lookup structure
  ! metadata structures
  USE globalData,only:bvar_meta                     ! metadata for basin (GRU) variables
  ! netcdf
  USE netcdf
  USE netcdf_util_module,only:netcdf_err                 ! netcdf error handling
  implicit none
  
  integer(c_int), intent(in)             :: init_cond_ncid
  integer(c_int), intent(in)             :: start_gru
  integer(c_int), intent(in)             :: num_gru
  integer(c_int), intent(out)            :: err

  ! local variables
  integer(i4b)                           :: nTDH          ! number of points in time-delay histogram
  integer(i4b)                           :: dimID        ! varible dimension ids
  integer(i4b)                           :: fileGRU      ! number of GRUs in file
  integer(i4b)                           :: i          
  integer(i4b)                           :: iVar     
  integer(i4b)                           :: dimLen       ! data dimensions
  character(256)                         :: dimName      ! not used except as a placeholder in call to inq_dim function
  integer(i4b)                           :: ncVarID      ! variable ID in netcdf file
  integer(i4b),dimension(1)              :: ndx          ! intermediate array of loop indices
   

  character(len=256)                     :: message
  
  character(len=32),parameter            :: tdhDimName    ='tdh'      ! dimension name for time-delay basin variables

  ! --------------------------------------------------------------------------------------------------------
  err = 0; message="read_initcond.f90 - readInitCond_bvar/"
  if(restartFileType/=singleHRU)then
    ! get dimension of time delay histogram (TDH) from initial conditions file
    err = nf90_inq_dimid(init_cond_ncid,"tdh",dimID);
    
    if(err/=nf90_noerr)then
      write(*,*) 'WARNING: routingRunoffFuture is not in the initial conditions file ... using zeros'  ! previously created in var_derive.f90
      err=nf90_noerr    ! reset this err
    
    else 
      ! the state file *does* have the basin variable(s), so process them
      err = nf90_inquire_dimension(init_cond_ncid,dimID,len=nTDH);
      if(err/=nf90_noerr)then
        message=trim(message)//'problem reading tdh dimension from initial condition file/'//trim(nf90_strerror(err))
        print*, message
        return
      end if
      
      ! get number of GRUs in file
      err = nf90_inq_dimid(init_cond_ncid,"gru",dimID);               if(err/=nf90_noerr)then; message=trim(message)//'problem finding gru dimension/'//trim(nf90_strerror(err)); return; end if
      err = nf90_inquire_dimension(init_cond_ncid,dimID,len=fileGRU); if(err/=nf90_noerr)then; message=trim(message)//'problem reading gru dimension/'//trim(nf90_strerror(err)); return; end if

      ! check vs hardwired value set in globalData.f90
      if(nTDH /= nTimeDelay)then
        write(*,*) 'tdh=',nTDH,' nTimeDelay=',nTimeDelay
        message=trim(message)//': state file time delay dimension tdh does not match summa expectation of nTimeDelay set in globalData()'
        return
      endif

      ndx = (/iLookBVAR%routingRunoffFuture/)   ! array of desired variable indices
      allocate(init_cond_bvar(size(ndx)))
      do i = 1, size(ndx)
        iVar = ndx(i)
        ! get tdh dimension Id in file (should be 'tdh')
        err = nf90_inq_dimid(init_cond_ncid,trim(tdhDimName), dimID);
        if(err/=0)then;message=trim(message)//': problem with dimension ids for tdh vars';print*,message;return;endif

        ! get the tdh dimension length (dimName and dimLen are outputs of this call)
        err = nf90_inquire_dimension(init_cond_ncid,dimID,dimName,dimLen); call netcdf_err(err,message)
        if(err/=0)then;message=trim(message)//': problem getting the dimension length for tdh vars';print*,message;return;endif

        ! get tdh-based variable id
        err = nf90_inq_varid(init_cond_ncid,trim(bvar_meta(iVar)%varName),ncVarID); call netcdf_err(err,message)
        if(err/=0)then; message=trim(message)//': problem with getting basin variable id, var='//trim(bvar_meta(iVar)%varName); return; endif

        allocate(init_cond_bvar(i)%var_data(num_gru,dimLen),stat=err)
        if(err/=0)then; print*, 'err= ',err; message=trim(message)//'problem allocating GRU variable data'; return; endif

        ! get data
        err = nf90_get_var(init_cond_ncid,ncVarID,init_cond_bvar(i)%var_data, start=(/start_gru,1/),count=(/num_gru,dimLen/)); call netcdf_err(err,message)
        if(err/=0)then; message=trim(message)//': problem getting the data'; return; endif
      end do
    endif
  endif


end subroutine readInitCond_bvar




end module read_initcond_module