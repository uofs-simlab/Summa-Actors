 ! *********************************************************************************************************
    ! public subroutine printRestartFile: print a re-start file
    ! *********************************************************************************************************
    subroutine writeRestart(filename,         & ! intent(in): name of restart file
        nGRU,             & ! intent(in): number of GRUs
        nHRU,             & ! intent(in): number of HRUs
        prog_meta,        & ! intent(in): prognostics metadata
        prog_data,        & ! intent(in): prognostics data
        bvar_meta,        & ! intent(in): basin (gru) variable metadata
        bvar_data,        & ! intent(in): basin (gru) variable data
        maxLayers,        & ! intent(in): maximum number of layers
        maxSnowLayers,    & ! intent(in): maximum number of snow layers
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
USE globalData,only:nTimeDelay             ! number of timesteps in the time delay histogram

implicit none
! --------------------------------------------------------------------------------------------------------
! input
character(len=256),intent(in)      :: filename      ! name of the restart file
integer(i4b),intent(in)            :: nGRU          ! number of GRUs
integer(i4b),intent(in)            :: nHRU          ! number of HRUs
type(var_info),intent(in)          :: prog_meta(:)  ! prognostic variable metadata
type(gru_hru_doubleVec),intent(in) :: prog_data     ! prognostic vars
type(var_info),intent(in)          :: bvar_meta(:)  ! basin variable metadata
type(gru_doubleVec),intent(in)     :: bvar_data     ! basin variables
type(var_info),intent(in)          :: indx_meta(:)  ! metadata
type(gru_hru_intVec),intent(in)    :: indx_data     ! indexing vars
! output: error control
integer(i4b),intent(out)           :: err           ! error code
character(*),intent(out)           :: message       ! error message
! --------------------------------------------------------------------------------------------------------
! dummy variables
integer(i4b), intent(in)           :: maxLayers     ! maximum number of total layers
integer(i4b), intent(in)           :: maxSnowLayers ! maximum number of snow layers

! local variables
integer(i4b)                       :: ncid          ! netcdf file id
integer(i4b),allocatable           :: ncVarID(:)    ! netcdf variable id
integer(i4b)                       :: ncSnowID      ! index variable id
integer(i4b)                       :: ncSoilID      ! index variable id

integer(i4b)                       :: nSoil         ! number of soil layers
integer(i4b)                       :: nSnow         ! number of snow layers
integer(i4b)                       :: maxSnow       ! maximum number of snow layers
integer(i4b)                       :: maxSoil       ! maximum number of soil layers
integer(i4b)                       :: nLayers       ! number of total layers
integer(i4b),parameter             :: nSpectral=2   ! number of spectal bands
integer(i4b),parameter             :: nScalar=1     ! size of a scalar
integer(i4b)                       :: nProgVars     ! number of prognostic variables written to state file

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
logical(lgt)                       :: okLength      ! flag to check if the vector length is OK
character(len=256)                 :: cmessage      ! downstream error message
! --------------------------------------------------------------------------------------------------------

! initialize error control
err=0; message='writeRestart/'

! size of prognostic variable vector
nProgVars = size(prog_meta)
allocate(ncVarID(nProgVars+1))     ! include 1 additional basin variable in ID array (possibly more later)

! maximum number of soil layers
maxSoil = gru_struc(1)%hruInfo(1)%nSoil

! maximum number of snow layers
maxSnow = maxSnowLayers

! create file
err = nf90_create(trim(filename),nf90_classic_model,ncid)
message='iCreate[create]'; call netcdf_err(err,message); if(err/=0)return

! define dimensions
err = nf90_def_dim(ncid,trim(hruDimName)    ,nHRU       ,    hruDimID); message='iCreate[hru]'     ; call netcdf_err(err,message); if(err/=0)return
err = nf90_def_dim(ncid,trim(gruDimName)    ,nGRU       ,    gruDimID); message='iCreate[gru]'     ; call netcdf_err(err,message); if(err/=0)return
err = nf90_def_dim(ncid,trim(tdhDimName)    ,nTimeDelay ,    tdhDimID); message='iCreate[tdh]'     ; call netcdf_err(err,message); if(err/=0)return
err = nf90_def_dim(ncid,trim(scalDimName)   ,nScalar    ,   scalDimID); message='iCreate[scalar]'  ; call netcdf_err(err,message); if(err/=0)return
err = nf90_def_dim(ncid,trim(specDimName)   ,nSpectral  ,   specDimID); message='iCreate[spectral]'; call netcdf_err(err,message); if(err/=0)return
err = nf90_def_dim(ncid,trim(midSoilDimName),maxSoil    ,midSoilDimID); message='iCreate[ifcSoil]' ; call netcdf_err(err,message); if(err/=0)return
err = nf90_def_dim(ncid,trim(midTotoDimName),maxLayers  ,midTotoDimID); message='iCreate[midToto]' ; call netcdf_err(err,message); if(err/=0)return
err = nf90_def_dim(ncid,trim(ifcSoilDimName),maxSoil+1  ,ifcSoilDimID); message='iCreate[ifcSoil]' ; call netcdf_err(err,message); if(err/=0)return
err = nf90_def_dim(ncid,trim(ifcTotoDimName),maxLayers+1,ifcTotoDimID); message='iCreate[ifcToto]' ; call netcdf_err(err,message); if(err/=0)return
if (maxSnow>0) err = nf90_def_dim(ncid,trim(midSnowDimName),maxSnow    ,midSnowDimID); message='iCreate[ifcSnow]' ; call netcdf_err(err,message); if(err/=0)return
if (maxSnow>0) err = nf90_def_dim(ncid,trim(ifcSnowDimName),maxSnow+1  ,ifcSnowDimID); message='iCreate[ifcSnow]' ; call netcdf_err(err,message); if(err/=0)return
! re-initialize error control
err=0; message='writeRestart/'

! define prognostic variables
do iVar = 1,nProgVars
if (prog_meta(iVar)%varType==iLookvarType%unknown) cycle

! define variable
select case(prog_meta(iVar)%varType)
case(iLookvarType%scalarv);                err = nf90_def_var(ncid,trim(prog_meta(iVar)%varname),nf90_double,(/hruDimID,  scalDimID /),ncVarID(iVar))
case(iLookvarType%wLength);                err = nf90_def_var(ncid,trim(prog_meta(iVar)%varname),nf90_double,(/hruDimID,  specDimID /),ncVarID(iVar))
case(iLookvarType%midSoil);                err = nf90_def_var(ncid,trim(prog_meta(iVar)%varname),nf90_double,(/hruDimID,midSoilDimID/),ncVarID(iVar))
case(iLookvarType%midToto);                err = nf90_def_var(ncid,trim(prog_meta(iVar)%varname),nf90_double,(/hruDimID,midTotoDimID/),ncVarID(iVar))
case(iLookvarType%ifcSoil);                err = nf90_def_var(ncid,trim(prog_meta(iVar)%varname),nf90_double,(/hruDimID,ifcSoilDimID/),ncVarID(iVar))
case(iLookvarType%ifcToto);                err = nf90_def_var(ncid,trim(prog_meta(iVar)%varname),nf90_double,(/hruDimID,ifcTotoDimID/),ncVarID(iVar))
case(iLookvarType%midSnow); if (maxSnow>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varname),nf90_double,(/hruDimID,midSnowDimID/),ncVarID(iVar))
case(iLookvarType%ifcSnow); if (maxSnow>0) err = nf90_def_var(ncid,trim(prog_meta(iVar)%varname),nf90_double,(/hruDimID,ifcSnowDimID/),ncVarID(iVar))
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
err = nf90_def_var(ncid,trim(indx_meta(iLookIndex%nSnow)%varName),nf90_int,(/hruDimID/),ncSnowID); call netcdf_err(err,message)
err = nf90_put_att(ncid,ncSnowID,'long_name',trim(indx_meta(iLookIndex%nSnow)%vardesc));           call netcdf_err(err,message)
err = nf90_put_att(ncid,ncSnowID,'units'    ,trim(indx_meta(iLookIndex%nSnow)%varunit));           call netcdf_err(err,message)

! define index variables - soil
err = nf90_def_var(ncid,trim(indx_meta(iLookIndex%nSoil)%varName),nf90_int,(/hruDimID/),ncSoilID); call netcdf_err(err,message)
err = nf90_put_att(ncid,ncSoilID,'long_name',trim(indx_meta(iLookIndex%nSoil)%vardesc));           call netcdf_err(err,message)
err = nf90_put_att(ncid,ncSoilID,'units'    ,trim(indx_meta(iLookIndex%nSoil)%varunit));           call netcdf_err(err,message)

! end definition phase
err = nf90_enddef(ncid); call netcdf_err(err,message); if (err/=0) return

! write variables
do iGRU = 1,nGRU
do iHRU = 1,gru_struc(iGRU)%hruCount
cHRU = gru_struc(iGRU)%hruInfo(iHRU)%hru_ix
do iVar = 1,size(prog_meta)

! excape if this variable is not used
if (prog_meta(iVar)%varType==iLookvarType%unknown) cycle

! actual number of layers
nSnow = gru_struc(iGRU)%hruInfo(iHRU)%nSnow
nSoil = gru_struc(iGRU)%hruInfo(iHRU)%nSoil
nLayers = nSoil + nSnow

! check size
! NOTE: this may take time that we do not wish to use
okLength=.true.
select case (prog_meta(iVar)%varType)
case(iLookVarType%scalarv);              okLength = (size(prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat) == nScalar  )
case(iLookVarType%wlength);              okLength = (size(prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat) == nSpectral)
case(iLookVarType%midSoil);              okLength = (size(prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat) == nSoil    )
case(iLookVarType%midToto);              okLength = (size(prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat) == nLayers  )
case(iLookVarType%ifcSoil);              okLength = (size(prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat) == nSoil+1  )
case(iLookVarType%ifcToto);              okLength = (size(prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat) == nLayers+1)
case(iLookVarType%midSnow); if (nSnow>0) okLength = (size(prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat) == nSnow    )
case(iLookVarType%ifcSnow); if (nSnow>0) okLength = (size(prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat) == nSnow+1  )
case default; err=20; message=trim(message)//'unknown var type'; return
end select

! error check
if(.not.okLength)then
message=trim(message)//'bad vector length for variable '//trim(prog_meta(iVar)%varname)
err=20; return
endif

! write data
select case (prog_meta(iVar)%varType)
case(iLookVarType%scalarv);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat/),start=(/cHRU,1/),count=(/1,nScalar  /))
case(iLookVarType%wlength);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat/),start=(/cHRU,1/),count=(/1,nSpectral/))
case(iLookVarType%midSoil);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat/),start=(/cHRU,1/),count=(/1,nSoil    /))
case(iLookVarType%midToto);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat/),start=(/cHRU,1/),count=(/1,nLayers  /))
case(iLookVarType%ifcSoil);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat/),start=(/cHRU,1/),count=(/1,nSoil+1  /))
case(iLookVarType%ifcToto);              err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat/),start=(/cHRU,1/),count=(/1,nLayers+1/))
case(iLookVarType%midSnow); if (nSnow>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat/),start=(/cHRU,1/),count=(/1,nSnow    /))
case(iLookVarType%ifcSnow); if (nSnow>0) err=nf90_put_var(ncid,ncVarID(iVar),(/prog_data%gru(iGRU)%hru(iHRU)%var(iVar)%dat/),start=(/cHRU,1/),count=(/1,nSnow+1  /))
case default; err=20; message=trim(message)//'unknown var type'; return
end select

! error check
if (err.ne.0) message=trim(message)//'writing variable:'//trim(prog_meta(iVar)%varName)
call netcdf_err(err,message); if (err/=0) return
err=0; message='writeRestart/'

end do ! iVar loop

! write index variables
err=nf90_put_var(ncid,ncSnowID,(/indx_data%gru(iGRU)%hru(iHRU)%var(iLookIndex%nSnow)%dat/),start=(/cHRU/),count=(/1/))
err=nf90_put_var(ncid,ncSoilID,(/indx_data%gru(iGRU)%hru(iHRU)%var(iLookIndex%nSoil)%dat/),start=(/cHRU/),count=(/1/))

end do ! iHRU loop

! write selected basin variables
err=nf90_put_var(ncid,ncVarID(nProgVars+1),(/bvar_data%gru(iGRU)%var(iLookBVAR%routingRunoffFuture)%dat/),  start=(/iGRU/),count=(/1,nTimeDelay/))

end do  ! iGRU loop

! close file
call nc_file_close(ncid,err,cmessage)
if(err/=0)then;message=trim(message)//trim(cmessage);return;end if

! cleanup
deallocate(ncVarID)

end subroutine writeRestart