module read_param_module
  USE, intrinsic :: iso_c_binding
  USE nrtype
  implicit none
  private
  public::allocateParamStructures
  public::openParamFile
  public::getNumVarParam
  public::closeParamFile
  public::getParamSizes
  public::overwriteParam
  public::readParamFromNetCDF
  contains
subroutine allocateParamStructures(index_gru, index_hru, handle_dpar_struct, &
    handle_mpar_struct, handle_bpar_struct, err) bind(C, name="allocateParamStructures")
  
  USE globalData,only:mpar_meta,bpar_meta
  USE globalData,only:gru_struc
  USE data_types,only:var_dlength,var_d
  USE allocspace_module,only:allocLocal

  implicit none
  integer(c_int),intent(in)       :: index_gru
  integer(c_int),intent(in)       :: index_hru
  type(c_ptr),intent(in),value    :: handle_dpar_struct
  type(c_ptr),intent(in),value    :: handle_mpar_struct
  type(c_ptr),intent(in),value    :: handle_bpar_struct
  integer(c_int),intent(out)      :: err

  type(var_d), pointer            :: dpar_struct
  type(var_dlength), pointer      :: mpar_struct
  type(var_d), pointer            :: bpar_struct

  integer(i4b)                    :: nSnow  
  integer(i4b)                    :: nSoil

  character(len=256)              :: message

  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_dpar_struct, dpar_struct)
  call c_f_pointer(handle_mpar_struct, mpar_struct)
  call c_f_pointer(handle_bpar_struct, bpar_struct)
  ! start of subroutine
  err=0; message="read_attribute.f90 - allocateAttributeStructures"

  nSnow = gru_struc(index_gru)%hruInfo(index_hru)%nSnow
  nSoil = gru_struc(index_gru)%hruInfo(index_hru)%nSoil

  ! initalize the structure with allocatable components
  call allocLocal(mpar_meta,dpar_struct,nSnow,nSoil,err,message); 
  call allocLocal(mpar_meta,mpar_struct,nSnow,nSoil,err,message); 
  call allocLocal(bpar_meta,bpar_struct,nSnow=0,nSoil=0,err=err,message=message); 
  if(err/=0)then; message=trim(message); print*, message; return; endif;
end subroutine

subroutine openParamFile(param_ncid, param_file_exists, err) bind(C, name="openParamFile")
  USE netcdf
  USE netcdf_util_module,only:nc_file_open
  ! Parameters File
  USE summaFileManager,only:SETTINGS_PATH             ! path for metadata files
  USE summaFileManager,only:PARAMETER_TRIAL           ! file with parameter trial values
  
  implicit none
  integer(c_int),intent(out)          :: param_ncid
  logical(c_bool),intent(out)         :: param_file_exists
  integer(c_int),intent(out)          :: err
  
  ! local variables
  character(LEN=1024)                 :: infile           ! input filename
  character(len=256)                  :: message          ! error message
  character(len=1024)                 :: cmessage         ! error message for downwind routine
  err=0; message="read_param.f90 - openParamFile"    
  ! **********************************************************************************************
  ! * open files, etc.
  ! **********************************************************************************************
  
  infile = trim(SETTINGS_PATH)//trim(PARAMETER_TRIAL) ! build filename

  ! check whether the user-specified file exists and warn if it does not
  inquire(file=trim(infile),exist=param_file_exists)
  if (.not.param_file_exists) then
      write(*,'(A)') NEW_LINE('A')//'!! WARNING:  trial parameter file not found; proceeding instead with other default parameters; check path in file manager input if this was not the desired behavior'//NEW_LINE('A')
      return
  endif

  ! open trial parameters file if it exists
  call nc_file_open(trim(infile),nf90_nowrite,param_ncid,err,cmessage)
  if(err/=0)then
    message=trim(message)//trim(cmessage)
    print*, message
    return
  end if

end subroutine openParamFile

subroutine getNumVarParam(param_ncid, num_var, err) bind(C, name="getNumVarParam")
  USE netcdf
  USE netcdf_util_module,only:netcdf_err                     ! netcdf error handling function
  implicit none
  integer(c_int),intent(in)            :: param_ncid
  integer(c_int),intent(out)           :: num_var
  integer(c_int),intent(out)           :: err

  ! local variables
  character(len=256)                   :: message       ! error message
  
  err=0; message="read_param.f90 - getNumVarAndDims/"
  ! get the number of variables in the parameter file
  err=nf90_inquire(param_ncid, nVariables=num_var)
  call netcdf_err(err,message)
  if (err/=0) then
    err=20
    print*, message
    return
  end if

end subroutine getNumVarParam

subroutine closeParamFile(param_ncid, err) bind(C, name="closeParamFile")
  USE netcdf_util_module,only:nc_file_close
  implicit none
  integer(c_int),intent(in)            :: param_ncid
  integer(c_int),intent(out)           :: err
  ! local variables
  character(len=256)                   :: message
  ! --------------------------------------------------------------------------------------------------------
 
  err=0; message="read_param.f90 - closeParamFile/"

  call nc_file_close(param_ncid,err,message)
  if(err/=0)then
    message=trim(message)
    print*, message
    return
  end if

end subroutine closeParamFile

! get the sizes of the arrays for dpar_array bpar_array
subroutine getParamSizes(dpar_array_size, bpar_array_size, type_array_size) bind(C, name="getParamSizes")
  USE var_lookup,only:maxvarMpar      ! model parameters: maximum number variables
  USE var_lookup,only:maxvarBpar      ! model parameters: maximum number variables
  USE var_lookup,only:maxvarType

  implicit none
  integer(c_int),intent(out)    :: dpar_array_size
  integer(c_int),intent(out)    :: bpar_array_size
  integer(c_int),intent(out)    :: type_array_size


  dpar_array_size = maxvarMpar
  bpar_array_size = maxvarBpar
  type_array_size = maxvarType


end subroutine getParamSizes

subroutine overwriteParam(index_gru, index_hru, handle_type_struct, &
  handle_dpar_struct, handle_mpar_struct, handle_bpar_struct, err) bind(C, name="overwriteParam")
  USE var_lookup,only:maxvarMpar      ! model parameters: maximum number variables
  USE var_lookup,only:maxvarBpar      ! model parameters: maximum number variables
  USE var_lookup,only:iLookTYPE      ! named variables to index elements of the data vectors
  ! global data
  USE globalData,only:gru_struc
  USE globalData,only:localParFallback                        ! local column default parameters
  USE globalData,only:basinParFallback                        ! basin-average default parameter
  USE data_types,only:var_dlength,var_i,var_d

  USE pOverwrite_module,only:pOverwrite                       ! module to overwrite default parameter values with info from the Noah tables
  USE allocspace_module,only:allocLocal
  implicit none
  integer(c_int),intent(in)     :: index_gru
  integer(c_int),intent(in)     :: index_hru
  ! structures
  type(c_ptr),intent(in),value  :: handle_type_struct
  type(c_ptr),intent(in),value  :: handle_dpar_struct
  type(c_ptr),intent(in),value  :: handle_mpar_struct
  type(c_ptr),intent(in),value  :: handle_bpar_struct

  ! error control
  integer(c_int), intent(out)   :: err

  ! local variables
  type(var_i),pointer           :: type_struct                 !  model parameters
  type(var_d),pointer           :: dpar_struct                 !  model parameters
  type(var_dlength),pointer     :: mpar_struct                 !  model parameters
  type(var_d),pointer           :: bpar_struct                 !  model parameters

  integer(i4b)                  :: iVar
  integer(i4b)                  :: iDat

  character(len=256)            :: message
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_type_struct, type_struct)
  call c_f_pointer(handle_dpar_struct, dpar_struct)
  call c_f_pointer(handle_mpar_struct, mpar_struct)
  call c_f_pointer(handle_bpar_struct, bpar_struct)
  ! Start subroutine
  err=0; message="read_param.f90 - overwriteParam"

  ! Set the basin parameters with the default values
  do ivar=1, size(localParFallback)
    dpar_struct%var(iVar) = localParFallback(iVar)%default_val
  end do

  call pOverwrite(type_struct%var(iLookTYPE%vegTypeIndex), &  ! vegetation category
                  type_struct%var(iLookTYPE%soilTypeIndex),&  ! soil category
                  dpar_struct%var(:),err,message)             ! default model parameters
  
  do ivar=1, size(localParFallback)
    do iDat=1, size(mpar_struct%var(iVar)%dat)
      mpar_struct%var(iVar)%dat(iDat) = dpar_struct%var(iVar)
    end do
  end do

  do iVar=1, size(basinParFallback)
    bpar_struct%var(iVar) = basinParFallback(iVar)%default_val
  end do

end subroutine overwriteParam


subroutine readParamFromNetCDF(param_ncid, index_gru, index_hru, start_index_gru, &
    num_vars, handle_mpar_struct, handle_bpar_struct, err) bind(C, name="readParamFromNetCDF")
  USE netcdf
  USE netcdf_util_module,only:netcdf_err     ! netcdf error handling function

  USE data_types,only:var_dlength,var_d
  USE get_ixname_module,only:get_ixparam,get_ixbpar   ! access function to find index of elements in structure
  
  USE globalData,only:index_map,gru_struc             ! mapping from global HRUs to the elements in the data structures
  USE globalData,only:integerMissing  ! missing integer
  
  implicit none
  ! dummy variables
  integer(c_int),intent(in)     :: param_ncid
  integer(c_int),intent(in)     :: index_gru
  integer(c_int),intent(in)     :: index_hru
  integer(c_int),intent(in)     :: start_index_gru
  integer(c_int),intent(in)     :: num_vars
  type(c_ptr),intent(in),value  :: handle_mpar_struct
  type(c_ptr),intent(in),value  :: handle_bpar_struct
  integer(c_int), intent(out)   :: err
  ! define local variables
  type(var_dlength),pointer     :: mpar_struct                 !  model parameters
  type(var_d),pointer           :: bpar_struct                 !  model parameters

  character(len=256)            :: message          ! error message
  character(len=1024)           :: cmessage         ! error message for downwind routine
  integer(i4b)                  :: localHRU_ix      ! index of HRU within data structure
  integer(i4b)                  :: ixParam          ! index of the model parameter in the data structure
  
  ! indices/metadata in the NetCDF file
  integer(i4b)                  :: num_dims            ! number of dimensions

  integer(i4b)                  :: ivarid           ! variable index
  character(LEN=64)             :: dimName          ! dimension name

  character(LEN=64)             :: parName          ! parameter name
  integer(i4b)                  :: nSoil_file       ! number of soil layers in the file
  integer(i4b)                  :: idim_list(2)     ! list of dimension ids
  ! data in the netcdf file
  integer(i4b)                  :: parLength        ! length of the parameter data
  real(dp),allocatable          :: parVector(:)     ! model parameter vector
  integer(i4b)                  :: fHRU             ! index of HRU in input file
  integer(i4b)                  :: netcdf_index

  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_mpar_struct, mpar_struct)
  call c_f_pointer(handle_bpar_struct, bpar_struct)
  err=0; message="read_param.f90 - readParamFromNetCDF/"


  ! **********************************************************************************************
  ! * read the local parameters and the basin parameters
  ! ********************************************************************************************** 
  do ivarid=1,num_vars
    ! get the parameter name
    err=nf90_inquire_variable(param_ncid, ivarid, name=parName)
    call netcdf_err(err,message)
    if (err/=0) then
      err=20
      print*,message
      return
    end if

    ! get the local parameters
    ixParam = get_ixparam( trim(parName) )
    if(ixParam/=integerMissing)then
      ! **********************************************************************************************
      ! * read the local parameters
      ! **********************************************************************************************
      ! get the variable shape
      err=nf90_inquire_variable(param_ncid, ivarid, nDims=num_dims, dimids=idim_list)
      if(err/=0)then
          message=trim(message)//trim(cmessage)
          print*, message
          return
      end if
        
        ! get the length of the depth dimension (if it exists)
        if(num_dims==2)then
            ! get the information on the 2nd dimension for 2-d variables
            err=nf90_inquire_dimension(param_ncid, idim_list(2), dimName, nSoil_file)
            if(err/=0)then
                message=trim(message)//trim(cmessage)
                print*, message
                return
            end if
            
            ! check that it is the depth dimension
            if(trim(dimName)/='depth')then
                message=trim(message)//'expect 2nd dimension of 2-d variable to be depth (dimension name = '//trim(dimName)//')'
                err=20; return
            endif

            ! ! check that the dimension length is correct
            if(size(mpar_struct%var(ixParam)%dat) /= nSoil_file)then
                message=trim(message)//'unexpected number of soil layers in parameter file'
                err=20; return
            endif

            ! define parameter length
            parLength = nSoil_file
        else
            parLength = 1
        endif ! if two dimensions

        ! allocate space for model parameters
        allocate(parVector(parLength),stat=err)
        if(err/=0)then
            message=trim(message)//'problem allocating space for parameter vector'
            err=20; return
        endif
        

        localHRU_ix=index_map(index_hru)%localHRU_ix
        fHRU = gru_struc(index_gru)%hruInfo(localHRU_ix)%hru_nc
        ! read parameter data
        select case(num_dims)
            case(1); err=nf90_get_var(param_ncid, ivarid, parVector, start=(/fHRU/), count=(/1/) )
            case(2); err=nf90_get_var(param_ncid, ivarid, parVector, start=(/fHRU,1/), count=(/1,nSoil_file/) )
            case default; err=20; message=trim(message)//'unexpected number of dimensions for parameter '//trim(parName)
        end select
          
        ! error check for the parameter read
        if(err/=0)then
            message=trim(message)//trim(cmessage)
            print*, message
            return
        end if
          
          ! populate parameter structures
        select case(num_dims)
            case(1); mpar_struct%var(ixParam)%dat(:) = parVector(1)  ! also distributes scalar across depth dimension
            case(2); mpar_struct%var(ixParam)%dat(:) = parVector(:)
            case default; err=20; message=trim(message)//'unexpected number of dimensions for parameter '//trim(parName)
        end select

        ! deallocate space for model parameters
        deallocate(parVector,stat=err)
        if(err/=0)then
            message=trim(message)//'problem deallocating space for parameter vector'
            print*, message
            err=20; return
        endif
    
    ! **********************************************************************************************
    ! * read the basin parameters
    ! **********************************************************************************************
    
    ! get the basin parameters
    else
        ! get the parameter index
        ixParam = get_ixbpar( trim(parName) )

        ! allow extra variables in the file that are not used
        if(ixParam==integerMissing) cycle

        ! read parameter data
        netcdf_index = start_index_gru + index_gru - 1
        err=nf90_get_var(param_ncid, ivarid, bpar_struct%var(ixParam), start=(/netcdf_index/))
        if(err/=0)then
          message=trim(message)//trim(cmessage)
          print*, message
          return
        end if
    endif

  end do ! (looping through the parameters in the NetCDF file)

end subroutine readParamFromNetCDF


end module read_param_module