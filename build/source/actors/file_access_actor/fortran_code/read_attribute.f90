module read_attribute_module
USE, intrinsic :: iso_c_binding
USE nrtype

implicit none
private
public::allocateAttributeStructures
public::openAttributeFile
public::getNumVarAttr
public::closeAttributeFile
public::readAttributeFromNetCDF

contains

subroutine allocateAttributeStructures(index_gru, index_hru, & ! indexes into gru_struc
    handle_attr_struct, handle_type_struct, handle_id_struct, err) bind(C, name="allocateAttributeStructures")
  USE data_types,only:var_d, var_i, var_i8
  USE globalData,only:gru_struc
  USE globalData,only:attr_meta,type_meta,id_meta
  USE allocspace_module,only:allocLocal
  implicit none
  integer(c_int),intent(in)           :: index_gru
  integer(c_int),intent(in)           :: index_hru
  type(c_ptr), intent(in), value      :: handle_attr_struct
  type(c_ptr), intent(in), value      :: handle_type_struct
  type(c_ptr), intent(in), value      :: handle_id_struct
  integer(c_int), intent(out)         :: err
  type(var_d), pointer                :: attr_struct
  type(var_i), pointer                :: type_struct
  type(var_i8), pointer               :: id_struct
  integer(i4b)                        :: nSoil
  integer(i4b)                        :: nSnow
  character(len=256)                  :: message
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_attr_struct, attr_struct)
  call c_f_pointer(handle_type_struct, type_struct)
  call c_f_pointer(handle_id_struct,   id_struct)
  ! Start subroutine
  err=0; message="read_attribute.f90 - allocateAttributeStructures"

  nSnow = gru_struc(index_gru)%hruInfo(index_hru)%nSnow
  nSoil = gru_struc(index_gru)%hruInfo(index_hru)%nSoil

  call allocLocal(attr_meta,attr_struct,nSnow,nSoil,err,message);
  call allocLocal(type_meta,type_struct,nSnow,nSoil,err,message);
  call allocLocal(id_meta,id_struct,nSnow,nSoil,err,message);
  if(err/=0)then; message=trim(message); print*, message; return; endif;
  
end subroutine allocateAttributeStructures


subroutine openAttributeFile(attr_ncid, err) bind(C, name="openAttributeFile")
  USE netcdf
  USE netcdf_util_module,only:nc_file_open                   ! open netcdf file
  ! Attribute File
  USE summaActors_FileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaActors_FileManager,only:LOCAL_ATTRIBUTES                  ! name of model initial attributes file
  implicit none
  integer(c_int),intent(out)                :: attr_ncid
  integer(c_int),intent(out)                :: err

  ! local variables
  character(len=256)                        :: message       ! error message
  character(len=256)                        :: attrFile           ! attributes file name

  err=0; message="read_attribute.f90 - openAttributesFile"
  attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)

  call nc_file_open(trim(attrFile),nf90_noWrite,attr_ncid,err,message)
  if(err/=0)then
      message=trim(message)
      print*, message
      return
  endif

end subroutine

subroutine getNumVarAttr(attr_ncid, num_var, err) bind(C, name="getNumVarAttr")
  USE netcdf
  USE netcdf_util_module,only:netcdf_err                     ! netcdf error handling function
  implicit none
  integer(c_int),intent(in)                 :: attr_ncid
  integer(c_int),intent(out)                :: num_var
  integer(c_int),intent(out)                :: err

  ! local variables
  character(len=256)                        :: message       ! error message
  err=0; message="read_attribute.f90 - getNumVar"
  ! get number of variables total in netcdf file
  err = nf90_inquire(attr_ncid,nvariables=num_var)
  call netcdf_err(err,message)
  if (err/=0) then
      message=trim(message)//'problem with nf90_inquire'
      return
  endif

end subroutine getNumVarAttr

subroutine closeAttributeFile(attr_ncid, err) bind(C, name="closeAttributeFile")
  USE netcdf_util_module,only:nc_file_close
  implicit none
  integer(c_int),intent(in)         :: attr_ncid
  integer(c_int),intent(out)        :: err
  ! local variables
  character(len=256)                :: message
  err=0; message="read_attribute.f90 - closeAttributeFile"

  call nc_file_close(attr_ncid,err,message)
  if (err/=0)then
    message=trim(message)
    return
  end if

end subroutine closeAttributeFile



! Read in the local attributes for an HRU
subroutine readAttributeFromNetCDF(ncid, index_gru, index_hru, num_var, &
  handle_attr_struct, handle_type_struct, handle_id_struct, err) bind(C, name="readAttributeFromNetCDF")
  ! netcdf utilities
  USE netcdf
  USE netcdf_util_module,only:nc_file_open                   ! open netcdf file
  USE netcdf_util_module,only:nc_file_close                  ! close netcdf file
  USE nr_utility_module ,only:arth
  ! needed global data structures and metadata
  USE globalData,only:gru_struc                              ! gru-hru mapping structure
  USE globalData,only:attr_meta,type_meta,id_meta            ! metadata structures
  USE get_ixname_module,only:get_ixAttr,get_ixType,get_ixId  ! access function to find index of elements in structure
  ! Information to make up the attributes file
  USE summaActors_FileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaActors_FileManager,only:LOCAL_ATTRIBUTES                  ! name of model initial attributes file
  ! Fortran Data Type Structures
  USE data_types,only:var_d, var_i, var_i8
  implicit none
  ! indexes into gru_struc
  integer(c_int), intent(in)            :: ncid
  integer(c_int), intent(in)            :: index_gru
  integer(c_int), intent(in)            :: index_hru
  ! number of variables from the netCDF file
  integer(c_int), intent(in)            :: num_var
  ! data structures to populate
  type(c_ptr), intent(in), value        :: handle_attr_struct
  type(c_ptr), intent(in), value        :: handle_type_struct
  type(c_ptr), intent(in), value        :: handle_id_struct
  ! error control
  integer(c_int), intent(out)           :: err
  ! local variables
  integer(i4b)                          :: iVar               ! loop through varibles in the netcdf file
  integer(i4b)                          :: varType            ! type of variable (categorica, numerical, idrelated)
  integer(i4b)                          :: varIndx            ! index of variable within its data structure
  ! Fortran structures
  type(var_d), pointer                  :: attr_struct
  type(var_i), pointer                  :: type_struct
  type(var_i8), pointer                 :: id_struct
  ! check structures - to verify input
  integer(i4b)                          :: iCheck             ! index of an attribute name
  logical(lgt),allocatable              :: checkType(:)       ! vector to check if we have all desired categorical values
  logical(lgt),allocatable              :: checkId(:)         ! vector to check if we have all desired IDs
  logical(lgt),allocatable              :: checkAttr(:)       ! vector to check if we have all desired local attributes
  ! netcdf variables
  character(LEN=nf90_max_name)          :: varName            ! character array of netcdf variable name
  integer(i4b),parameter                :: categorical=101    ! named variable to denote categorical data
  integer(i4b),parameter                :: numerical=102      ! named variable to denote numerical data
  integer(i4b),parameter                :: idrelated=103      ! named variable to denote ID related data
  integer(i4b)                          :: categorical_var(1) ! temporary categorical variable from local attributes netcdf file
  real(rkind)                           :: numeric_var(1)     ! temporary numeric variable from local attributes netcdf file
  integer(8)                            :: idrelated_var(1)   ! temporary ID related variable from local attributes netcdf file
  character(len=256)                    :: attr_file          ! attributes file name
  character(len=256)                    :: message           
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_attr_struct, attr_struct)
  call c_f_pointer(handle_type_struct, type_struct)
  call c_f_pointer(handle_id_struct,   id_struct)

  err=0; message="read_attribute_file_access_actor - read_attribute.f90"

  attr_file= trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)

  ! **********************************************************************************************
  ! (1) prepare check vectors
  ! **********************************************************************************************
  allocate(checkType(size(type_meta)),checkAttr(size(attr_meta)),checkId(size(id_meta)),stat=err)
  if(err/=0)then
      err=20
      message=trim(message)//'problem allocating space for variable check vectors'
      print*, message
      return
  endif
  checkType(:) = .false.
  checkAttr(:) = .false.
  checkId(:)   = .false.

  iCheck = 1
  do iVar = 1,num_var
    ! inqure about current variable name, type, number of dimensions
    err = nf90_inquire_variable(ncID,iVar,name=varName)
    if(err/=nf90_noerr)then; 
        message=trim(message)//'problem inquiring variable: '//trim(varName)//'/'//trim(nf90_strerror(err)); 
        print*, message
        return 
    endif
    
    ! find attribute name
    select case(trim(varName))
   
      ! ** categorical data
      case('vegTypeIndex','soilTypeIndex','slopeTypeIndex','downHRUindex')

          ! get the index of the variable
          varType = categorical
          varIndx = get_ixType(varName)
          checkType(varIndx) = .true.

          ! check that the variable could be identified in the data structure
          if(varIndx < 1)then
              err=20; 
              message=trim(message)//'unable to find variable ['//trim(varName)//'] in data structure'; 
              print*, message
              return; 
          endif
  

          err = nf90_get_var(ncID,iVar,categorical_var,start=(/gru_struc(index_gru)%hruInfo(index_hru)%hru_nc/),count=(/1/))
          if(err/=nf90_noerr)then
              message=trim(message)//'problem reading: '//trim(varName)
              print*, message
              return
          end if
          type_struct%var(varIndx) = categorical_var(1)

          ! ** ID related data
      case('hruId')
          ! get the index of the variable
          varType = idrelated
          varIndx = get_ixId(varName)
          checkId(varIndx) = .true.
  
          ! check that the variable could be identified in the data structure
          if(varIndx < 1)then
              err=20
              message=trim(message)//'unable to find variable ['//trim(varName)//'] in data structure'
              print*, message
              return
          endif
  
          ! get data from netcdf file and store in vector

          err = nf90_get_var(ncID,iVar,idrelated_var,start=(/gru_struc(index_gru)%hruInfo(index_hru)%hru_nc/),count=(/1/))
          if(err/=nf90_noerr)then
              message=trim(message)//'problem reading: '//trim(varName)
              print*, message
              return
          end if
          id_struct%var(varIndx) = idrelated_var(1)


      ! ** numerical data
      case('latitude','longitude','elevation','tan_slope','contourLength','HRUarea','mHeight')

          ! get the index of the variable
          varType = numerical
          varIndx = get_ixAttr(varName)
          checkAttr(varIndx) = .true.
  
          ! check that the variable could be identified in the data structure
          if(varIndx < 1)then
              err=20; message=trim(message)//'unable to find variable ['//trim(varName)//'] in data structure'
              print*, message
              return 
          endif
          ! get data from netcdf file and store in vector
                 
          err = nf90_get_var(ncID,iVar,numeric_var,start=(/gru_struc(index_gru)%hruInfo(index_hru)%hru_nc/),count=(/1/))
          if(err/=nf90_noerr)then
              message=trim(message)//'problem reading: '//trim(varName)
              print*, message
              return
          end if
          attr_struct%var(varIndx) = numeric_var(1)
    
      
      ! for mapping varibles, do nothing (information read above)
      case('hru2gruId','gruId'); cycle
  
      ! check that variables are what we expect
      case default
          message=trim(message)//'unknown variable ['//trim(varName)//'] in local attributes file'
          print*,message
          err=20
          return

    end select ! select variable

  end do ! (looping through netcdf local attribute file)

  ! ** now handle the optional aspect variable if it's missing
  varIndx = get_ixAttr('aspect')
  ! check that the variable was not found in the attribute file
  if(.not. checkAttr(varIndx)) then
      write(*,*) NEW_LINE('A')//'INFO: aspect not found in the input attribute file, continuing ...'//NEW_LINE('A')
      attr_struct%var(varIndx) = nr_realMissing      ! populate variable with out-of-range value, used later
      checkAttr(varIndx) = .true.
  endif

  varIndx = get_ixTYPE('downkHRU')
  checkType(varIndx) = .true.
  ! **********************************************************************************************
  ! (4) check that we have all the desired varaibles
  ! **********************************************************************************************
  ! check that we have all desired categorical variables
  if(any(.not.checkType))then
    do iCheck = 1,size(type_meta)
        if(.not.checkType(iCheck))then 
            err=20
            message=trim(message)//'missing variable ['//trim(type_meta(iCheck)%varname)//'] in local attributes file'
            print*, message
            return
        endif
    end do
  endif

  ! check that we have all desired ID variables
  if(any(.not.checkId))then
    do iCheck = 1,size(id_meta)
        if(.not.checkId(iCheck))then
            err=20
            message=trim(message)//'missing variable ['//trim(id_meta(iCheck)%varname)//'] in local attributes file'
            print*, message
            return 
        endif
    end do
  endif

  ! check that we have all desired local attributes
  if(any(.not.checkAttr))then
    do iCheck = 1,size(attr_meta)
        if(.not.checkAttr(iCheck))then; 
            err=20
            message=trim(message)//'missing variable ['//trim(attr_meta(iCheck)%varname)//'] in local attributes file'
            print*, message
            return 
        endif
    end do
  endif

  ! free memory
  deallocate(checkType)
  deallocate(checkId)
  deallocate(checkAttr)
end subroutine readAttributeFromNetCDF
end module read_attribute_module
