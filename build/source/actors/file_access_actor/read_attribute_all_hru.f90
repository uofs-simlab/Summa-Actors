module read_attribute_all_hru
    USE, intrinsic :: iso_c_binding
    USE nrtype
    implicit none
    private
    public::read_attribute_file_access_actor
contains
subroutine read_attribute_file_access_actor(num_gru,err) bind(C, name="readAttributeFileAccessActor")
    USE globalData,only:outputStructure                     ! Using the output structure as global input for the attribute data. This is so we can hrus can setup params in parallel.
    USE netcdf
    USE netcdf_util_module,only:nc_file_open                   ! open netcdf file
    USE netcdf_util_module,only:nc_file_close                  ! close netcdf file
    USE netcdf_util_module,only:netcdf_err                     ! netcdf error handling function
    ! provide access to derived data types
    USE data_types,only:var_d                            ! x%var(:)     (i4b)
    USE data_types,only:var_i                            ! x%var(:)     integer(8)
    USE data_types,only:var_i8                           ! x%var(:)     (dp)
    ! provide access to global data
    USE globalData,only:gru_struc                              ! gru-hru mapping structure
    USE globalData,only:attr_meta,type_meta,id_meta            ! metadata structures
    USE get_ixname_module,only:get_ixAttr,get_ixType,get_ixId  ! access function to find index of elements in structure
    ! Attribute File
    USE summaActors_FileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
    USE summaActors_FileManager,only:LOCAL_ATTRIBUTES                  ! name of model initial attributes file

    
    implicit none
   
    integer(c_int),intent(in)            :: num_gru            ! id of the HRU
    integer(c_int),intent(out)           :: err                ! error code

    ! Local Variables
    character(len=256)                      :: message            ! error message
    character(len=256)                   :: cmessage           ! error message for downwind routine
    integer(i4b)                         :: iVar               ! loop through varibles in the netcdf file
    integer(i4b)                         :: varType            ! type of variable (categorica, numerical, idrelated)
    integer(i4b)                         :: varIndx            ! index of variable within its data structure
   
    ! check structures
    integer(i4b)                         :: iCheck             ! index of an attribute name
    logical(lgt),allocatable             :: checkType(:)       ! vector to check if we have all desired categorical values
    logical(lgt),allocatable             :: checkId(:)         ! vector to check if we have all desired IDs
    logical(lgt),allocatable             :: checkAttr(:)       ! vector to check if we have all desired local attributes
   
    ! netcdf variables
    integer(i4b)                         :: ncID               ! netcdf file id
    character(LEN=nf90_max_name)         :: varName            ! character array of netcdf variable name
    integer(i4b)                         :: nVar               ! number of variables in netcdf local attribute file
    integer(i4b),parameter               :: categorical=101    ! named variable to denote categorical data
    integer(i4b),parameter               :: numerical=102      ! named variable to denote numerical data
    integer(i4b),parameter               :: idrelated=103      ! named variable to denote ID related data
    integer(i4b)                         :: categorical_var(1) ! temporary categorical variable from local attributes netcdf file
    real(dp)                             :: numeric_var(1)     ! temporary numeric variable from local attributes netcdf file
    integer(8)                           :: idrelated_var(1)   ! temporary ID related variable from local attributes netcdf file
    
    integer(i4b)                         :: iGRU
    integer(i4b)                         :: iHRU
    ! attribute file
    character(len=256)                    :: attrFile           ! attributes file name


    ! define mapping variables
   
    ! Start procedure here
    err=0; message="read_attriute_all_hru "

    attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)

   
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
   
    ! **********************************************************************************************
    ! (2) open netcdf file
    ! **********************************************************************************************
    ! open file
    call nc_file_open(trim(attrFile),nf90_noWrite,ncID,err,cmessage)
    if(err/=0)then
        message=trim(message)//trim(cmessage)
        print*, message
        return
    endif
   
    ! get number of variables total in netcdf file
    err = nf90_inquire(ncID,nvariables=nVar)
    call netcdf_err(err,message)
    if (err/=0) then
        message=trim(message)//'problem with nf90_inquire'
        return
    endif
    ! **********************************************************************************************
    ! (3) read local attributes
    ! **********************************************************************************************
    ! loop through variables in netcdf file and pull out local attributes
    iCheck = 1
    do iVar = 1,nVar
   
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
        

                do iGRU=1,num_gru
                    do iHRU = 1,gru_struc(iGRU)%hruCount
                        err = nf90_get_var(ncID,iVar,categorical_var,start=(/gru_struc(iGRU)%hruInfo(iHRU)%hru_nc/),count=(/1/))
                        if(err/=nf90_noerr)then
                            message=trim(message)//'problem reading: '//trim(varName)
                            print*, message
                            return
                        end if
                        outputStructure(1)%typeStruct(1)%gru(iGRU)%hru(iHRU)%var(varIndx) = categorical_var(1)
                    end do
                end do
   
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
                do iGRU=1,num_gru
                    do iHRU = 1,gru_struc(iGRU)%hruCount
                        err = nf90_get_var(ncID,iVar,idrelated_var,start=(/gru_struc(iGRU)%hruInfo(iHRU)%hru_nc/),count=(/1/))
                        if(err/=nf90_noerr)then
                            message=trim(message)//'problem reading: '//trim(varName)
                            print*, message
                            return
                        end if
                        outputStructure(1)%idStruct(1)%gru(iGRU)%hru(iHRU)%var(varIndx) = idrelated_var(1)
                    end do
                end do
    
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
                
                do iGRU=1,num_gru
                    do iHRU = 1,gru_struc(iGRU)%hruCount        
                        err = nf90_get_var(ncID,iVar,numeric_var,start=(/gru_struc(iGRU)%hruInfo(iHRU)%hru_nc/),count=(/1/))
                        if(err/=nf90_noerr)then
                            message=trim(message)//'problem reading: '//trim(varName)
                            print*, message
                            return
                        end if
                        outputStructure(1)%attrStruct(1)%gru(iGRU)%hru(iHRU)%var(varIndx) = numeric_var(1)
                    end do
                end do
            
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
        do iGRU=1,num_gru
            do iHRU = 1, gru_struc(iGRU)%hruCount
                outputStructure(1)%attrStruct(1)%gru(iGRU)%hru(iHRU)%var(varIndx) = nr_realMissing      ! populate variable with out-of-range value, used later
            end do
        end do
        checkAttr(varIndx) = .true.
    endif
   
    ! TODO: find out why this is here, probably for the lateral flows
    varIndx = get_ixTYPE('downkHRU')
    checkType(varIndx) = .true.
    ! outputStructure(1)%typeStruct(1)%gru(iGRU)%hru(iHRU)%var(varIndx) = 0
   
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
   
    ! **********************************************************************************************
    ! (5) close netcdf file
    ! **********************************************************************************************
   
    call nc_file_close(ncID,err,cmessage)
    if (err/=0)then; message=trim(message)//trim(cmessage); return; end if
   
    ! free memory
    deallocate(checkType)
    deallocate(checkId)
    deallocate(checkAttr)
   
end subroutine


end module