module read_param_all_hru
    USE, intrinsic :: iso_c_binding
    USE nrtype
    implicit none
    private
    public::read_param_file_access_actor
    public::overwriteParam
contains


! overwriteParm sets the basin parameters
! It can then overwrite them from information from the Noah-MP tables
subroutine overwriteParam(num_gru, err) bind(C, name="overwriteParam")
    USE globalData,only:outputStructure
    USE pOverwrite_module,only:pOverwrite                       ! module to overwrite default parameter values with info from the Noah tables
    USE globalData,only:gru_struc
    USE globalData,only:localParFallback                        ! local column default parameters
    USE globalData,only:basinParFallback                        ! basin-average default parameter
    USE var_lookup,only:iLookTYPE                               ! look-up values for classification of veg, soils etc.

    implicit none
    integer(c_int),intent(in)             :: num_gru          ! number of GRUs in the run_domain
    integer(c_int),intent(out)            :: err              ! error code
    
    ! local
    integer(i4b)                          :: iGRU
    integer(i4b)                          :: iHRU
    integer(i4b)                          :: iVar
    integer(i4b)                          :: iDat
    character(len=256)                    :: message

    err=0; message="overwriteParam"

    ! Need to set the basin parameters with the default values for when we copy
    do iGRU=1,num_gru
        do iHRU=1,gru_struc(iGRU)%hruCount
            do iVar=1, size(localParFallback)
                outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(iHRU)%var(iVar) = localParFallback(iVar)%default_val
            end do
            ! overwrite default model parameters with information from the Noah-MP tables
            call pOverwrite(outputStructure(1)%typeStruct(1)%gru(iGRU)%hru(iHRU)%var(iLookTYPE%vegTypeIndex),  &  ! vegetation category
                            outputStructure(1)%typeStruct(1)%gru(iGRU)%hru(iHRU)%var(iLookTYPE%soilTypeIndex), &  ! soil category
                            outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(iHRU)%var(:), &                              ! default model parameters
                            err,message)
            
            do iVar=1, size(localParFallback)
                do iDat=1, size(outputStructure(1)%mparStruct(1)%gru(iGRU)%hru(iHRU)%var(iVar)%dat)
                    outputStructure(1)%mparStruct(1)%gru(iGRU)%hru(iHRU)%var(iVar)%dat(iDat) = outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(iHRU)%var(iVar)
                end do
            end do
        end do
        do iVar=1,size(basinParFallback)
            outputStructure(1)%bparStruct(1)%gru(iGRU)%var(iVar) = basinParFallback(iVar)%default_val
        end do
    end do

end subroutine
subroutine read_param_file_access_actor(startGRU,num_gru,err) bind(C, name="readParamFileAccessActor")
   ! used to read model initial conditions
    USE summaActors_FileManager,only:SETTINGS_PATH             ! path for metadata files
    USE summaActors_FileManager,only:PARAMETER_TRIAL           ! file with parameter trial values
    USE get_ixname_module,only:get_ixparam,get_ixbpar   ! access function to find index of elements in structure
    USE globalData,only:index_map,gru_struc             ! mapping from global HRUs to the elements in the data structures
    USE var_lookup,only:iLookPARAM,iLookTYPE,iLookID    ! named variables to index elements of the data vectors
    USE globalData,only:integerMissing  ! missing integer
    USE globalData,only:realMissing     ! missing real number

    USE netcdf
    USE netcdf_util_module,only:nc_file_close  ! close netcdf file
    USE netcdf_util_module,only:nc_file_open   ! open netcdf file
    USE netcdf_util_module,only:netcdf_err     ! netcdf error handling function

    USE globalData,only:outputStructure

    implicit none
    ! define input

    integer(c_int),intent(in)             :: startGRU         ! starting Index of gru Batch
    integer(c_int),intent(in)             :: num_gru          ! number of GRUs in the run_domain
    integer(c_int),intent(out)            :: err              ! error code
    
    ! define local variables
    character(len=256)                    :: message          ! error message
    character(len=1024)                   :: cmessage         ! error message for downwind routine
    character(LEN=1024)                   :: infile           ! input filename
    integer(i4b)                          :: localHRU_ix      ! index of HRU within data structure
    integer(i4b)                          :: ixParam          ! index of the model parameter in the data structure
    ! indices/metadata in the NetCDF file
    integer(i4b)                          :: ncid             ! netcdf id
    integer(i4b)                          :: nDims            ! number of dimensions
    integer(i4b)                          :: nVars            ! number of variables
    integer(i4b)                          :: idimid           ! dimension index
    integer(i4b)                          :: ivarid           ! variable index
    character(LEN=64)                     :: dimName          ! dimension name
    character(LEN=64)                     :: parName          ! parameter name
    integer(i4b)                          :: dimLength        ! dimension length
    integer(i4b)                          :: nHRU_file        ! number of HRUs in the parafile
    integer(i4b)                          :: nGRU_file        ! number of GRUs in the parafile
    integer(i4b)                          :: nSoil_file       ! number of soil layers in the file
    integer(i4b)                          :: idim_list(2)     ! list of dimension ids
    ! data in the netcdf file
    integer(i4b)                          :: parLength        ! length of the parameter data
    integer(8),allocatable                :: hruId(:)         ! HRU identifier in the file
    real(dp),allocatable                  :: parVector(:)     ! model parameter vector
    logical                               :: fexist           ! inquire whether the parmTrial file exists
    integer(i4b)                          :: fHRU             ! index of HRU in input file
    integer(i4b)                          :: iGRU
    integer(i4b)                          :: iHRU

    err=0; message="read_param_all_hru.f90/"    
    ! **********************************************************************************************
    ! * open files, etc.
    ! **********************************************************************************************
    
    infile = trim(SETTINGS_PATH)//trim(PARAMETER_TRIAL) ! build filename

    ! check whether the user-specified file exists and warn if it does not
    inquire(file=trim(infile),exist=fexist)
    if (.not.fexist) then
        write(*,'(A)') NEW_LINE('A')//'!! WARNING:  trial parameter file not found; proceeding instead with other default parameters; check path in file manager input if this was not the desired behavior'//NEW_LINE('A')
        return
    endif

    ! open trial parameters file if it exists
    call nc_file_open(trim(infile),nf90_nowrite,ncid,err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; end if

    ! get the number of variables in the parameter file
    err=nf90_inquire(ncid, nDimensions=nDims, nVariables=nVars)
    call netcdf_err(err,message); if (err/=0) then; err=20; return; end if

    ! initialize the number of HRUs
    nHRU_file=integerMissing
    nGRU_file=integerMissing

    ! get the length of the dimensions
    do idimid=1,nDims
        ! get the dimension name and length
        err=nf90_inquire_dimension(ncid, idimid, name=dimName, len=dimLength)
        if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
        ! get the number of HRUs
        if(trim(dimName)=='hru') nHRU_file=dimLength
        if(trim(dimName)=='gru') nGRU_file=dimLength
    end do

    ! allocate hruID vector
    allocate(hruId(nHRU_file))

    ! check HRU dimension exists
    if(nHRU_file==integerMissing)then
        message=trim(message)//'unable to identify HRU dimension in file '//trim(infile)
        err=20; return
    endif
    ! **********************************************************************************************
   ! * read the HRU index
   ! **********************************************************************************************
   ! loop through the parameters in the NetCDF file
   do ivarid=1,nVars

        ! get the parameter name
        err=nf90_inquire_variable(ncid, ivarid, name=parName)
        call netcdf_err(err,message)
        if (err/=0) then 
            err=20
            print*, message
            return
        end if

        ! special case of the HRU id
        if(trim(parName)=='hruIndex' .or. trim(parName)=='hruId')then

            ! read HRUs
            err=nf90_get_var(ncid, ivarid, hruId)
            if(err/=0)then
                message=trim(message)//trim(cmessage)
                print*, message
                return
            end if

        endif   ! if the HRU id

    end do  ! looping through variables in the file

   ! **********************************************************************************************
   ! * read the local parameters and the basin parameters
   ! ********************************************************************************************** 
   do ivarid=1,nVars
        ! get the parameter name
        err=nf90_inquire_variable(ncid, ivarid, name=parName)
        call netcdf_err(err,message); if (err/=0) then; err=20; return; end if
    
        ! get the local parameters
        ixParam = get_ixparam( trim(parName) )
        if(ixParam/=integerMissing)then
            ! **********************************************************************************************
            ! * read the local parameters
            ! **********************************************************************************************
            
            ! get the variable shape
            err=nf90_inquire_variable(ncid, ivarid, nDims=nDims, dimids=idim_list)
            if(err/=0)then
                message=trim(message)//trim(cmessage)
                print*, message
                return
            end if
            
            ! get the length of the depth dimension (if it exists)
            if(nDims==2)then
                ! get the information on the 2nd dimension for 2-d variables
                err=nf90_inquire_dimension(ncid, idim_list(2), dimName, nSoil_file)
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
                
                ! TODO: implement this check
                ! ! check that the dimension length is correct
                ! if(size(outputStructur(1)%mparStruct%var(ixParam)%dat) /= nSoil_file)then
                !     message=trim(message)//'unexpected number of soil layers in parameter file'
                !     err=20; return
                ! endif

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
            
            ! loop through all hrus
            do iGRU=1, num_gru
                do iHRU=1, gru_struc(iGRU)%hruCount
                    localHRU_ix=index_map(iHRU)%localHRU_ix
                    fHRU = gru_struc(iGRU)%hruInfo(localHRU_ix)%hru_nc
                    ! read parameter data
                    select case(nDims)
                        case(1); err=nf90_get_var(ncid, ivarid, parVector, start=(/fHRU/), count=(/1/) )
                        case(2); err=nf90_get_var(ncid, ivarid, parVector, start=(/fHRU,1/), count=(/1,nSoil_file/) )
                        case default; err=20; message=trim(message)//'unexpected number of dimensions for parameter '//trim(parName)
                    end select
                    
                    ! error check for the parameter read
                    if(err/=0)then
                        message=trim(message)//trim(cmessage)
                        print*, message
                        return
                    end if
                    
                     ! populate parameter structures
                    select case(nDims)
                        case(1); outputStructure(1)%mparStruct(1)%gru(iGRU)%hru(localHRU_ix)%var(ixParam)%dat(:) = parVector(1)  ! also distributes scalar across depth dimension
                        case(2); outputStructure(1)%mparStruct(1)%gru(iGRU)%hru(localHRU_ix)%var(ixParam)%dat(:) = parVector(:)
                        case default; err=20; message=trim(message)//'unexpected number of dimensions for parameter '//trim(parName)
                    end select
                end do
            end do
            
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

            ! allocate space for model parameters
            allocate(parVector(nGRU_file),stat=err)
            if(err/=0)then
                message=trim(message)//'problem allocating space for parameter vector'
                print*, message
                err=20; return
            endif

            ! read parameter data
            err=nf90_get_var(ncid, ivarid, parVector )
            if(err/=0)then; message=trim(message)//trim(cmessage); return; end if

            ! populate parameter structures
            do iGRU=1, num_gru
                outputStructure(1)%bparStruct(1)%gru(iGRU)%var(ixParam) = parVector(iGRU+startGRU-1)
            end do
            
            ! deallocate space for model parameters
            deallocate(parVector,stat=err)
            if(err/=0)then
                message=trim(message)//'problem deallocating space for parameter vector'
                print*, message
                err=20; return
            endif
        endif

   end do ! (looping through the parameters in the NetCDF file)
   
   ! Now we must close the netcdf file
   call nc_file_close(ncid,err,message)
   if(err/=0)then;message=trim(message)//trim(cmessage);return;end if
end subroutine read_param_file_access_actor
end module read_param_all_hru