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

module read_attribute_module
USE, intrinsic :: iso_c_binding
USE nrtype
implicit none
private
public::read_dimension
public::read_attribute
contains

! ************************************************************************************************
! public subroutine read_dimension: read HRU and GRU dimension information on local attributes
! ************************************************************************************************
subroutine read_dimension(numGRUs,numHRUs,startGRU,err) bind(C, name="readDimension")

  USE netcdf
  USE netcdf_util_module,only:nc_file_open                   ! open netcdf file
  USE netcdf_util_module,only:nc_file_close                  ! close netcdf file
  USE nr_utility_module ,only:arth
  ! provide access to global data
  USE globalData,only:gru_struc                              ! gru->hru mapping structure
  USE globalData,only:index_map                              ! hru->gru mapping structure
  ! file paths for attribute file
  USE summaActors_FileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaActors_FileManager,only:LOCAL_ATTRIBUTES                  ! name of model initial attributes file


  implicit none

  ! Dummy Variables
  
  integer(i4b),intent(in)              :: numGRUs            ! number of GRUs for the run domain
  integer(i4b),intent(out)             :: numHRUs            ! number of HRUs for the run domain (value filled in this subroutine)
  integer(i4b),intent(in)              :: startGRU           ! Index of the starting GRU
  integer(i4b),intent(out)             :: err                ! error code
  
  ! Local Variables
  character(len=256)                   :: attrFile           ! name of attributed file
  integer(i4b)                         :: fileGRU            ! number of GRUs in the input file
  integer(i4b)                         :: fileHRU            ! number of HRUs in the input file
  integer(i4b)                         :: iHRU               ! HRU couinting index
  integer(i4b)                         :: iGRU               ! GRU loop index
  integer(8),allocatable               :: gru_id(:),hru_id(:)! read gru/hru IDs in from attributes file
  integer(8),allocatable               :: hru2gru_id(:)      ! read hru->gru mapping in from attributes file
  integer(i4b),allocatable             :: hru_ix(:)          ! hru index for search
  character(len=256)                   :: message            ! error message


  ! define variables for NetCDF file operation
  integer(i4b)                         :: ncID               ! NetCDF file ID
  integer(i4b)                         :: varID              ! NetCDF variable ID
  integer(i4b)                         :: gruDimId           ! variable id of GRU dimension from netcdf file
  integer(i4b)                         :: hruDimId           ! variable id of HRU dimension from netcdf file
  character(len=256)                   :: cmessage           ! error message for downwind routine

  err=0; message="read_dimension/"
  attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)

  ! open nc file
  call nc_file_open(trim(attrFile),nf90_noWrite,ncID,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
  
  ! *********************************************************************************************
  ! read and set GRU dimensions
  ! **********************************************************************************************
  ! get gru dimension of whole file
  err = nf90_inq_dimid(ncID,"gru",gruDimId);                   if(err/=nf90_noerr)then; message=trim(message)//'problem finding gru dimension/'//trim(nf90_strerror(err)); return; end if
  err = nf90_inquire_dimension(ncID, gruDimId, len = fileGRU); if(err/=nf90_noerr)then; message=trim(message)//'problem reading gru dimension/'//trim(nf90_strerror(err)); return; end if

  ! get hru dimension of whole file
  err = nf90_inq_dimid(ncID,"hru",hruDimId);                   if(err/=nf90_noerr)then; message=trim(message)//'problem finding hru dimension/'//trim(nf90_strerror(err)); return; end if
  err = nf90_inquire_dimension(ncID, hruDimId, len = fileHRU); if(err/=nf90_noerr)then; message=trim(message)//'problem reading hru dimension/'//trim(nf90_strerror(err)); return; end if

  ! check dimensions
  if(numGRUs > fileGRU .or. numGRUs < 1) then; err=20; message=trim(message)//"numGRUs is out of range"; return; end if

  ! *********************************************************************************************
  ! read mapping vectors and populate mapping structures
  ! **********************************************************************************************
  ! allocate space for GRU indices and HRU indices
  allocate(gru_id(fileGRU))
  allocate(hru_ix(fileHRU),hru_id(fileHRU),hru2gru_id(fileHRU))

  ! read gru_id from netcdf file
  err = nf90_inq_varid(ncID,"gruId",varID);     if (err/=0) then; message=trim(message)//'problem finding gruId'; return; end if
  err = nf90_get_var(ncID,varID,gru_id);        if (err/=0) then; message=trim(message)//'problem reading gruId'; return; end if

  ! read hru_id from netcdf file
  err = nf90_inq_varid(ncID,"hruId",varID);     if (err/=0) then; message=trim(message)//'problem finding hruId'; return; end if
  err = nf90_get_var(ncID,varID,hru_id);        if (err/=0) then; message=trim(message)//'problem reading hruId'; return; end if

  ! read hru2gru_id from netcdf file
  err = nf90_inq_varid(ncID,"hru2gruId",varID); if (err/=0) then; message=trim(message)//'problem finding hru2gruId'; return; end if
  err = nf90_get_var(ncID,varID,hru2gru_id);    if (err/=0) then; message=trim(message)//'problem reading hru2gruId'; return; end if
  
  ! array from 1 to total # of HRUs in attributes file
  hru_ix=arth(1,1,fileHRU)

  ! check that the mappings are not alreaday allocated
  if (allocated(gru_struc)) then
    deallocate(gru_struc)
  endif

  if (allocated(index_map)) then
    deallocate(index_map)
  endif

  ! allocate first level of gru to hru mapping
  allocate(gru_struc(numGRUs))

  ! allocate space for the run
  iHRU = 1
  do iGRU = 1,numGRUs
    if (count(hru2gru_Id == gru_id(iGRU+startGRU-1)) < 1) then; err=20; message=trim(message)//'problem finding HRUs belonging to GRU'; return; end if
    gru_struc(iGRU)%hruCount          = count(hru2gru_Id == gru_id(iGRU+startGRU-1))                 ! number of HRUs in each GRU
    gru_struc(iGRU)%gru_id            = gru_id(iGRU+startGRU-1)                                  ! set gru id
    gru_struc(iGRU)%gru_nc            = iGRU+startGRU-1                                          ! set gru index in the netcdf file

    allocate(gru_struc(iGRU)%hruInfo(gru_struc(iGRU)%hruCount))                                  ! allocate second level of gru to hru map
    gru_struc(iGRU)%hruInfo(:)%hru_nc = pack(hru_ix,hru2gru_id == gru_struc(iGRU)%gru_id)        ! set hru id in attributes netcdf file
    gru_struc(iGRU)%hruInfo(:)%hru_ix = arth(iHRU,1,gru_struc(iGRU)%hruCount)                    ! set index of hru in run domain
    gru_struc(iGRU)%hruInfo(:)%hru_id = hru_id(gru_struc(iGRU)%hruInfo(:)%hru_nc)                ! set id of hru
    iHRU = iHRU + gru_struc(iGRU)%hruCount
  end do ! iGRU = 1,nGRU

  ! set hru to gru mapping
  numHRUs = sum(gru_struc%hruCount)   ! Total number of HRUs
  allocate(index_map(numHRUs))        ! allocate first level of hru to gru mapping

  do iGRU = 1, numGRUs
    index_map(gru_struc(iGRU)%hruInfo(:)%hru_ix)%gru_ix   = iGRU                                 ! index of gru in run domain to which the hru belongs
    index_map(gru_struc(iGRU)%hruInfo(:)%hru_ix)%localHRU_ix = hru_ix(1:gru_struc(iGRU)%hruCount)! index of hru within the gru
  end do ! iGRU =1, numGRUs


  deallocate(gru_id, hru_ix, hru_id, hru2gru_id)
  ! close netcdf file
  call nc_file_close(ncID,err,cmessage)
  if (err/=0) then; message=trim(message)//trim(cmessage); return; end if

end subroutine read_dimension

subroutine read_attribute(indxHRU, indxGRU, attrFile, attrStruct, typeStruct, idStruct, err, message)
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
  ! get the settings from the output stucture so we do not have to go to file
  USE globalData,only:outputStructure 
  implicit none

  integer(i4b),intent(in)              :: indxHRU            ! id of the HRU
  integer(i4b),intent(in)              :: indxGRU            ! id of the parent GRU    
  ! io vars
  character(*)                         :: attrFile           ! input filename
  type(var_d),intent(inout)            :: attrStruct         ! local attributes for each HRU
  type(var_i),intent(inout)            :: typeStruct         ! local classification of soil veg etc. for each HRU
  type(var_i8),intent(inout)           :: idStruct           ! 
  integer(i4b),intent(out)             :: err                ! error code
  character(*),intent(out)             :: message            ! error message

  ! define local variables
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

  ! define mapping variables

  ! Start procedure here
  err=0; message="read_attribute.f90/"

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
  
  ! Copy the attribute data that was filled in read_attribute_all_hru.f90

  ! ** categorical data (typeStruct)
  do iVar = 1, size(type_meta)
    checkType(iVar) = .true.
    typeStruct%var(iVar) = outputStructure(1)%typeStruct(1)%gru(indxGRU)%hru(indxHRU)%var(iVar)
  end do

  ! ** ID related data (idStruct)
  do iVar=1, size(id_meta)
    checkId(iVar) = .true.
    idStruct%var(iVar) = outputStructure(1)%idStruct(1)%gru(indxGRU)%hru(indxHRU)%var(iVar)
  end do

  ! ** numerical data (attrStruct)
  do iVar=1, size(attr_meta)
    checkAttr(iVar) = .true.
    attrStruct%var(iVar) = outputStructure(1)%attrStruct(1)%gru(indxGRU)%hru(indxHRU)%var(iVar)
  end do
 
! TODO: downkHRU can cause issues do not know how to hanlde yet
!  varIndx = get_ixTYPE('downkHRU')
!  checkType(varIndx) = .true.
!  typeStruct%var(varIndx) = 0

 ! **********************************************************************************************
 ! (4) check that we have all the desired varaibles
 ! **********************************************************************************************
 ! check that we have all desired categorical variables
  if(any(.not.checkType))then
    do iCheck = 1,size(type_meta)
      if(.not.checkType(iCheck))then
        err=20; message=trim(message)//'missing variable ['//trim(type_meta(iCheck)%varname)//'] in local attributes file'
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
      if(.not.checkAttr(iCheck))then
        err=20
        message=trim(message)//'missing variable ['//trim(attr_meta(iCheck)%varname)//'] in local attributes file'
        return
      endif
    end do
 endif

 ! free memory
 deallocate(checkType)
 deallocate(checkId)
 deallocate(checkAttr)

end subroutine read_attribute
end module read_attribute_module
