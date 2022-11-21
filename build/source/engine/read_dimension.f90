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

module read_dimension_module
USE, intrinsic :: iso_c_binding
USE nrtype
implicit none
private
public::read_dimension
! public::read_attribute
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
  USE summaFileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaFileManager,only:LOCAL_ATTRIBUTES                  ! name of model initial attributes file


  implicit none

  ! Dummy Variables
  
  integer(c_int),intent(in)              :: numGRUs            ! number of GRUs for the run domain
  integer(c_int),intent(out)             :: numHRUs            ! number of HRUs for the run domain (value filled in this subroutine)
  integer(c_int),intent(in)              :: startGRU           ! Index of the starting GRU
  integer(c_int),intent(out)             :: err                ! error code
  
  ! Local Variables
  character(len=256)                     :: attrFile           ! name of attributed file
  integer(i4b)                           :: fileGRU            ! number of GRUs in the input file
  integer(i4b)                           :: fileHRU            ! number of HRUs in the input file
  integer(i4b)                           :: iHRU               ! HRU couinting index
  integer(i4b)                           :: iGRU               ! GRU loop index
  integer(8),allocatable                 :: gru_id(:),hru_id(:)! read gru/hru IDs in from attributes file
  integer(8),allocatable                 :: hru2gru_id(:)      ! read hru->gru mapping in from attributes file
  integer(i4b),allocatable               :: hru_ix(:)          ! hru index for search
  character(len=256)                     :: message            ! error message


  ! define variables for NetCDF file operation
  integer(i4b)                           :: ncID               ! NetCDF file ID
  integer(i4b)                           :: varID              ! NetCDF variable ID
  integer(i4b)                           :: gruDimId           ! variable id of GRU dimension from netcdf file
  integer(i4b)                           :: hruDimId           ! variable id of HRU dimension from netcdf file
  character(len=256)                     :: cmessage           ! error message for downwind routine

  err=0; message="read_dimension/"
  attrFile = trim(SETTINGS_PATH)//trim(LOCAL_ATTRIBUTES)

  ! open nc file
  call nc_file_open(trim(attrFile),nf90_noWrite,ncID,err,cmessage)
  if(err/=0)then
    message=trim(message)//trim(cmessage) 
    print*, message
    print*, attrFile
    return
  end if
  
  ! *********************************************************************************************
  ! read and set GRU dimensions
  ! **********************************************************************************************
  ! get gru dimension of whole file
  err = nf90_inq_dimid(ncID,"gru",gruDimId)
  if(err/=nf90_noerr)then
    message=trim(message)//'problem finding gru dimension/'//trim(nf90_strerror(err))
    print*, message
    return
  end if

  err = nf90_inquire_dimension(ncID, gruDimId, len = fileGRU)
  if(err/=nf90_noerr)then; 
    message=trim(message)//'problem reading gru dimension/'//trim(nf90_strerror(err))
    print*, message
    return
  end if

  ! get hru dimension of whole file
  err = nf90_inq_dimid(ncID,"hru",hruDimId)
  if(err/=nf90_noerr)then
    message=trim(message)//'problem finding hru dimension/'//trim(nf90_strerror(err))
    print*, message
    return
  end if

  err = nf90_inquire_dimension(ncID, hruDimId, len = fileHRU)
  if(err/=nf90_noerr)then
    message=trim(message)//'problem reading hru dimension/'//trim(nf90_strerror(err))
    print*, message
    return
  end if

  ! check dimensions
  if(numGRUs > fileGRU .or. numGRUs < 1) then; 
    err=20
    message=trim(message)//"numGRUs is out of range"
    print*, message
    return
  end if

  ! *********************************************************************************************
  ! read mapping vectors and populate mapping structures
  ! **********************************************************************************************
  ! allocate space for GRU indices and HRU indices
  allocate(gru_id(fileGRU))
  allocate(hru_ix(fileHRU),hru_id(fileHRU),hru2gru_id(fileHRU))

  ! read gru_id from netcdf file
  err = nf90_inq_varid(ncID,"gruId",varID)
  if (err/=0) then
    message=trim(message)//'problem finding gruId'
    print*, message
    return
  end if

  err = nf90_get_var(ncID,varID,gru_id)
  if (err/=0) then
    message=trim(message)//'problem reading gruId'
    print*, message
    return
  end if

  ! read hru_id from netcdf file
  err = nf90_inq_varid(ncID,"hruId",varID)
  if (err/=0) then
    message=trim(message)//'problem finding hruId'
    print*, message
    return
  end if

  err = nf90_get_var(ncID,varID,hru_id)
  if (err/=0) then
    message=trim(message)//'problem reading hruId'
    print*, message
    return
  end if

  ! read hru2gru_id from netcdf file
  err = nf90_inq_varid(ncID,"hru2gruId",varID)
  if (err/=0) then
    message=trim(message)//'problem finding hru2gruId'
    print*, message
    return
  end if

  err = nf90_get_var(ncID,varID,hru2gru_id)
  if (err/=0) then
    message=trim(message)//'problem reading hru2gruId'
    print*, message
    return
  end if
  
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

end module read_dimension_module
