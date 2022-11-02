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

module read_param4chm_module

! missing values
USE globalData,only:integerMissing  ! missing integer
USE globalData,only:realMissing     ! missing real number

! runtime options
USE globalData,only:iRunModeFull,iRunModeGRU,iRunModeHRU ! run modes

! common modules
USE nrtype
USE netcdf
USE netcdf_util_module,only:nc_file_close  ! close netcdf file
USE netcdf_util_module,only:nc_file_open   ! open netcdf file
USE netcdf_util_module,only:netcdf_err     ! netcdf error handling function

! data types
USE data_types,only:var_d                  ! spatial double data type:  x%gru(:)%var(:)
USE data_types,only:var_i8           ! spatial integer data type: x%gru(:)%hru(:)%var(:)
USE data_types,only:var_dlength      ! spatial double data type:  x%gru(:)%hru(:)%var(:)%dat(:)

implicit none
private
public::read_param
contains


! ************************************************************************************************
! public subroutine read_param4chm: read trial model parameter values
! ************************************************************************************************
subroutine read_param(indxHRU,indxGRU,mparStruct,bparStruct,dparStruct,err)
   USE globalData,only:outputStructure
   USE globalData,only:mpar_meta,bpar_meta

   implicit none
   ! define input
   integer(i4b),intent(in)               :: indxHRU   
   integer(i4b),intent(in)               :: indxGRU
   ! define output
   type(var_dlength),intent(inout)       :: mparStruct       ! model parameters
   type(var_d),intent(inout)             :: bparStruct       ! basin parameters
   type(var_d),intent(inout)             :: dparStruct       ! default model parameters
   integer(i4b),intent(out)              :: err              ! error code
   ! 
   character(len=256)                    :: message          ! error message
   integer(i4b)                          :: iVar             ! 

   ! Start procedure here
   err=0; message="read_paramActors.f90/"

   dparStruct%var(:) = outputStructure(1)%dparStruct(1)%gru(indxGRU)%hru(indxHRU)%var(:)

   ! populate parameter structures
   do iVar=1, size(mpar_meta)
      mparStruct%var(iVar)%dat(:) = outputStructure(1)%mparStruct(1)%gru(indxGRU)%hru(indxHRU)%var(iVar)%dat(:)
   end do

   do iVar=1, size(bpar_meta)
      bparStruct%var(iVar) = outputStructure(1)%bparStruct(1)%gru(indxGRU)%var(iVar) 
   end do

 end subroutine read_param

end module read_param4chm_module
