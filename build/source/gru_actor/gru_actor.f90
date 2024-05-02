module gru_actor
USE,intrinsic :: iso_c_binding
USE nrtype


implicit none

public :: getHruInfo_fortran


contains

subroutine getHruInfo_fortran(indx_gru, num_hru) bind(C, name="getHruInfo_fortran")
  USE globalData,only:gru_struc
  USE summa_init_struc,only:init_struc
  USE var_lookup,only:iLookTYPE          ! look-up values for classification of veg, soils etc.
  USE var_lookup,only:iLookID            ! look-up values for hru and gru IDs
  implicit none
  ! Dummy variables
  integer(c_int), intent(in)  :: indx_gru
  integer(c_int), intent(out) :: num_hru
  ! local variables
  integer(i4b)                :: kHRU
  integer(i4b)                :: jHRU
  integer(i4b)                :: iHRU

  num_hru = gru_struc(indx_gru)%hruCount
end subroutine getHruInfo_fortran


end module gru_actor
