module gru_struc_module
  USE, intrinsic :: iso_c_binding
  implicit none

  public::init_gru_struc
  public::pop_gru_struc
  contains
subroutine init_gru_struc(num_gru, file_hru, hru_ix) bind(C, name="init_gru_struc")
  USE globalData,only:gru_struc                   ! gru->hru mapping structure
  USE globalData,only:index_map                   ! hru->gru mapping structure
  USE nr_utility_module,only:arth
  implicit none
  integer(c_int), intent(in) :: num_gru
  integer(c_int), intent(in) :: file_hru
  integer(c_int), intent(out) :: hru_ix(file_hru)

  if (allocated(gru_struc)) deallocate(gru_struc)
  allocate(gru_struc(num_gru))

  hru_ix = arth(1,1,file_hru)

end subroutine init_gru_struc

subroutine pop_gru_struc(iGRU, gru_id, hru_id, hru2gru_id, hru_ix, file_gru, &
    file_hru, num_gru, start_gru) bind(C, name="pop_gru_struc")
  USE globalData,only:gru_struc                   ! gru->hru mapping structure
  USE globalData,only:index_map                   ! hru->gru mapping structure
  USE nr_utility_module,only:arth
  implicit none
  integer(c_int), intent(in)  :: iGRU
  integer(c_int), intent(in)  :: file_gru
  integer(c_int), intent(in)  :: file_hru
  integer(c_int), intent(in)  :: num_gru
  integer(c_int), intent(in)  :: start_gru
  integer(c_long), intent(in) :: gru_id(file_gru)
  integer(c_long), intent(in) :: hru_id(file_hru)
  integer(c_long), intent(in) :: hru2gru_id(file_hru)
  integer(c_int), intent(in)  :: hru_ix(file_hru)
  integer ::  iHRU

  iHRU = 1
  gru_struc(iGRU)%hruCount          = count(hru2gru_Id == gru_id(iGRU+start_gru-1))                 ! number of HRUs in each GRU
  gru_struc(iGRU)%gru_id            = gru_id(iGRU+start_gru-1)                                      ! set gru id
  gru_struc(iGRU)%gru_nc            = iGRU+start_gru-1                                              ! set gru index in the netcdf file

  allocate(gru_struc(iGRU)%hruInfo(gru_struc(iGRU)%hruCount))                                  ! allocate second level of gru to hru map
  gru_struc(iGRU)%hruInfo(:)%hru_nc = pack(hru_ix,hru2gru_id == gru_struc(iGRU)%gru_id)        ! set hru id in attributes netcdf file
  gru_struc(iGRU)%hruInfo(:)%hru_ix = arth(iHRU,1,gru_struc(iGRU)%hruCount)                    ! set index of hru in run domain
  gru_struc(iGRU)%hruInfo(:)%hru_id = hru_id(gru_struc(iGRU)%hruInfo(:)%hru_nc)                ! set id of hru
  iHRU = iHRU + gru_struc(iGRU)%hruCount


end subroutine pop_gru_struc

end module gru_struc_module