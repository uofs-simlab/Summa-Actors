
! This module contains all the functions that are used to
! access the forcing file and setup the forcing data
! for the HRUs to read from
module access_forcing_module

USE, intrinsic :: iso_c_binding
USE nrtype

USE data_types,only:ilength         ! global data structure for forcing data
USE actor_data_types,only:file_info_array

USE globalData,only:time_meta,forc_meta       ! metadata structures



implicit none
private
public::read_forcingFile


contains




end module access_forcing_module