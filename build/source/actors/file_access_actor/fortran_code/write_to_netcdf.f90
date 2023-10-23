module write_to_netcdf_module
USE, intrinsic :: iso_c_binding
USE nrtype
USE data_types

!TODO: This module is only used for writeParamToNetCDF the others are not.


implicit none
public::writeParamToNetCDF
public::writeGRUStatistics

contains

! Subroutine that writes data from the HRU actor to be written to netcdf
subroutine writeParamToNetCDF(handle_ncid,    &
                          index_gru,          &
                          index_hru,          &
                          handle_attr_struct, &
                          handle_type_struct, &
                          handle_mpar_struct, &
                          handle_bpar_struct, &
                          err) bind(C, name="writeParamToNetCDF")
  USE globalData,only:attr_meta,type_meta,mpar_meta,bpar_meta ! meta structures
  USE globalData,only:gru_struc
  USE modelwrite_module,only:writeParm 
  USE globalData,only:structInfo                              ! information on the data structures
  implicit none
  ! dummy variables
  type(c_ptr),   intent(in), value  :: handle_ncid        ! ncid of the output file
  integer(c_int),intent(in)         :: index_gru          ! index of GRU in gru_struc
  integer(c_int),intent(in)         :: index_hru          ! index of HRU in gru_struc
  type(c_ptr),   intent(in), value  :: handle_attr_struct
  type(c_ptr),   intent(in), value  :: handle_type_struct
  type(c_ptr),   intent(in), value  :: handle_mpar_struct
  type(c_ptr),   intent(in), value  :: handle_bpar_struct 
  integer(c_int),intent(out)        :: err 
  ! local variables pointers
  type(var_i), pointer              :: ncid
  type(var_d), pointer              :: attr_struct
  type(var_i), pointer              :: type_struct
  type(var_dlength), pointer        :: mpar_struct
  type(var_d),pointer               :: bpar_struct
  ! local variables
  integer(i4b)                      :: iStruct
  character(LEN=256)                :: cmessage
  character(LEN=256)                :: message
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  message="file_access_actor.f90 - writeParamToNetCDF"
  call c_f_pointer(handle_ncid, ncid)
  call c_f_pointer(handle_attr_struct, attr_struct)
  call c_f_pointer(handle_type_struct, type_struct)
  call c_f_pointer(handle_mpar_struct, mpar_struct)
  call c_f_pointer(handle_bpar_struct, bpar_struct)

  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('attr'); call writeParm(ncid,gru_struc(index_gru)%hruInfo(index_hru)%hru_ix, &
        attr_struct,attr_meta,err,cmessage)
      case('type'); call writeParm(ncid,gru_struc(index_gru)%hruInfo(index_hru)%hru_ix, &
        type_struct,type_meta,err,cmessage)
      case('mpar'); call writeParm(ncid,gru_struc(index_gru)%hruInfo(index_hru)%hru_ix, &
        mpar_struct,mpar_meta,err,cmessage)
    end select
    if(err/=0)then
      message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
      print*, message
      return
    endif
  end do

  ! write GRU parameters
  call writeParm(ncid,index_gru,bpar_struct,bpar_meta,err,cmessage)
  if(err/=0)then
    message=trim(message)//trim(cmessage)
    print*, message
    return
  endif
end subroutine writeParamToNetCDF



subroutine writeGRUStatistics(handle_ncid,      &
                              gru_var_ids,      &
                              gru_stats_vector, &
                              num_gru,          &
                              err) bind(C, name="WriteGRUStatistics")
  USE data_types,only:var_i,netcdf_gru_actor_info,serializable_netcdf_gru_actor_info
  USE var_lookup, only: maxvarFreq ! number of output frequencies
  USE netcdf
  implicit none
  ! Dummy Variables
  type(c_ptr), intent(in), value                      :: handle_ncid
  type(netcdf_gru_actor_info),intent(in)              :: gru_var_ids
  type(serializable_netcdf_gru_actor_info),intent(in) :: gru_stats_vector(num_gru)
  integer(c_int), intent(in)                          :: num_gru
  integer(c_int), intent(out)                         :: err
  
  ! Local Variables
  type(var_i), pointer                                :: ncid
  real(c_double), dimension(num_gru)                  :: run_time_array
  real(c_double), dimension(num_gru)                  :: init_time_array
  real(c_double), dimension(num_gru)                  :: forcing_time_array
  real(c_double), dimension(num_gru)                  :: run_physics_time_array
  real(c_double), dimension(num_gru)                  :: write_output_time_array
  real(c_double), dimension(num_gru)                  :: rel_tol_array
  real(c_double), dimension(num_gru)                  :: abs_tol_array
  integer(c_int), dimension(num_gru)                  :: successful_array
  integer(c_int), dimension(num_gru)                  :: num_attempts_array

  integer(c_int)                                      :: i
  integer(c_int)                                      :: iFreq         
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  call c_f_pointer(handle_ncid, ncid)

  ! Assemble fortran arrays
  do i=1,num_gru
    run_time_array(i) = gru_stats_vector(i)%run_time
    init_time_array(i) = gru_stats_vector(i)%init_duration
    forcing_time_array(i) = gru_stats_vector(i)%forcing_duration
    run_physics_time_array(i) = gru_stats_vector(i)%run_physics_duration
    write_output_time_array(i) = gru_stats_vector(i)%write_output_duration
    rel_tol_array(i) = gru_stats_vector(i)%rel_tol
    abs_tol_array(i) = gru_stats_vector(i)%abs_tol
    successful_array(i) = gru_stats_vector(i)%successful
    num_attempts_array(i) = gru_stats_vector(i)%num_attempts
  end do

  ! Write to NetCDF
  do iFreq=1, maxvarFreq
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%run_time_var_id, run_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%init_duration_var_id, init_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%forcing_duration_var_id, forcing_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%run_physics_duration_var_id, run_physics_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%write_output_duration_var_id, write_output_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%state_var_id, successful_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%num_attempts_var_id, num_attempts_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%rel_tol_var_id, rel_tol_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%abs_tol_var_id, abs_tol_array)
  end do

end subroutine writeGRUStatistics


end module write_to_netcdf_module