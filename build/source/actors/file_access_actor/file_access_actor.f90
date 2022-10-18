module file_access_actor
USE, intrinsic :: iso_c_binding
USE nrtype
USE data_types

implicit none
public::writeParamToNetCDF
public::writeDataToNetCDF
public::writeBasinToNetCDF
public::writeTimeToNetCDF

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
  USE writeOutput_module,only:writeParm 
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

subroutine writeDataToNetCDF() bind(C, name="writeDataToNetCDF")
implicit none
end subroutine writeDataToNetCDF

subroutine writeBasinToNetCDF() bind(C, name="writeBasinToNetCDF")
implicit none
end subroutine writeBasinToNetCDF

subroutine writeTimeToNetCDF() bind(C, name="writeTimeToNetCDF")
implicit none
end subroutine writeTimeToNetCDF




end module file_access_actor