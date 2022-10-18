module hru_actor
USE,intrinsic :: iso_c_binding
USE nrtype
implicit none


public::getSummaVariableInfo

contains
subroutine getSummaVariableInfo(var_type, var_fortran_index, data_struct) bind(C, name="getSummaVariableInfo")
  integer(c_int)          :: var_type
  integer(c_int)          :: var_fortran_index
  type(c_ptr)             :: data_struct


  
  


end subroutine getSummaVariableInfo

end module hru_actor