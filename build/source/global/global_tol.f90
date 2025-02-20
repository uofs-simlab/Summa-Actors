module global_tol
  use iso_c_binding
  implicit none
  logical(c_bool), save :: default_tol = .true.
contains
  subroutine f_set_default_tol(new_tol) bind(C)
    use iso_c_binding
    implicit none
    logical(c_bool), intent(in) :: new_tol
    default_tol = new_tol
  end subroutine set_default_tol

  function f_get_default_tol() result(val) bind(C, name="get_default_tol")
    use iso_c_binding, only: c_bool
    implicit none
    logical(c_bool) :: val
    val = default_tol
  end function get_default_tol

end module global_tol
