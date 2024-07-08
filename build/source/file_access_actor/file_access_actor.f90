module file_access_actor
  USE, intrinsic :: iso_c_binding
  implicit none
  public::f_getNumTimeSteps
  contains
subroutine f_getNumTimeSteps(num_timesteps) bind(C, name="f_getNumTimeSteps")
  USE globalData,only:numtim
  implicit none
  integer(c_int), intent(out) :: num_timesteps
  num_timesteps = numtim
end subroutine f_getNumTimeSteps

end module file_access_actor
