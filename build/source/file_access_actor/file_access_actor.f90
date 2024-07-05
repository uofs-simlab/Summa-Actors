module file_access_actor


  !======= Inclusions ===========
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE data_types
  USE actor_data_types
  USE globalData
  USE globalData,only:integerMissing      ! missing integer value
  USE globalData,only:realMissing         ! missing double precision value
  
  implicit none
  public::f_getNumTimeSteps
  public::FileAccessActor_DeallocateStructures

  character(len=64), parameter     :: summaVersion = ''
  character(len=64), parameter     :: buildTime = ''
  character(len=64), parameter     :: gitBranch = ''
  character(len=64), parameter     :: gitHash = ''
  
  contains

subroutine f_getNumTimeSteps(num_timesteps) bind(C, name="f_getNumTimeSteps")
  USE globalData,only:numtim
  implicit none
  integer(c_int), intent(out) :: num_timesteps
  num_timesteps = numtim
end subroutine f_getNumTimeSteps

subroutine FileAccessActor_DeallocateStructures() bind(C,name="FileAccessActor_DeallocateStructures")
  ! TODO: The index_map is allocated by the job actor, but deallocated by the file access actor
  USE globalData,only:index_map 
  implicit none

  ! TODO: The index_map is allocated by the job actor, but deallocated by the file access actor
  deallocate(index_map)
end subroutine FileAccessActor_DeallocateStructures

end module file_access_actor
