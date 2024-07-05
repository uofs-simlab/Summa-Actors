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

subroutine FileAccessActor_DeallocateStructures(handle_ncid) bind(C,name="FileAccessActor_DeallocateStructures")
  USE netcdf_util_module,only:nc_file_close 
  USE globalData,only:structInfo                              ! information on the data structures
  USE output_structure_module,only:outputTimeStep
  USE output_structure_module,only:summa_struct
  USE var_lookup,only:maxvarFreq                ! maximum number of output files
  ! TODO: The index_map is allocated by the job actor, but deallocated by the file access actor
  USE globalData,only:index_map 
  implicit none
  type(c_ptr),intent(in), value        :: handle_ncid

  type(var_i),pointer                  :: ncid
  integer(i4b)                         :: iFreq
  character(LEN=256)                   :: cmessage
  character(LEN=256)                   :: message
  integer(i4b)                         :: err

  call c_f_pointer(handle_ncid, ncid)
  ! close the open output FIle
  do iFreq=1,maxvarFreq
    if (ncid%var(iFreq)/=integerMissing) then
      call nc_file_close(ncid%var(iFreq),err,cmessage)
      if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
    endif   
  end do

  deallocate(ncid)
  ! deallocate(outputTimeStep)
  ! deallocate(summa_struct)
  deallocate(index_map)
end subroutine FileAccessActor_DeallocateStructures

end module file_access_actor
