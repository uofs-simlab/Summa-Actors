module batch_distributer
  USE, intrinsic :: iso_c_binding
  ! global constants
  USE globalData,only:integerMissing      ! missing integer value
  USE globalData,only:realMissing         ! missing double precision value
  
  implicit none
  public::initFileManagerModule_fortran
  public::defineGlobalData_fortran
  public::deallocateGlobalData_fortran

  contains

subroutine initFileManagerModule_fortran(summaFileManagerIn_C,err,message_r) &
    bind(C, name="initFileManagerModule_fortran")
  USE C_interface_module
  USE summaFileManager
  implicit none

  ! dummy variables
  character(kind=c_char,len=1),intent(in)   :: summaFileManagerIn_C
  integer(c_int),intent(out)                :: err
  type(c_ptr),intent(out)                   :: message_r ! message to return to the caller
  ! local variables
  character(len=256)                        :: summaFileManagerIn    
  character(len=256)                        :: message = ""
  
  ! Convert strings 
  call f_c_string_ptr(trim(message), message_r);
  call c_f_string_chars(summaFileManagerIn_C, summaFileManagerIn)

  ! Initialize the file manager
  call summa_SetTimesDirsAndFiles(summaFileManagerIn, err, message)
  if (err /= 0) then; call f_c_string_ptr(trim(message), message_r); return; endif
end subroutine initFileManagerModule_fortran


subroutine defineGlobalData_fortran(err, message_r) bind(C, name="defineGlobalData_fortran")
  USE C_interface_module
  USE summa_globalData,only:summa_defineGlobalData           ! used to define global summa data structures

  implicit none
  ! dummy variables
  integer(c_int),intent(out)                :: err
  type(c_ptr),intent(out)                   :: message_r ! message to return to the caller
  ! local variables
  character(len=256)                        :: message = "" 

  ! Convert strings
  call f_c_string_ptr(trim(message), message_r)

  ! Define global data
  call summa_defineGlobalData(err, message)
  if (err /= 0) then; call f_c_string_ptr(trim(message), message_r); return; endif

end subroutine defineGlobalData_fortran

subroutine deallocateGlobalData_fortran(err, message_r) bind(C, name="deallocateGlobalData_fortran")
  USE C_interface_module
  USE globalData,only:averageFlux_meta
  USE var_lookup,only:childFLUX_MEAN
  USE globalData,only:statForc_meta, statProg_meta, statDiag_meta, statFlux_meta, &
                      statIndx_meta, statBvar_meta
  USE globalData,only:forcChild_map, progChild_map, diagChild_map, fluxChild_map, &
                      indxChild_map, bvarChild_map


  implicit none
  ! dummy variables
  integer(c_int),intent(out)                :: err
  type(c_ptr),intent(out)                   :: message_r ! message to return to the caller
  ! local variables
  character(len=256)                       :: message = "" 

  ! Convert strings
  call f_c_string_ptr(trim(message), message_r)

  if(allocated(averageFlux_meta)) deallocate(averageFlux_meta)
  if(allocated(childFLUX_MEAN)) deallocate(childFLUX_MEAN)
  if(allocated(statForc_meta)) deallocate(statForc_meta)
  if(allocated(statProg_meta)) deallocate(statProg_meta)
  if(allocated(statDiag_meta)) deallocate(statDiag_meta)
  if(allocated(statFlux_meta)) deallocate(statFlux_meta)
  if(allocated(statIndx_meta)) deallocate(statIndx_meta)
  if(allocated(statBvar_meta)) deallocate(statBvar_meta)
  if(allocated(forcChild_map)) deallocate(forcChild_map)
  if(allocated(progChild_map)) deallocate(progChild_map)
  if(allocated(diagChild_map)) deallocate(diagChild_map)
  if(allocated(fluxChild_map)) deallocate(fluxChild_map)
  if(allocated(indxChild_map)) deallocate(indxChild_map)
  if(allocated(bvarChild_map)) deallocate(bvarChild_map)



end subroutine deallocateGlobalData_fortran


end module batch_distributer