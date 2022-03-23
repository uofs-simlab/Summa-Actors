
module cppwrap_auxiliary

use, intrinsic :: iso_c_binding
use data_types

  

  ! privacy
  implicit none
  public::comp_len_string
  public::c_f_string
  
 contains
! ***************** Auxiliary functions and subroutines ***********************

  function comp_len_string(c_str) result(length)
    character(kind=c_char,len=1),intent(in)   :: c_str(*)
    integer                                   :: length
  	
    length=0
    do
       if (c_str(length+1) == C_NULL_CHAR) exit
       length = length + 1
    end do 
    
  end function comp_len_string
  
! -----------------------------------------------------------  	
  subroutine c_f_string(c_str, f_str, l)
  
    character(kind=c_char,len=1),intent(in) :: c_str(*)
    integer, intent(in)                     :: l
    character(len=l),intent(out)            :: f_str
  
    character(len=:), allocatable           :: temp
    integer                                 :: length   
      
    length = comp_len_string(c_str)
    
    allocate(character(len=length) :: temp)    
    temp = transfer(c_str(1:length), temp)
    
    f_str = temp
    
    deallocate(temp)
    
  end subroutine c_f_string
  
     
end module cppwrap_auxiliary


