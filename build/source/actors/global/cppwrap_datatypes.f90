
module cppwrap_datatypes

use, intrinsic :: iso_c_binding
use data_types

implicit none
  
contains


! **************************** flagVec ****************************

function new_handle_flagVec() result(handle) bind(C, name='new_handle_flagVec')
  
  type(c_ptr) :: handle
  type(flagVec), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_flagVec

!-----------------------------------
subroutine delete_handle_flagVec(handle) bind(C, name='delete_handle_flagVec')
  
  type(c_ptr), intent(in), value :: handle
  type(flagVec), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_flagVec

!-----------------------------------
subroutine set_data_flagVec(handle, array, arr_size) bind(C, name='set_data_flagVec')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in), value :: arr_size
  integer(c_int), intent(in) :: array(arr_size)
  type(flagVec), pointer :: p
  
  call c_f_pointer(handle, p)    
  if (allocated(p%dat)) then
    if (size(p%dat) /= arr_size) then
      deallocate(p%dat)
      allocate(p%dat(arr_size))
    end if
  else
    allocate(p%dat(arr_size))
  end if
  
  where (array==1)
    p%dat = .true.
  elsewhere
    p%dat = .false.
  end where
  
end subroutine set_data_flagVec

!-----------------------------------
subroutine get_size_data_flagVec(handle, arr_size) bind(C, name='get_size_data_flagVec')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(flagVec), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%dat)) then
    arr_size = size(p%dat, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_flagVec

!-----------------------------------
subroutine get_data_flagVec(handle, array) bind(C, name='get_data_flagVec')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: array(*)
  type(flagVec), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%dat)) then
    where (p%dat)
      array(:size(p%dat)) = 1
    elsewhere
      array(:size(p%dat)) = 0
    end where
  end if
  
end subroutine get_data_flagVec

! **************************** var_i ********************************

function new_handle_var_i() result(handle) bind(C, name='new_handle_var_i')
  
  type(c_ptr) :: handle
  type(var_i), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_var_i

!-----------------------------------
subroutine delete_handle_var_i(handle) bind(C, name='delete_handle_var_i')
  
  type(c_ptr), intent(in), value :: handle
  type(var_i), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_var_i

!-----------------------------------
subroutine set_data_var_i(handle, array, arr_size) bind(C, name='set_data_var_i')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in), value :: arr_size
  integer(c_int), intent(in) :: array(arr_size)
  type(var_i), pointer :: p
  
  call c_f_pointer(handle, p)    
  if (allocated(p%var)) then
    if (size(p%var) /= arr_size) then
      deallocate(p%var)
      allocate(p%var(arr_size))
    end if
  else
    allocate(p%var(arr_size))
  end if
  p%var = array
  
end subroutine set_data_var_i

!-----------------------------------
subroutine get_size_data_var_i(handle, arr_size) bind(C, name='get_size_data_var_i')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(var_i), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    arr_size = size(p%var, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_var_i

!-----------------------------------
subroutine get_data_var_i(handle, array) bind(C, name='get_data_var_i')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: array(*)
  type(var_i), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    array(:size(p%var)) = p%var
  end if
  
end subroutine get_data_var_i

! **************************** var_i8 ****************************

function new_handle_var_i8() result(handle) bind(C, name='new_handle_var_i8')
  
  type(c_ptr) :: handle
  type(var_i8), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_var_i8

!-----------------------------------
subroutine delete_handle_var_i8(handle) bind(C, name='delete_handle_var_i8')
  
  type(c_ptr), intent(in), value :: handle
  type(var_i8), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_var_i8

!-----------------------------------
subroutine set_data_var_i8(handle, array, arr_size) bind(C, name='set_data_var_i8')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in), value :: arr_size
  integer(c_long), intent(in) :: array(arr_size)
  type(var_i8), pointer :: p
  
  call c_f_pointer(handle, p)    
  if (allocated(p%var)) then
    if (size(p%var) /= arr_size) then
      deallocate(p%var)
      allocate(p%var(arr_size))
    end if
  else
    allocate(p%var(arr_size))
  end if
  p%var = array
  
end subroutine set_data_var_i8

!-----------------------------------
subroutine get_size_data_var_i8(handle, arr_size) bind(C, name='get_size_data_var_i8')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(var_i8), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    arr_size = size(p%var, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_var_i8

!-----------------------------------
subroutine get_data_var_i8(handle, array) bind(C, name='get_data_var_i8')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_long), intent(out) :: array(*)
  type(var_i8), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    array(:size(p%var)) = p%var
  end if
  
end subroutine get_data_var_i8

! **************************** ilength **************************

function new_handle_ilength() result(handle) bind(C, name='new_handle_ilength')
  
  type(c_ptr) :: handle
  type(ilength), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_ilength

!-----------------------------------
subroutine delete_handle_ilength(handle) bind(C, name='delete_handle_ilength')
  
  type(c_ptr), intent(in), value :: handle
  type(ilength), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_ilength

!-----------------------------------
subroutine set_data_ilength(handle, array, arr_size) bind(C, name='set_data_ilength')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in), value :: arr_size
  integer(c_int), intent(in) :: array(arr_size)
  type(ilength), pointer :: p
  
  call c_f_pointer(handle, p)    
  if (allocated(p%dat)) then
    if (size(p%dat) /= arr_size) then
      deallocate(p%dat)
      allocate(p%dat(arr_size))
    end if
  else
    allocate(p%dat(arr_size))
  end if
  p%dat = array
  
end subroutine set_data_ilength

!-----------------------------------
subroutine get_size_data_ilength(handle, arr_size) bind(C, name='get_size_data_ilength')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(ilength), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%dat)) then
    arr_size = size(p%dat, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_ilength

!-----------------------------------
subroutine get_data_ilength(handle, array) bind(C, name='get_data_ilength')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: array(*)
  type(ilength), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%dat)) then
    array(:size(p%dat)) = p%dat
  end if
  
end subroutine get_data_ilength

! **************************** i8length **************************

  function new_handle_i8length() result(handle) bind(C, name='new_handle_i8length')
  
  type(c_ptr) :: handle
  type(i8length), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_i8length

!-----------------------------------
subroutine delete_handle_i8length(handle) bind(C, name='delete_handle_i8length')
  
  type(c_ptr), intent(in), value :: handle
  type(i8length), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_i8length

!-----------------------------------
subroutine set_data_i8length(handle, array, arr_size) bind(C, name='set_data_i8length')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in), value :: arr_size
  integer(c_long), intent(in) :: array(arr_size)
  type(i8length), pointer :: p
  
  call c_f_pointer(handle, p)    
  if (allocated(p%dat)) then
    if (size(p%dat) /= arr_size) then
      deallocate(p%dat)
      allocate(p%dat(arr_size))
    end if
  else
    allocate(p%dat(arr_size))
  end if
  p%dat = array
  
end subroutine set_data_i8length

!-----------------------------------
subroutine get_size_data_i8length(handle, arr_size) bind(C, name='get_size_data_i8length')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(i8length), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%dat)) then
    arr_size = size(p%dat, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_i8length

!-----------------------------------
subroutine get_data_i8length(handle, array) bind(C, name='get_data_i8length')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_long), intent(out) :: array(*)
  type(i8length), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%dat)) then
    array(:size(p%dat)) = p%dat
  end if
  
end subroutine get_data_i8length

! **************************** var_d ********************************

function new_handle_var_d() result(handle) bind(C, name='new_handle_var_d')
  
  type(c_ptr) :: handle
  type(var_d), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_var_d

!-----------------------------------
subroutine delete_handle_var_d(handle) bind(C, name='delete_handle_var_d')
  
  type(c_ptr), intent(in), value :: handle
  type(var_d), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_var_d

!-----------------------------------
subroutine set_data_var_d(handle, array, arr_size) bind(C, name='set_data_var_d')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in), value :: arr_size
  real(c_double), intent(in) :: array(arr_size)
  type(var_d), pointer :: p
  
  call c_f_pointer(handle, p)    
  if (allocated(p%var)) then
    if (size(p%var) /= arr_size) then
      deallocate(p%var)
      allocate(p%var(arr_size))
    end if
  else
    allocate(p%var(arr_size))
  end if
  p%var = array
  
end subroutine set_data_var_d

!-----------------------------------
subroutine get_size_data_var_d(handle, arr_size) bind(C, name='get_size_data_var_d')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(var_d), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    arr_size = size(p%var, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_var_d

!-----------------------------------
subroutine get_data_var_d(handle, array) bind(C, name='get_data_var_d')
  
  type(c_ptr), intent(in), value :: handle
  real(c_double), intent(out) :: array(*)
  type(var_d), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    array(:size(p%var)) = p%var
  end if
  
end subroutine get_data_var_d

! **************************** dlength **************************

function new_handle_dlength() result(handle) bind(C, name='new_handle_dlength')
  
  type(c_ptr) :: handle
  type(dlength), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_dlength

!-----------------------------------
subroutine delete_handle_dlength(handle) bind(C, name='delete_handle_dlength')
  
  type(c_ptr), intent(in), value :: handle
  type(dlength), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_dlength

!-----------------------------------
subroutine set_data_dlength(handle, array, arr_size) bind(C, name='set_data_dlength')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in), value :: arr_size
  real(c_double), intent(in) :: array(arr_size)
  type(dlength), pointer :: p
  
  call c_f_pointer(handle, p)    
  if (allocated(p%dat)) then
    if (size(p%dat) /= arr_size) then
      deallocate(p%dat)
      allocate(p%dat(arr_size))
    end if
  else
    allocate(p%dat(arr_size))
  end if
  p%dat = array
  
end subroutine set_data_dlength

!-----------------------------------
subroutine get_size_data_dlength(handle, arr_size) bind(C, name='get_size_data_dlength')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(dlength), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%dat)) then
    arr_size = size(p%dat, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_dlength

!-----------------------------------
subroutine get_data_dlength(handle, array) bind(C, name='get_data_dlength')
  
  type(c_ptr), intent(in), value :: handle
  real(c_double), intent(out) :: array(*)
  type(dlength), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%dat)) then
    array(:size(p%dat)) = p%dat
  end if
  
end subroutine get_data_dlength

! **************************** var_flagVec **************************

function new_handle_var_flagVec() result(handle) bind(C, name='new_handle_var_flagVec')
  
  type(c_ptr) :: handle
  type(var_flagVec), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_var_flagVec

!-----------------------------------
subroutine delete_handle_var_flagVec(handle) bind(C, name='delete_handle_var_flagVec')
  
  type(c_ptr), intent(in), value :: handle
  type(var_flagVec), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_var_flagVec

!-----------------------------------
subroutine set_data_var_flagVec(handle, array, num_row, num_col, num_elements) bind(C, name='set_data_var_flagVec')
  
  type(c_ptr), intent(in), value    :: handle
  integer(c_int), intent(in), value :: num_row
  integer(c_int), intent(in), value :: num_elements
  integer(c_int), intent(in)        :: num_col(num_row) 
  integer(c_int), intent(in)        :: array(num_elements)
  type(var_flagVec), pointer :: p
  integer(c_int)  :: i,sum_elem
  
  
  call c_f_pointer(handle, p)    
  if (allocated(p%var)) then
    if (size(p%var) /= num_row) then
      deallocate(p%var)
      allocate(p%var(num_row))
      do i=1,num_row
        allocate( p%var(i)%dat(num_col(i)) )
      end do
    end if
  else
      allocate(p%var(num_row))
      do i=1,num_row
        allocate( p%var(i)%dat(num_col(i)) )
      end do
  end if
  
  sum_elem = 0
  do i=1,num_row
      where ( array( sum_elem+1 : sum_elem+num_col(i) ) == 1 )
      p%var(i)%dat = .true.
    elsewhere
      p%var(i)%dat = .false.
    end where
    sum_elem = sum_elem + num_col(i)
  end do
  
end subroutine set_data_var_flagVec

!-----------------------------------
subroutine get_size_var_flagVec(handle, var_size) bind(C, name='get_size_var_flagVec')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: var_size
  type(var_flagVec), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    var_size = size(p%var, kind=c_int)
  else
    var_size = 0_c_int
  end if
  
end subroutine get_size_var_flagVec

!-----------------------------------
subroutine get_size_data_var_flagVec(handle, var_size, dat_size) bind(C, name='get_size_data_var_flagVec')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in) :: var_size
  integer(c_int), intent(out) :: dat_size(*)
  type(var_flagVec), pointer :: p
  integer(c_int)  :: i
  
  call c_f_pointer(handle, p)
  do i=1,var_size
      dat_size(i) = size(p%var(i)%dat, kind=c_int)
  end do
  
end subroutine get_size_data_var_flagVec

!-----------------------------------
subroutine get_data_var_flagVec(handle, array) bind(C, name='get_data_var_flagVec')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: array(*)
  type(var_flagVec), pointer :: p
  integer(c_int)  :: i,size_var,size_dat,size_array
  
  call c_f_pointer(handle, p)
  
  size_array = 0
  if (allocated(p%var)) then
    size_var = size(p%var)
    do i=1,size_var
      size_dat = size(p%var(i)%dat)
    where (p%var(i)%dat)
      array(size_array+1 : size_array+size_dat) = 1
    elsewhere
      array(size_array+1 : size_array+size_dat) = 0
    end where
      size_array = size_array + size_dat
    end do
    
  end if
  
end subroutine get_data_var_flagVec

! **************************** var_ilength ***************************

function new_handle_var_ilength() result(handle) bind(C, name='new_handle_var_ilength')
  
  type(c_ptr) :: handle
  type(var_ilength), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_var_ilength

!-----------------------------------
subroutine delete_handle_var_ilength(handle) bind(C, name='delete_handle_var_ilength')
  
  type(c_ptr), intent(in), value :: handle
  type(var_ilength), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_var_ilength

!-----------------------------------
subroutine set_data_var_ilength(handle, array, num_row, num_col, num_elements) bind(C, name='set_data_var_ilength')
  
  type(c_ptr), intent(in), value    :: handle
  integer(c_int), intent(in), value :: num_row
  integer(c_int), intent(in), value :: num_elements
  integer(c_int), intent(in)        :: num_col(num_row) 
  integer(c_int), intent(in)        :: array(num_elements)
  type(var_ilength), pointer :: p
  integer(c_int)  :: i,j,sum_elem
  
  
  call c_f_pointer(handle, p)    
  if (allocated(p%var)) then
    if (size(p%var) /= num_row) then
      deallocate(p%var)
      allocate(p%var(num_row))
      do i=1,num_row
        allocate( p%var(i)%dat(num_col(i)) )
      end do
    end if
  else
      allocate(p%var(num_row))
      do i=1,num_row
        allocate( p%var(i)%dat(num_col(i)) )
      end do
  end if
  
  sum_elem = 0
  do i=1,num_row
    do j=1,num_col(i)
      p%var(i)%dat(j) = array(sum_elem + j)
    end do
    sum_elem = sum_elem + num_col(i)
  end do
  
end subroutine set_data_var_ilength

!-----------------------------------
subroutine get_size_var_ilength(handle, var_size) bind(C, name='get_size_var_ilength')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: var_size
  type(var_ilength), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    var_size = size(p%var, kind=c_int)
  else
    var_size = 0_c_int
  end if
  
end subroutine get_size_var_ilength

!-----------------------------------
subroutine get_size_data_var_ilength(handle, var_size, dat_size) bind(C, name='get_size_data_var_ilength')
  
  type(c_ptr), intent(in), value:: handle
  integer(c_int), intent(in)    :: var_size
  integer(c_int), intent(out)   :: dat_size(*)
  type(var_ilength), pointer    :: p
  integer(c_int)  :: i
  
  call c_f_pointer(handle, p)
  
  do i=1,var_size
      dat_size(i) = size(p%var(i)%dat, kind=c_int)
  end do
  
end subroutine get_size_data_var_ilength

!-----------------------------------
subroutine get_data_var_ilength(handle, array) bind(C, name='get_data_var_ilength')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: array(*)
  type(var_ilength), pointer :: p
  integer(c_int)  :: i,j,size_var,size_dat,size_array
  
  call c_f_pointer(handle, p)
  
  size_array = 0
  if (allocated(p%var)) then
    size_var = size(p%var)
    do i=1,size_var
      size_dat = size(p%var(i)%dat)
      do j=1,size_dat
        array(size_array+j) = p%var(i)%dat(j)
      end do
      size_array = size_array + size_dat
    end do
    
  end if
  
end subroutine get_data_var_ilength

! **************************** var_i8length **************************

function new_handle_var_i8length() result(handle) bind(C, name='new_handle_var_i8length')
  
  type(c_ptr) :: handle
  type(var_i8length), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_var_i8length

!-----------------------------------
subroutine delete_handle_var_i8length(handle) bind(C, name='delete_handle_var_i8length')
  
  type(c_ptr), intent(in), value :: handle
  type(var_i8length), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_var_i8length

!-----------------------------------
subroutine set_data_var_i8length(handle, array, num_row, num_col, num_elements) bind(C, name='set_data_var_i8length')
  
  type(c_ptr), intent(in), value    :: handle
  integer(c_int), intent(in), value :: num_row
  integer(c_int), intent(in), value :: num_elements
  integer(c_int), intent(in)        :: num_col(num_row) 
  integer(c_long), intent(in)       :: array(num_elements)
  type(var_i8length), pointer :: p
  integer(c_int)  :: i,j,sum_elem
  
  
  call c_f_pointer(handle, p)    
  if (allocated(p%var)) then
    if (size(p%var) /= num_row) then
      deallocate(p%var)
      allocate(p%var(num_row))
      do i=1,num_row
        allocate( p%var(i)%dat(num_col(i)) )
      end do
    end if
  else
      allocate(p%var(num_row))
      do i=1,num_row
        allocate( p%var(i)%dat(num_col(i)) )
      end do
  end if
  
  sum_elem = 0
  do i=1,num_row
    do j=1,num_col(i)
      p%var(i)%dat(j) = array(sum_elem + j)
    end do
    sum_elem = sum_elem + num_col(i)
  end do
  
end subroutine set_data_var_i8length

!-----------------------------------
subroutine get_size_var_i8length(handle, var_size) bind(C, name='get_size_var_i8length')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: var_size
  type(var_i8length), pointer :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    var_size = size(p%var, kind=c_int)
  else
    var_size = 0_c_int
  end if
  
end subroutine get_size_var_i8length

!-----------------------------------
subroutine get_size_data_var_i8length(handle, var_size, dat_size) bind(C, name='get_size_data_var_i8length')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in) :: var_size
  integer(c_int), intent(out) :: dat_size(*)
  type(var_i8length), pointer :: p
  integer(c_int)  :: i
  
  call c_f_pointer(handle, p)
  do i=1,var_size
    dat_size(i) = size(p%var(i)%dat, kind=c_int)
  end do
  
end subroutine get_size_data_var_i8length

!-----------------------------------
subroutine get_data_var_i8length(handle, array) bind(C, name='get_data_var_i8length')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_long), intent(out) :: array(*)
  type(var_i8length), pointer :: p
  integer(c_int)  :: i,j,size_var,size_dat,size_array
  
  call c_f_pointer(handle, p)
  
  size_array = 0
  if (allocated(p%var)) then
    size_var = size(p%var)
    do i=1,size_var
      size_dat = size(p%var(i)%dat)
      do j=1,size_dat
        array(size_array+j) = p%var(i)%dat(j)
      end do
      size_array = size_array + size_dat
    end do
    
  end if
  
end subroutine get_data_var_i8length

! **************************** var_dlength **************************

function new_handle_var_dlength() result(handle) bind(C, name='new_handle_var_dlength')
  
  type(c_ptr) :: handle
  type(var_dlength), pointer :: p
  
  allocate(p)    
  handle = c_loc(p)   
  
end function new_handle_var_dlength

!-----------------------------------
subroutine delete_handle_var_dlength(handle) bind(C, name='delete_handle_var_dlength')
  
  type(c_ptr), intent(in), value :: handle
  type(var_dlength), pointer :: p
  
  call c_f_pointer(handle, p)
  deallocate(p)
  
end subroutine delete_handle_var_dlength

!-----------------------------------
subroutine set_data_var_dlength(handle, array, num_row, num_col, num_elements) bind(C, name='set_data_var_dlength')
  
  type(c_ptr), intent(in), value    :: handle
  integer(c_int), intent(in), value :: num_row
  integer(c_int), intent(in), value :: num_elements
  integer(c_int), intent(in)        :: num_col(num_row) 
  real(c_double), intent(in)        :: array(num_elements)
  type(var_dlength), pointer :: p
  integer(c_int)  :: i,j,sum_elem
  
  
  call c_f_pointer(handle, p)    
  if (allocated(p%var)) then
    if (size(p%var) /= num_row) then
      deallocate(p%var)
      allocate(p%var(num_row))
      do i=1,num_row
        allocate( p%var(i)%dat(num_col(i)) )
      end do
    end if
  else
      allocate(p%var(num_row))
      do i=1,num_row
        allocate( p%var(i)%dat(num_col(i)) )
      end do
  end if
  
  sum_elem = 0
  do i=1,num_row
    do j=1,num_col(i)
      p%var(i)%dat(j) = array(sum_elem + j)
    end do
    sum_elem = sum_elem + num_col(i)
  end do
  
end subroutine set_data_var_dlength

!-----------------------------------
subroutine get_size_var_dlength(handle, var_size) bind(C, name='get_size_var_dlength')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out)    :: var_size
  type(var_dlength), pointer     :: p
  
  call c_f_pointer(handle, p)
  if (allocated(p%var)) then
    var_size = size(p%var, kind=c_int)
  else
    var_size = 0_c_int
  end if
  
end subroutine get_size_var_dlength

!-----------------------------------
subroutine get_size_data_var_dlength(handle, var_size, dat_size) bind(C, name='get_size_data_var_dlength')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in) :: var_size
  integer(c_int), intent(out) :: dat_size(*)
  type(var_dlength), pointer :: p
  integer(c_int)  :: i
  
  call c_f_pointer(handle, p)
  
  do i=1,var_size
      dat_size(i) = size(p%var(i)%dat, kind=c_int)
  end do
  
end subroutine get_size_data_var_dlength

!-----------------------------------
subroutine get_data_var_dlength(handle, array) bind(C, name='get_data_var_dlength')
  
  type(c_ptr), intent(in), value :: handle
  real(c_double), intent(out) :: array(*)
  type(var_dlength), pointer :: p
  integer(c_int)  :: i,j,size_var,size_dat,size_array
  
  call c_f_pointer(handle, p)
  
  size_array = 0
  if (allocated(p%var)) then
    size_var = size(p%var)
    do i=1,size_var
      size_dat = size(p%var(i)%dat)
      do j=1,size_dat
        array(size_array+j) = p%var(i)%dat(j)
      end do
      size_array = size_array + size_dat
    end do
    
  end if
  
end subroutine get_data_var_dlength

! **************************** var_dlength **************************
! ************************ var_dlength_array ************************
function new_handle_dlength_array() result(handle) bind(C, name='new_handle_dlength_array')
  type(c_ptr) :: handle
  type(var_dlength_array), pointer :: p

  allocate(p)
  handle=c_loc(p)

end function new_handle_dlength_array
!--------------------------------
subroutine delete_handle_dlength_array(handle) bind(C, name='delete_handle_dlength_array')

  type(c_ptr), intent(in), value :: handle
  type(var_dlength_array), pointer :: p

  call c_f_pointer(handle, p)
  deallocate(p)

end subroutine
! ************************ var_dlength_array ************************
! ***************************** file_info ***************************
function new_handle_file_info() result(handle) bind(C, name='new_handle_file_info')
  type(c_ptr) :: handle
  type(file_info_array), pointer :: p

  allocate(p)
  handle = c_loc(p)

end function new_handle_file_info

!---------------------------------
subroutine delete_handle_file_info(handle) bind(C, name='delete_handle_file_info')

  type(c_ptr), intent(in), value :: handle
  type(file_info_array), pointer :: p

  call c_f_pointer(handle, p)
  deallocate(p)

end subroutine delete_handle_file_info
! ***************************** file_info ***************************
    
end module cppwrap_datatypes


