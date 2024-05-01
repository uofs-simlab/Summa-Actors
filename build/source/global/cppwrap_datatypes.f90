
module cppwrap_datatypes

use, intrinsic :: iso_c_binding
use data_types
use actor_data_types

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

!-----------------------------------
subroutine get_size_data_typeStruct(handle, arr_size) bind(C, name='get_size_data_typeStruct')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(hru_type), pointer :: hru_data
  
  call c_f_pointer(handle, hru_data)
  if (allocated(hru_data%typeStruct%var)) then
    arr_size = size(hru_data%typeStruct%var, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_typeStruct

!-----------------------------------
subroutine get_data_typeStruct(handle, array) bind(C, name='get_data_typeStruct')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: array(*)
  type(hru_type), pointer :: hru_data
  
  call c_f_pointer(handle, hru_data)
  if (allocated(hru_data%typeStruct%var)) then
    array(:size(hru_data%typeStruct%var)) = hru_data%typeStruct%var
  end if
  
end subroutine get_data_typeStruct

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



subroutine get_size_data_attrStruct(handle, arr_size) bind(C, name='get_size_data_attrStruct')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(hru_type), pointer :: hru_data
  
  call c_f_pointer(handle, hru_data)
  if (allocated(hru_data%attrStruct%var)) then
    arr_size = size(hru_data%attrStruct%var, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_attrStruct

!-----------------------------------
subroutine get_data_attrStruct(handle, array) bind(C, name='get_data_attrStruct')
  
  type(c_ptr), intent(in), value :: handle
  real(c_double), intent(out) :: array(*)
  type(hru_type), pointer :: hru_data
  
  call c_f_pointer(handle, hru_data)
  if (allocated(hru_data%attrStruct%var)) then
    array(:size(hru_data%attrStruct%var)) = hru_data%attrStruct%var
  end if
  
end subroutine get_data_attrStruct

subroutine get_size_data_bparStruct(handle, arr_size) bind(C, name='get_size_data_bparStruct')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out) :: arr_size
  type(hru_type), pointer :: hru_data
  
  call c_f_pointer(handle, hru_data)
  if (allocated(hru_data%bparStruct%var)) then
    arr_size = size(hru_data%bparStruct%var, kind=c_int)
  else
    arr_size = 0_c_int
  end if
  
end subroutine get_size_data_bparStruct

!-----------------------------------
subroutine get_data_bparStruct(handle, array) bind(C, name='get_data_bparStruct')
  
  type(c_ptr), intent(in), value :: handle
  real(c_double), intent(out) :: array(*)
  type(hru_type), pointer :: hru_data
  
  call c_f_pointer(handle, hru_data)
  if (allocated(hru_data%bparStruct%var)) then
    array(:size(hru_data%bparStruct%var)) = hru_data%bparStruct%var
  end if
  
end subroutine get_data_bparStruct

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
  integer(c_int)  :: i,j,size_var,size_dat,size_array,j2
  integer(c_int)  :: start_index(1)
  
  call c_f_pointer(handle, p)
  
  size_array = 0
  if (allocated(p%var)) then
    size_var = size(p%var)
    do i=1,size_var
      size_dat = size(p%var(i)%dat)
      start_index = lbound(p%var(i)%dat)

      if (start_index(1) == 0) then
        size_dat = size_dat - 1
      endif

      j2=1
      do j=start_index(1),size_dat
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
  integer(c_int)              :: i,j,size_var,size_dat,size_array,j2,loop_val
  integer(c_int)              :: start_index(1)
  
  call c_f_pointer(handle, p)
  
  size_array = 0
  if (allocated(p%var)) then
    size_var = size(p%var)
    do i=1,size_var
      size_dat = size(p%var(i)%dat)
      start_index = lbound(p%var(i)%dat)

      if (start_index(1) == 0) then
        loop_val = size_dat - 1
      else
        loop_val = size_dat
      endif


      j2=1
      do j=1,loop_val
        array(size_array+j) = p%var(i)%dat(j)
        j2=j2+1
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

subroutine get_size_var_mparStruct(handle, var_size) bind(C, name='get_size_var_mparStruct')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(out)    :: var_size
  type(hru_type), pointer     :: hru_data
  
  call c_f_pointer(handle, hru_data)
  if (allocated(hru_data%mparStruct%var)) then
    var_size = size(hru_data%mparStruct%var, kind=c_int)
  else
    var_size = 0_c_int
  end if
  
end subroutine get_size_var_mparStruct


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
subroutine get_size_data_mparStruct(handle, var_size, dat_size) bind(C, name='get_size_data_mparStruct')
  
  type(c_ptr), intent(in), value :: handle
  integer(c_int), intent(in) :: var_size
  integer(c_int), intent(out) :: dat_size(*)
  type(hru_type), pointer :: hru_data
  integer(c_int)  :: i
  
  call c_f_pointer(handle, hru_data)
  
  do i=1,var_size
      dat_size(i) = size(hru_data%mparStruct%var(i)%dat, kind=c_int)
  end do
  
end subroutine get_size_data_mparStruct


!-----------------------------------
subroutine get_data_var_dlength(handle, array) bind(C, name='get_data_var_dlength')
  
  type(c_ptr), intent(in), value :: handle
  real(c_double), intent(out) :: array(*)
  type(var_dlength), pointer  :: p
  integer(c_int)              :: i,j,size_var,size_dat,size_array,j2,loop_val
  integer(c_int)              :: start_index(1)
  
  call c_f_pointer(handle, p)
  
  size_array = 0
  if (allocated(p%var)) then
    size_var = size(p%var)
    do i=1,size_var
      size_dat = size(p%var(i)%dat)
      start_index = lbound(p%var(i)%dat)

      if (start_index(1) == 0) then
        loop_val = size_dat - 1
      else
        loop_val = size_dat
      endif

      j2=1
      do j=start_index(1),loop_val
        array(size_array+j2) = p%var(i)%dat(j)
        j2=j2+1
      end do
      size_array = size_array + size_dat
    end do
    
  end if
  
end subroutine get_data_var_dlength

subroutine get_data_mparStruct(handle, array) bind(C, name='get_data_mparStruct')
  
  type(c_ptr), intent(in), value :: handle
  real(c_double), intent(out)    :: array(*)
  type(hru_type), pointer        :: hru_data
  integer(c_int)                 :: i,j,size_var,size_dat,size_array,j2,loop_val
  integer(c_int)                 :: start_index(1)
  
  call c_f_pointer(handle, hru_data)
  
  size_array = 0
  if (allocated(hru_data%mparStruct%var)) then
    size_var = size(hru_data%mparStruct%var)
    do i=1,size_var
      size_dat = size(hru_data%mparStruct%var(i)%dat)
      start_index = lbound(hru_data%mparStruct%var(i)%dat)

      if (start_index(1) == 0) then
        loop_val = size_dat - 1
      else
        loop_val = size_dat
      endif

      j2=1
      do j=start_index(1),loop_val
        array(size_array+j2) = hru_data%mparStruct%var(i)%dat(j)
        j2=j2+1
      end do
      size_array = size_array + size_dat
    end do
    
  end if
  
end subroutine get_data_mparStruct


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

  
! ****************************** z_lookup ****************************
#ifdef V4_ACTIVE
function new_handle_z_lookup() result(handle) bind(C, name="new_handle_z_lookup")
  type(c_ptr)            :: handle
  type(zLookup), pointer :: p

  allocate(p)
  handle = c_loc(p)
end function

subroutine delete_handle_z_lookup(handle) bind(C, name="delete_handle_z_lookup")
  type(c_ptr), intent(in), value :: handle
  type(zLookup), pointer :: p

  call c_f_pointer(handle, p)
  deallocate(p)
end subroutine


subroutine get_size_z_lookup(handle, size_z) bind(C, name='get_size_z_lookup')
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(out)       :: size_z
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)
  if (allocated(hru_data%lookupStruct%z)) then
    size_z = size(hru_data%lookupStruct%z, kind=c_int)
  else
    size_z = 0_c_int
  end if
end subroutine get_size_z_lookup

subroutine get_size_var_lookup(handle, z, var_size) bind(C, name='get_size_var_lookup')
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: z
  integer(c_int), intent(out)       :: var_size
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)
  var_size = size(hru_data%lookupStruct%z(z)%var, kind=c_int)
end subroutine


subroutine get_size_data_lookup(handle, z, var, size_data) bind(C, name='get_size_data_lookup')  
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: z
  integer(c_int), intent(in)        :: var
  integer(c_int), intent(out)       :: size_data
  type(hru_type), pointer           :: hru_data
  
  call c_f_pointer(handle, hru_data)

  size_data = size(hru_data%lookupStruct%z(z)%var(var)%lookup, kind=c_int)
end subroutine get_size_data_lookup


subroutine get_data_zlookup(handle, z, var, array) bind(C, name='get_data_zlookup')
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: z
  integer(c_int), intent(in)        :: var
  real(c_double), intent(out)       :: array(*)
  type(hru_type), pointer           :: hru_data
  integer(c_int)                    :: i, size_data

  call c_f_pointer(handle, hru_data)

  size_data = size(hru_data%lookupStruct%z(z)%var(var)%lookup, kind=c_int)
  array(:size_data) = hru_data%lookupStruct%z(z)%var(var)%lookup
end subroutine get_data_zlookup
#endif


! ****************************** z_lookup ****************************

! ****************************** var_dlength ****************************
subroutine get_size_var_dlength_by_indx(handle, struct_indx, size_var) &
    bind(C, name='get_size_var_dlength_by_indx')
  
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(out)       :: size_var
  type(hru_type), pointer           :: hru_data
  
  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! forcStat
      if (allocated(hru_data%forcStat%var)) then
        size_var = size(hru_data%forcStat%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(2) ! progStat
      if (allocated(hru_data%progStat%var)) then
        size_var = size(hru_data%progStat%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(3) ! diagStat
      if (allocated(hru_data%diagStat%var)) then
        size_var = size(hru_data%diagStat%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(4) ! fluxStat
      if (allocated(hru_data%fluxStat%var)) then
        size_var = size(hru_data%fluxStat%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(5) ! indxStat
      if (allocated(hru_data%indxStat%var)) then
        size_var = size(hru_data%indxStat%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(6) ! bvarStat
      if (allocated(hru_data%bvarStat%var)) then
        size_var = size(hru_data%bvarStat%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(7) ! mparStruct
      if (allocated(hru_data%mparStruct%var)) then
        size_var = size(hru_data%mparStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(8) ! progStruct
      if (allocated(hru_data%progStruct%var)) then
        size_var = size(hru_data%progStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(9) ! diagStruct
      if (allocated(hru_data%diagStruct%var)) then
        size_var = size(hru_data%diagStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(10) ! fluxStruct
      if (allocated(hru_data%fluxStruct%var)) then
        size_var = size(hru_data%fluxStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(11) ! bvarStruct
      if (allocated(hru_data%bvarStruct%var)) then
        size_var = size(hru_data%bvarStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
  end select
end subroutine get_size_var_dlength_by_indx

subroutine get_size_data_var_dlength_by_indx(handle, struct_indx, size_var, &
    dat_size) bind(C, name='get_size_data_var_dlength_by_indx')
  
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(in)        :: size_var
  integer(c_int), intent(out)       :: dat_size(*)
  type(hru_type), pointer           :: hru_data
  integer(c_int)                    :: i
  
  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! forcStat
      do i=1,size_var
        dat_size(i) = size(hru_data%forcStat%var(i)%dat, kind=c_int)
      end do
    case(2) ! progStat
      do i=1,size_var
        dat_size(i) = size(hru_data%progStat%var(i)%dat, kind=c_int)
      end do
    case(3) ! diagStat
      do i=1,size_var
        dat_size(i) = size(hru_data%diagStat%var(i)%dat, kind=c_int)
      end do
    case(4) ! fluxStat
      do i=1,size_var
        dat_size(i) = size(hru_data%fluxStat%var(i)%dat, kind=c_int)
      end do
    case(5) ! indxStat
      do i=1,size_var
        dat_size(i) = size(hru_data%indxStat%var(i)%dat, kind=c_int)
      end do
    case(6) ! bvarStat
      do i=1,size_var
        dat_size(i) = size(hru_data%bvarStat%var(i)%dat, kind=c_int)
      end do
    case(7) ! mparStruct
      do i=1,size_var
        dat_size(i) = size(hru_data%mparStruct%var(i)%dat, kind=c_int)
      end do
    case(8) ! progStruct
      do i=1,size_var
        dat_size(i) = size(hru_data%progStruct%var(i)%dat, kind=c_int)
      end do
    case(9) ! diagStruct
      do i=1,size_var
        dat_size(i) = size(hru_data%diagStruct%var(i)%dat, kind=c_int)
      end do
    case(10) ! fluxStruct
      do i=1,size_var
        dat_size(i) = size(hru_data%fluxStruct%var(i)%dat, kind=c_int)
      end do
    case(11) ! bvarStruct
      do i=1,size_var
        dat_size(i) = size(hru_data%bvarStruct%var(i)%dat, kind=c_int)
      end do
  end select

end subroutine get_size_data_var_dlength_by_indx

subroutine get_data_var_dlength_by_indx(handle, struct_indx, dat)&
    bind(C, name='get_data_var_dlength_by_indx')

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  real(c_double), intent(out)       :: dat(*)
  type(hru_type), pointer           :: hru_data
  integer(c_int)              :: i,j,size_var,size_dat,size_array,j2,loop_val
  integer(c_int)              :: start_index(1)

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! forcStat
      size_array = 0
      if (allocated(hru_data%forcStat%var)) then
        size_var = size(hru_data%forcStat%var)
        do i=1,size_var
          size_dat = size(hru_data%forcStat%var(i)%dat)
          start_index = lbound(hru_data%forcStat%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          endif

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%forcStat%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(2) ! progStat
      size_array = 0
      if (allocated(hru_data%progStat%var)) then
        size_var = size(hru_data%progStat%var)
        do i=1,size_var
          size_dat = size(hru_data%progStat%var(i)%dat)
          start_index = lbound(hru_data%progStat%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          endif

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%progStat%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(3) ! diagStat
      size_array = 0
      if (allocated(hru_data%diagStat%var)) then
        size_var = size(hru_data%diagStat%var)
        do i=1,size_var
          size_dat = size(hru_data%diagStat%var(i)%dat)
          start_index = lbound(hru_data%diagStat%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          endif

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%diagStat%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(4) ! fluxStat
      size_array = 0
      if (allocated(hru_data%fluxStat%var)) then
        size_var = size(hru_data%fluxStat%var)
        do i=1,size_var
          size_dat = size(hru_data%fluxStat%var(i)%dat)
          start_index = lbound(hru_data%fluxStat%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          endif

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%fluxStat%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(5) ! indxStat
      size_array = 0
      if (allocated(hru_data%indxStat%var)) then
        size_var = size(hru_data%indxStat%var)
        do i=1,size_var
          size_dat = size(hru_data%indxStat%var(i)%dat)
          start_index = lbound(hru_data%indxStat%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          endif

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%indxStat%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(6) ! bvarStat
      size_array = 0
      if (allocated(hru_data%bvarStat%var)) then
        size_var = size(hru_data%bvarStat%var)
        do i=1,size_var
          size_dat = size(hru_data%bvarStat%var(i)%dat)
          start_index = lbound(hru_data%bvarStat%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          endif

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%bvarStat%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(7) ! mparStruct
      size_array = 0
      if (allocated(hru_data%mparStruct%var)) then
        size_var = size(hru_data%mparStruct%var)
        do i=1,size_var
          size_dat = size(hru_data%mparStruct%var(i)%dat)
          start_index = lbound(hru_data%mparStruct%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          endif

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%mparStruct%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(8) ! progStruct
      size_array = 0
      if (allocated(hru_data%progStruct%var)) then
        size_var = size(hru_data%progStruct%var)
        do i=1,size_var
          size_dat = size(hru_data%progStruct%var(i)%dat)
          start_index = lbound(hru_data%progStruct%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          endif

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%progStruct%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(9) ! diagStruct
      size_array = 0
      if (allocated(hru_data%diagStruct%var)) then
        size_var = size(hru_data%diagStruct%var)
        do i=1,size_var
          size_dat = size(hru_data%diagStruct%var(i)%dat)
          start_index = lbound(hru_data%diagStruct%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          end if

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%diagStruct%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(10) ! fluxStruct
      size_array = 0
      if (allocated(hru_data%fluxStruct%var)) then
        size_var = size(hru_data%fluxStruct%var)
        do i=1,size_var
          size_dat = size(hru_data%fluxStruct%var(i)%dat)
          start_index = lbound(hru_data%fluxStruct%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          end if

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%fluxStruct%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
    case(11) ! bvarStruct
      size_array = 0
      if (allocated(hru_data%bvarStruct%var)) then
        size_var = size(hru_data%bvarStruct%var)
        do i=1,size_var
          size_dat = size(hru_data%bvarStruct%var(i)%dat)
          start_index = lbound(hru_data%bvarStruct%var(i)%dat)

          if (start_index(1) == 0) then
            loop_val = size_dat - 1
          else
            loop_val = size_dat
          end if

          j2=1
          do j=start_index(1),loop_val
            dat(size_array+j2) = hru_data%bvarStruct%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
  end select
end subroutine get_data_var_dlength_by_indx

subroutine set_data_var_dlength_local(metaData, varData_out, num_var, &
    varData_in, num_elements, dat_array)
  USE var_lookup,only:iLookVarType           ! look up structure for variable typed
  
  type(var_info),    intent(in)  :: metaData(:)
  type(var_dlength), intent(out) :: varData_out
  integer(c_int),    intent(in)  :: num_var
  integer(c_int),    intent(in)  :: varData_in(num_var)
  integer(c_int),    intent(in)  :: num_elements
  real(c_double),    intent(in)  :: dat_array(num_elements)

  integer(c_int)                 :: iVar,dat_length,j,sum_elem  

  if (size(metaData(:)) /= num_var) then
    print *, 'ERROR: Number of variables in metaData does not match num_var'
    print*, 'Number of variables in metaData:', size(metaData(:))
    print*, 'num_var:', num_var
    stop
  end if

  if (allocated(varData_out%var)) then
    if (size(varData_out%var) /= num_var) then
      deallocate(varData_out%var)
      allocate(varData_out%var(num_var))
    end if
  else
    allocate(varData_out%var(num_var))
  end if

  sum_elem = 0
  do iVar=1,num_var
    select case(metadata(iVar)%vartype)
      case(iLookVarType%ifcSnow, iLookVarType%ifcSoil, iLookVarType%ifcToto)
        dat_length = varData_in(iVar) - 1
        allocate(varData_out%var(iVar)%dat(0:dat_length))
        ! set the data
        do j=0,dat_length
          varData_out%var(iVar)%dat(j) = dat_array(sum_elem + j)
        end do
        sum_elem = sum_elem + dat_length + 1
      case default
        dat_length = varData_in(iVar)
        allocate(varData_out%var(iVar)%dat(dat_length))
        ! set the data
        do j=1,dat_length
          varData_out%var(iVar)%dat(j) = dat_array(sum_elem + j)
        end do
        sum_elem = sum_elem + dat_length
    end select
  end do


end subroutine

subroutine set_data_var_dlength_by_indx(handle, struct_indx, num_var, var_arr,&
    num_elements, dat_array) bind(C, name='set_data_var_dlength_by_indx')
  USE globalData,only:statForc_meta, statProg_meta, statDiag_meta, &
                      statFlux_meta, statIndx_meta, statBvar_meta
  USE globalData,only:mpar_meta, prog_meta, diag_meta, flux_meta, bvar_meta

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(in)        :: num_var
  integer(c_int), intent(in)        :: var_arr(num_var)
  integer(c_int), intent(in)        :: num_elements
  real(c_double), intent(in)        :: dat_array(num_elements)
  type(hru_type), pointer           :: hru_data

  ! integer(c_int)                    :: i,j,sum_elem

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! forcStat
      call set_data_var_dlength_local(statForc_meta(:)%var_info, &
          hru_data%forcStat, num_var, var_arr, num_elements, dat_array)
    case(2) ! progStat
      call set_data_var_dlength_local(statProg_meta(:)%var_info, &
          hru_data%progStat, num_var, var_arr, num_elements, dat_array)
    case(3) ! diagStat
      call set_data_var_dlength_local(statDiag_meta(:)%var_info, &
          hru_data%diagStat, num_var, var_arr, num_elements, dat_array)
    case(4) ! fluxStat
      call set_data_var_dlength_local(statFlux_meta(:)%var_info, &
          hru_data%fluxStat, num_var, var_arr, num_elements, dat_array)
    case(5) ! indxStat
      call set_data_var_dlength_local(statIndx_meta(:)%var_info, &
          hru_data%indxStat, num_var, var_arr, num_elements, dat_array)
    case(6) ! bvarStat
      call set_data_var_dlength_local(statBvar_meta(:)%var_info, &
          hru_data%bvarStat, num_var, var_arr, num_elements, dat_array)
    case(7) ! mparStruct
      call set_data_var_dlength_local(mpar_meta(:),hru_data%mparStruct, &
          num_var, var_arr, num_elements, dat_array)
    case(8) ! progStruct
      call set_data_var_dlength_local(prog_meta(:), hru_data%progStruct, &
          num_var, var_arr, num_elements, dat_array)
    case(9) ! diagStruct
      call set_data_var_dlength_local(diag_meta(:), hru_data%diagStruct, &
          num_var, var_arr, num_elements, dat_array)
    case(10) ! fluxStruct
      call set_data_var_dlength_local(flux_meta, hru_data%fluxStruct, num_var,&
          var_arr, num_elements, dat_array)
    case(11) ! bvarStruct
      call set_data_var_dlength_local(bvar_meta(:), hru_data%bvarStruct, &
          num_var, var_arr, num_elements, dat_array)
  end select
end subroutine set_data_var_dlength_by_indx

! ****************************** var_dlength ****************************

! ****************************** var_ilength ****************************
subroutine get_size_var_ilength_by_indx(handle, struct_indx, size_var) &
    bind(C, name='get_size_var_ilength_by_indx')
  
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(out)       :: size_var
  type(hru_type), pointer           :: hru_data
  
  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! indxStruct
      if (allocated(hru_data%indxStruct%var)) then
        size_var = size(hru_data%indxStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
  end select
end subroutine get_size_var_ilength_by_indx

subroutine get_size_data_var_ilength_by_indx(handle, struct_indx, size_var, &
    dat_size) bind(C, name='get_size_data_var_ilength_by_indx')

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(in)        :: size_var
  integer(c_int), intent(out)       :: dat_size(*)
  type(hru_type), pointer           :: hru_data
  integer(c_int)                    :: i

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! indxStruct
      do i=1,size_var
        dat_size(i) = size(hru_data%indxStruct%var(i)%dat, kind=c_int)
      end do
  end select

end subroutine get_size_data_var_ilength_by_indx

subroutine get_data_var_ilength_by_indx(handle, struct_indx, dat) &
    bind(C, name='get_data_var_ilength_by_indx')

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(out)       :: dat(*)
  type(hru_type), pointer           :: hru_data
  integer(c_int)                    :: i,j,size_var,size_dat,size_array,j2
  integer(c_int)                    :: start_index(1)

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! indxStruct
      size_array = 0
      if (allocated(hru_data%indxStruct%var)) then
        size_var = size(hru_data%indxStruct%var)
        do i=1,size_var
          size_dat = size(hru_data%indxStruct%var(i)%dat)
          start_index = lbound(hru_data%indxStruct%var(i)%dat)

          if (start_index(1) == 0) then
            size_dat = size_dat - 1
          endif

          j2=1
          do j=start_index(1),size_dat
            dat(size_array+j2) = hru_data%indxStruct%var(i)%dat(j)
            j2=j2+1
          end do
          size_array = size_array + size_dat
        end do
      end if
  end select
end subroutine get_data_var_ilength_by_indx

subroutine set_data_var_ilength_by_indx(handle, struct_indx, num_var, var_arr,&
    num_elements, dat_array) bind(C, name='set_data_var_ilength_by_indx')

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(in)        :: num_var
  integer(c_int), intent(in)        :: var_arr(num_var)
  integer(c_int), intent(in)        :: num_elements
  integer(c_int), intent(in)        :: dat_array(num_elements)
  type(hru_type), pointer           :: hru_data

  integer(c_int)                    :: i,j,sum_elem

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! indxStruct
      ! create the structure if it doesn't exist
      if (allocated(hru_data%indxStruct%var)) then
        print*, "ALLOCATED!!"
        if (size(hru_data%indxStruct%var) /= num_var) then
          deallocate(hru_data%indxStruct%var)
          allocate(hru_data%indxStruct%var(num_var))
          do i=1,num_var
            allocate( hru_data%indxStruct%var(i)%dat(var_arr(i)) )
          end do
        end if
      else
        allocate(hru_data%indxStruct%var(num_var))
        do i=1,num_var
          allocate( hru_data%indxStruct%var(i)%dat(var_arr(i)) )
        end do
      end if

      ! Set the data
      sum_elem = 0
      do i=1,num_var
        do j=1,var_arr(i)
          if (size(hru_data%indxStruct%var(i)%dat) /= var_arr(i)) then
            print*, "ERROR: Size of data array does not match size of data"
            print*, "Size of data array:", size(hru_data%indxStruct%var(i)%dat)
            print*, "Size of data:", dat_array(sum_elem)
            stop
          end if
          hru_data%indxStruct%var(i)%dat(j) = dat_array(sum_elem + j)
        end do
        sum_elem = sum_elem + var_arr(i)
      end do
  end select
end subroutine set_data_var_ilength_by_indx

! ****************************** var_ilength ****************************



! ****************************** var_i ****************************

subroutine get_size_data_var_i_by_indx(handle, struct_indx, size_var) &
    bind(C, name='get_size_data_var_i_by_indx')
  
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(out)       :: size_var
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! timeStruct
      if (allocated(hru_data%timeStruct%var)) then
        size_var = size(hru_data%timeStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(2) ! typeStruct
      if (allocated(hru_data%typeStruct%var)) then
        size_var = size(hru_data%typeStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(3) ! startTime_hru
      if (allocated(hru_data%startTime_hru%var)) then
        size_var = size(hru_data%startTime_hru%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(4) ! finishTime_hru
      if (allocated(hru_data%finishTime_hru%var)) then
        size_var = size(hru_data%finishTime_hru%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(5) ! refTime_hru
      if (allocated(hru_data%refTime_hru%var)) then
        size_var = size(hru_data%refTime_hru%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(6) ! oldTime_hru
      if (allocated(hru_data%oldTime_hru%var)) then
        size_var = size(hru_data%oldTime_hru%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(7) ! statCounter
      if (allocated(hru_data%statCounter%var)) then
        size_var = size(hru_data%statCounter%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(8) ! outputTimeStep
      if (allocated(hru_data%outputTimeStep%var)) then
        size_var = size(hru_data%outputTimeStep%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
  end select
end subroutine get_size_data_var_i_by_indx

subroutine get_data_var_i_by_indx(handle, struct_indx, dat) &
    bind(C, name='get_data_var_i_by_indx')

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(out)          :: dat(*)
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! timeStruct
      if (allocated(hru_data%timeStruct%var)) then
        dat(:size(hru_data%timeStruct%var)) = hru_data%timeStruct%var
      end if
    case(2) ! typeStruct
      if (allocated(hru_data%typeStruct%var)) then
        dat(:size(hru_data%typeStruct%var)) = hru_data%typeStruct%var
      end if
    case(3) ! startTime_hru
      if (allocated(hru_data%startTime_hru%var)) then
        dat(:size(hru_data%startTime_hru%var)) = hru_data%startTime_hru%var
      end if
    case(4) ! finishTime_hru
      if (allocated(hru_data%finishTime_hru%var)) then
        dat(:size(hru_data%finishTime_hru%var)) = hru_data%finishTime_hru%var
      end if
    case(5) ! refTime_hru
      if (allocated(hru_data%refTime_hru%var)) then
        dat(:size(hru_data%refTime_hru%var)) = hru_data%refTime_hru%var
      end if
    case(6) ! oldTime_hru
      if (allocated(hru_data%oldTime_hru%var)) then
        dat(:size(hru_data%oldTime_hru%var)) = hru_data%oldTime_hru%var
      end if
    case(7) ! statCounter
      if (allocated(hru_data%statCounter%var)) then
        dat(:size(hru_data%statCounter%var)) = hru_data%statCounter%var
      end if
    case(8) ! outputTimeStep
      if (allocated(hru_data%outputTimeStep%var)) then
        dat(:size(hru_data%outputTimeStep%var)) = hru_data%outputTimeStep%var
      end if
  end select
end subroutine get_data_var_i_by_indx

subroutine set_data_var_i_by_indx(handle, struct_indx, num_var, summa_struct) &
    bind(C, name="set_data_var_i_by_indx")

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(in)        :: num_var
  integer(c_int), intent(in)        :: summa_struct(num_var)
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! timeStruct
      ! create the structure if it doesn't exist
      if (allocated(hru_data%timeStruct%var)) then
        if (size(hru_data%timeStruct%var) /= num_var) then
          deallocate(hru_data%timeStruct%var)
          allocate(hru_data%timeStruct%var(num_var))
        end if
      else
        allocate(hru_data%timeStruct%var(num_var))
      end if

      ! Set the data
      hru_data%timeStruct%var = summa_struct
    case(2) ! typeStruct
      ! create the structure if it doesn't exist
      if (allocated(hru_data%typeStruct%var)) then
        if (size(hru_data%typeStruct%var) /= num_var) then
          deallocate(hru_data%typeStruct%var)
          allocate(hru_data%typeStruct%var(num_var))
        end if
      else
        allocate(hru_data%typeStruct%var(num_var))
      end if

      ! Set the data
      hru_data%typeStruct%var = summa_struct
    case(3) ! startTime_hru
      ! create the structure if it doesn't exist
      if (allocated(hru_data%startTime_hru%var)) then
        if (size(hru_data%startTime_hru%var) /= num_var) then
          deallocate(hru_data%startTime_hru%var)
          allocate(hru_data%startTime_hru%var(num_var))
        end if
      else
        allocate(hru_data%startTime_hru%var(num_var))
      end if

      ! Set the data
      hru_data%startTime_hru%var = summa_struct
    case(4) ! finish_time
      ! create the structure if it doesn't exist
      if (allocated(hru_data%finishTime_hru%var)) then
        if (size(hru_data%finishTime_hru%var) /= num_var) then
          deallocate(hru_data%finishTime_hru%var)
          allocate(hru_data%finishTime_hru%var(num_var))
        end if
      else
        allocate(hru_data%finishTime_hru%var(num_var))
      end if

      ! Set the data
      hru_data%finishTime_hru%var = summa_struct
    case(5) ! refTime_hru
      ! create the structure if it doesn't exist
      if (allocated(hru_data%refTime_hru%var)) then
        if (size(hru_data%refTime_hru%var) /= num_var) then
          deallocate(hru_data%refTime_hru%var)
          allocate(hru_data%refTime_hru%var(num_var))
        end if
      else
        allocate(hru_data%refTime_hru%var(num_var))
      end if

      ! Set the data
      hru_data%refTime_hru%var = summa_struct
    case(6) ! oldTime_hru
      ! create the structure if it doesn't exist
      if (allocated(hru_data%oldTime_hru%var)) then
        if (size(hru_data%oldTime_hru%var) /= num_var) then
          deallocate(hru_data%oldTime_hru%var)
          allocate(hru_data%oldTime_hru%var(num_var))
        end if
      else
        allocate(hru_data%oldTime_hru%var(num_var))
      end if

      ! Set the data
      hru_data%oldTime_hru%var = summa_struct
    case(7) ! statCounter
      ! create the structure if it doesn't exist
      if (allocated(hru_data%statCounter%var)) then
        if (size(hru_data%statCounter%var) /= num_var) then
          deallocate(hru_data%statCounter%var)
          allocate(hru_data%statCounter%var(num_var))
        end if
      else
        allocate(hru_data%statCounter%var(num_var))
      end if

      ! Set the data
      hru_data%statCounter%var = summa_struct
    case(8) ! outputTimeStep
      ! create the structure if it doesn't exist
      if (allocated(hru_data%outputTimeStep%var)) then
        if (size(hru_data%outputTimeStep%var) /= num_var) then
          deallocate(hru_data%outputTimeStep%var)
          allocate(hru_data%outputTimeStep%var(num_var))
        end if
      else
        allocate(hru_data%outputTimeStep%var(num_var))
      end if

      ! Set the data
      hru_data%outputTimeStep%var = summa_struct
  end select
end subroutine set_data_var_i_by_indx
! ****************************** var_i ****************************

! ****************************** var_d ****************************

subroutine get_size_data_var_d_by_indx(handle, struct_indx, size_var) &
    bind(C, name='get_size_data_var_d_by_indx')
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(out)       :: size_var
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! forcStruct
      if (allocated(hru_data%forcStruct%var)) then
        size_var = size(hru_data%forcStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(2) ! attrStruct
      if (allocated(hru_data%attrStruct%var)) then
        size_var = size(hru_data%attrStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(3) ! bpar_struct
      if (allocated(hru_data%bparStruct%var)) then
        size_var = size(hru_data%bparStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(4) ! dpar_struct
      if (allocated(hru_data%dparStruct%var)) then
        size_var = size(hru_data%dparStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
  end select

end subroutine

subroutine get_data_var_d_by_indx(handle, struct_indx, dat) &
    bind(C, name='get_data_var_d_by_indx')

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  real(c_double), intent(out)       :: dat(*)
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! forcStruct
      if (allocated(hru_data%forcStruct%var)) then
        dat(:size(hru_data%forcStruct%var)) = hru_data%forcStruct%var
      end if
    case(2) ! attrStruct
      if (allocated(hru_data%attrStruct%var)) then
        dat(:size(hru_data%attrStruct%var)) = hru_data%attrStruct%var
      end if
    case(3) ! bpar_struct
      if (allocated(hru_data%bparStruct%var)) then
        dat(:size(hru_data%bparStruct%var)) = hru_data%bparStruct%var
      end if
    case(4) ! dpar_struct
      if (allocated(hru_data%dparStruct%var)) then
        dat(:size(hru_data%dparStruct%var)) = hru_data%dparStruct%var
      end if
  end select
end subroutine get_data_var_d_by_indx

subroutine set_data_var_d_by_indx(handle, struct_indx, num_var, summa_struct) &
    bind(C, name="set_data_var_d_by_indx")
  
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(in)        :: num_var
  real(c_double), intent(in)        :: summa_struct(num_var)
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)
  select case(struct_indx)
    case(1) ! forcStruct
      ! create the structure if it doesn't exist
      if (allocated(hru_data%forcStruct%var)) then
        if (size(hru_data%forcStruct%var) /= num_var) then
          deallocate(hru_data%forcStruct%var)
          allocate(hru_data%forcStruct%var(num_var))
        end if
      else
        allocate(hru_data%forcStruct%var(num_var))
      end if

      ! Set the data
      hru_data%forcStruct%var = summa_struct
    case(2) ! attrStruct
      ! create the structure if it doesn't exist
      if (allocated(hru_data%attrStruct%var)) then
        if (size(hru_data%attrStruct%var) /= num_var) then
          deallocate(hru_data%attrStruct%var)
          allocate(hru_data%attrStruct%var(num_var))
        end if
      else
        allocate(hru_data%attrStruct%var(num_var))
      end if

      ! Set the data
      hru_data%attrStruct%var = summa_struct
    case(3) ! bpar_struct
      ! create the structure if it doesn't exist
      if (allocated(hru_data%bparStruct%var)) then
        if (size(hru_data%bparStruct%var) /= num_var) then
          deallocate(hru_data%bparStruct%var)
          allocate(hru_data%bparStruct%var(num_var))
        end if
      else
        allocate(hru_data%bparStruct%var(num_var))
      end if

      ! Set the data
      hru_data%bparStruct%var = summa_struct
    case(4) ! dpar_struct
      ! create the structure if it doesn't exist
      if (allocated(hru_data%dparStruct%var)) then
        if (size(hru_data%dparStruct%var) /= num_var) then
          deallocate(hru_data%dparStruct%var)
          allocate(hru_data%dparStruct%var(num_var))
        end if
      else
        allocate(hru_data%dparStruct%var(num_var))
      end if

      ! Set the data
      hru_data%dparStruct%var = summa_struct
  end select


end subroutine set_data_var_d_by_indx

! ****************************** var_d ****************************

! ****************************** var_i8 ****************************
subroutine get_size_data_var_i8_by_indx(handle, struct_indx, size_var) &
    bind(C, name='get_size_data_var_i8_by_indx')
  
  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(out)       :: size_var
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! idStruct
      if (allocated(hru_data%idStruct%var)) then
        size_var = size(hru_data%idStruct%var, kind=c_int)
      else
        size_var = 0_c_int
      end if
  end select
end subroutine get_size_data_var_i8_by_indx

subroutine get_data_var_i8_by_indx(handle, struct_indx, dat) &
    bind(C, name='get_data_var_i8_by_indx')

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_long), intent(out)       :: dat(*)
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! idStruct
      if (allocated(hru_data%idStruct%var)) then
        dat(:size(hru_data%idStruct%var)) = hru_data%idStruct%var
      end if
  end select
end subroutine get_data_var_i8_by_indx

subroutine set_data_var_i8_by_indx(handle, struct_indx, num_var, summa_struct) &
    bind(C, name="set_data_var_i8_by_indx")

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(in)        :: num_var
  integer(c_long), intent(in)       :: summa_struct(num_var)
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! idStruct
      if (allocated(hru_data%idStruct%var)) then
        if (size(hru_data%idStruct%var) /= num_var) then
          deallocate(hru_data%idStruct%var)
          allocate(hru_data%idStruct%var(num_var))
          hru_data%idStruct%var = summa_struct
        end if
      else
        allocate(hru_data%idStruct%var(num_var))
        hru_data%idStruct%var = summa_struct
      end if
  end select

end subroutine


! ****************************** var_i8 ****************************

! ****************************** flag_vec ****************************
subroutine get_size_data_flagVec_by_indx(handle, struct_indx, size_var) &
    bind(C, name='get_size_data_flagVec_by_indx')

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(out)       :: size_var
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) !resetStats
      if (allocated(hru_data%resetStats%dat)) then
        size_var = size(hru_data%resetStats%dat, kind=c_int)
      else
        size_var = 0_c_int
      end if
    case(2) !finalizeStats
      if (allocated(hru_data%finalizeStats%dat)) then
        size_var = size(hru_data%finalizeStats%dat, kind=c_int)
      else
        size_var = 0_c_int
      end if
  end select
end subroutine get_size_data_flagVec_by_indx

subroutine get_data_flagVec_by_indx(handle, struct_indx, dat) &
    bind(C, name='get_data_flagVec_by_indx')

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(out)       :: dat(*)
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) !resetStats
      if (allocated(hru_data%resetStats%dat)) then
        dat(:size(hru_data%resetStats%dat)) = merge(1, 0, hru_data%resetStats%dat)
      end if
    case(2) !finalizeStats
      if (allocated(hru_data%finalizeStats%dat)) then
        dat(:size(hru_data%finalizeStats%dat)) = merge(1, 0, hru_data%finalizeStats%dat)
      end if
  end select
end subroutine get_data_flagVec_by_indx

subroutine set_data_flagVec_by_indx(handle, struct_indx, num_var, summa_struct)&
    bind(C, name="set_data_flagVec_by_indx")

  type(c_ptr),    intent(in), value :: handle
  integer(c_int), intent(in)        :: struct_indx
  integer(c_int), intent(in)        :: num_var
  integer(c_int), intent(in)        :: summa_struct(num_var)
  type(hru_type), pointer           :: hru_data

  call c_f_pointer(handle, hru_data)

  select case(struct_indx)
    case(1) ! resetStats
      ! create the structure if it doesn't exist
      if (allocated(hru_data%resetStats%dat)) then
        if (size(hru_data%resetStats%dat) /= num_var) then
          deallocate(hru_data%resetStats%dat)
          allocate(hru_data%resetStats%dat(num_var))
        end if
      else
        allocate(hru_data%resetStats%dat(num_var))
      end if

      ! Set the data
      hru_data%resetStats%dat = merge(.true., .false., summa_struct /= 0)
    case(2) ! finalizeStats
      ! create the structure if it doesn't exist
      if (allocated(hru_data%finalizeStats%dat)) then
        if (size(hru_data%finalizeStats%dat) /= num_var) then
          deallocate(hru_data%finalizeStats%dat)
          allocate(hru_data%finalizeStats%dat(num_var))
        end if
      else
        allocate(hru_data%finalizeStats%dat(num_var))
      end if

      ! Set the data
      hru_data%finalizeStats%dat = merge(.true., .false., summa_struct /= 0)
  end select


end subroutine
! ****************************** flag_vec ****************************

subroutine get_scalar_data_fortran(handle, fracJulDay, tmZoneOffsetFracDay, &
    year_length, computeVegFlux) bind(C, name='get_scalar_data_fortran')
  type(c_ptr), intent(in), value :: handle
  real(c_double), intent(out) :: fracJulDay
  real(c_double), intent(out) :: tmZoneOffsetFracDay
  integer(c_int), intent(out) :: year_length
  integer(c_int), intent(out) :: computeVegFlux
  type(hru_type), pointer :: hru_data

  call c_f_pointer(handle, hru_data)

  fracJulDay = hru_data%fracJulDay
  tmZoneOffsetFracDay = hru_data%tmZoneOffsetFracDay
  year_length = hru_data%yearLength
  computeVegFlux = hru_data%computeVegFlux

end subroutine get_scalar_data_fortran

subroutine set_scalar_data_fortran(handle, fracJulDay, tmZoneOffsetFracDay, &
    year_length, computeVegFlux) bind(C, name='set_scalar_data_fortran')
  type(c_ptr), intent(in), value :: handle
  real(c_double), intent(in) :: fracJulDay
  real(c_double), intent(in) :: tmZoneOffsetFracDay
  integer(c_int), intent(in) :: year_length
  integer(c_int), intent(in) :: computeVegFlux
  type(hru_type), pointer :: hru_data

  call c_f_pointer(handle, hru_data)

  hru_data%fracJulDay = fracJulDay
  hru_data%tmZoneOffsetFracDay = tmZoneOffsetFracDay
  hru_data%yearLength = year_length
  hru_data%computeVegFlux = computeVegFlux

end subroutine set_scalar_data_fortran


! ****************************** hru type ****************************
function new_handle_hru_type() result(handle) bind(C, name="new_handle_hru_type")
  type(c_ptr)            :: handle
  type(hru_type), pointer :: p

  allocate(p)
#ifdef V4_ACTIVE
  allocate(p%lookupStruct)
#endif
  allocate(p%forcStat)
  allocate(p%progStat)
  allocate(p%diagStat)
  allocate(p%fluxStat)
  allocate(p%indxStat)
  allocate(p%bvarStat)
  allocate(p%timeStruct)
  allocate(p%forcStruct)
  allocate(p%attrStruct)
  allocate(p%typeStruct)
  allocate(p%idStruct)
  allocate(p%indxStruct)
  allocate(p%mparStruct)
  allocate(p%progStruct)
  allocate(p%diagStruct)
  allocate(p%fluxStruct)
  allocate(p%bparStruct)
  allocate(p%bvarStruct)
  allocate(p%dparStruct)
  allocate(p%startTime_hru)
  allocate(p%finishTime_hru)
  allocate(p%refTime_hru)
  allocate(p%oldTime_hru)
  allocate(p%statCounter)
  allocate(p%outputTimeStep)
  allocate(p%resetStats)
  allocate(p%finalizeStats)
  handle = c_loc(p)
end function

subroutine delete_handle_hru_type(handle) bind(C, name="delete_handle_hru_type")
  type(c_ptr), intent(in), value :: handle
  type(hru_type), pointer :: p

  call c_f_pointer(handle, p)
#ifdef V4_ACTIVE
  deallocate(p%lookupStruct)
#endif
  deallocate(p%forcStat)
  deallocate(p%progStat)
  deallocate(p%diagStat)
  deallocate(p%fluxStat)
  deallocate(p%indxStat)
  deallocate(p%bvarStat)
  deallocate(p%timeStruct)
  deallocate(p%forcStruct)
  deallocate(p%attrStruct)
  deallocate(p%typeStruct)
  deallocate(p%idStruct)
  deallocate(p%indxStruct)
  deallocate(p%mparStruct)
  deallocate(p%progStruct)
  deallocate(p%diagStruct)
  deallocate(p%fluxStruct)
  deallocate(p%bparStruct)
  deallocate(p%bvarStruct)
  deallocate(p%dparStruct)
  deallocate(p%startTime_hru)
  deallocate(p%finishTime_hru)
  deallocate(p%refTime_hru)
  deallocate(p%oldTime_hru)
  deallocate(p%statCounter)
  deallocate(p%outputTimeStep)
  deallocate(p%resetStats)
  deallocate(p%finalizeStats)
  deallocate(p)

end subroutine

end module cppwrap_datatypes


