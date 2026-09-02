module cppwrap_datatypes

! Minimal C-interoperable constructors/destructors for the opaque Fortran handles used by the
! Summa-Actors C++ layer.
!
! History: this module previously exported ~100 bind(C) accessor routines (get_data_ / set_data_
! families, the by_indx variants, get_scalar_data_fortran, new_handle_hru_type, ...) that existed
! only to serialize per-HRU state to C++ for the old per-HRU actor design.  That design was replaced
! by the per-GRU actor model, which passes a single opaque gru_type handle around and never
! serializes.  Everything except the four handle routines below was dead (its only callers were
! global auxiliary.cpp and the hru_actor sources, all now removed) and has been deleted.
! NOTE: this file is preprocessed with -cpp, so avoid the two-character sequences that start or end
! a C block comment in the text above.

use, intrinsic :: iso_c_binding
use data_types
use actor_data_types

implicit none

contains

! **************************** var_i ****************************
! Used for the per-file NetCDF id array (`ncid`) handle in the file-access / output-buffer path.

function new_handle_var_i() result(handle) bind(C, name='new_handle_var_i')
  type(c_ptr) :: handle
  type(var_i), pointer :: p

  allocate(p)
  handle = c_loc(p)
end function new_handle_var_i

subroutine delete_handle_var_i(handle) bind(C, name='delete_handle_var_i')
  type(c_ptr), intent(in), value :: handle
  type(var_i), pointer :: p

  call c_f_pointer(handle, p)
  deallocate(p)
end subroutine delete_handle_var_i

! **************************** gru_type ****************************
! The opaque per-GRU data handle owned by the GRU actor (build/source/gru_actor/gru_actor.cpp).
! Allocates the fixed set of pointer targets on `gru_type`/`hru_type`; the variable-length `%dom(:)`
! / `%var(:)` / `%tim(:)` contents are filled later by initHRU / allocateOutputBuffer.

function new_handle_gru_type(num_hru) result(handle) bind(C, name="new_handle_gru_type")
  type(c_ptr)                :: handle
  integer(c_int), intent(in) :: num_hru
  type(gru_type), pointer    :: p
  integer(c_int)             :: i

  allocate(p)
  allocate(p%hru(num_hru))
  allocate(p%bvarStat)
  allocate(p%bvarStruct)
  allocate(p%gridStruct)

  do i=1,num_hru
    allocate(p%hru(i)%lookupStruct)
    allocate(p%hru(i)%forcStat)
    allocate(p%hru(i)%progStat)
    allocate(p%hru(i)%diagStat)
    allocate(p%hru(i)%fluxStat)
    allocate(p%hru(i)%indxStat)
    allocate(p%hru(i)%bvarStat)
    allocate(p%hru(i)%timeStruct)
    allocate(p%hru(i)%forcStruct)
    allocate(p%hru(i)%attrStruct)
    allocate(p%hru(i)%typeStruct)
    allocate(p%hru(i)%idStruct)
    allocate(p%hru(i)%indxStruct)
    allocate(p%hru(i)%mparStruct)
    allocate(p%hru(i)%progStruct)
    allocate(p%hru(i)%diagStruct)
    allocate(p%hru(i)%fluxStruct)
    allocate(p%hru(i)%bparStruct)
    allocate(p%hru(i)%bvarStruct)
    allocate(p%hru(i)%dparStruct)
    allocate(p%hru(i)%startTime_hru)
    allocate(p%hru(i)%finishTime_hru)
    allocate(p%hru(i)%refTime_hru)
    allocate(p%hru(i)%oldTime_hru)
    allocate(p%hru(i)%statCounter)
    allocate(p%hru(i)%outputTimeStep)
    allocate(p%hru(i)%resetStats)
    allocate(p%hru(i)%finalizeStats)
  end do

  handle = c_loc(p)
end function new_handle_gru_type

subroutine delete_handle_gru_type(handle) bind(C, name="delete_handle_gru_type")
  type(c_ptr), intent(in), value :: handle
  type(gru_type), pointer :: p
  integer(c_int)          :: i

  call c_f_pointer(handle, p)

  do i = 1, size(p%hru)
    deallocate(p%hru(i)%lookupStruct)
    deallocate(p%hru(i)%forcStat)
    deallocate(p%hru(i)%progStat)
    deallocate(p%hru(i)%diagStat)
    deallocate(p%hru(i)%fluxStat)
    deallocate(p%hru(i)%indxStat)
    deallocate(p%hru(i)%bvarStat)
    deallocate(p%hru(i)%timeStruct)
    deallocate(p%hru(i)%forcStruct)
    deallocate(p%hru(i)%attrStruct)
    deallocate(p%hru(i)%typeStruct)
    deallocate(p%hru(i)%idStruct)
    deallocate(p%hru(i)%indxStruct)
    deallocate(p%hru(i)%mparStruct)
    deallocate(p%hru(i)%progStruct)
    deallocate(p%hru(i)%diagStruct)
    deallocate(p%hru(i)%fluxStruct)
    deallocate(p%hru(i)%bparStruct)
    deallocate(p%hru(i)%bvarStruct)
    deallocate(p%hru(i)%dparStruct)
    deallocate(p%hru(i)%startTime_hru)
    deallocate(p%hru(i)%finishTime_hru)
    deallocate(p%hru(i)%refTime_hru)
    deallocate(p%hru(i)%oldTime_hru)
    deallocate(p%hru(i)%statCounter)
    deallocate(p%hru(i)%outputTimeStep)
    deallocate(p%hru(i)%resetStats)
    deallocate(p%hru(i)%finalizeStats)
  end do

  deallocate(p%hru)
  deallocate(p%bvarStat)
  deallocate(p%bvarStruct)
  deallocate(p%gridStruct)
  deallocate(p)
end subroutine delete_handle_gru_type

end module cppwrap_datatypes
