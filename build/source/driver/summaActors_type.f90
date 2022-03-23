
MODULE summa4chm_type
! used to define master summa data structure
! *****************************************************************************
! * higher-level derived data types
! *****************************************************************************
USE nrtype          ! variable types, etc.
USE data_types,only:&
                    ! no spatial dimension
                    var_i,               & ! x%var(:)            (i4b)
                    var_i8,              & ! x%var(:)            (i8b)
                    var_d,               & ! x%var(:)            (dp)
                    var_ilength,         & ! x%var(:)%dat        (i4b)
                    var_dlength,         & ! x%var(:)%dat        (dp)
                    gru_hru_doubleVec
implicit none
private

! ************************************************************************
! * master summa data type
! *****************************************************************************


END MODULE summa4chm_type
