module summaActors_deallocateOuptutStruct
  USE nrtype
  implicit none

  contains

subroutine deallocateData_output(dataStruct)
  USE data_types,only:gru_hru_time_doubleVec, &
                      gru_hru_time_intVec, &
                      gru_hru_time_flagVec, &
                      gru_hru_time_int, &
                      gru_hru_int, &
                      gru_hru_time_int8, &
                      gru_hru_time_double, &
                      gru_hru_double, &
                      gru_double
  implicit none
  class(*),intent(inout)      :: dataStruct
  ! local variables
  integer(i4b)                :: iGRU
  integer(i4b)                :: iHRU
  integer(i4b)                :: iVar
  integer(i4b)                :: iTim

  select type(dataStruct)
    class is (gru_hru_time_doubleVec)
      do iGRU = 1, size(dataStruct%gru(:))
        do iHRU = 1, size(dataStruct%gru(iGRU)%hru(:))
          do iVar = 1, size(dataStruct%gru(iGRU)%hru(iHRU)%var(:))
            do iTim = 1, size(dataStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(:))
              deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iTim)%dat)
            end do ! Time
            deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim)
          end do ! var
          deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var)
        end do ! hru
        deallocate(dataStruct%gru(iGRU)%hru)
      end do ! gru
      deallocate(dataStruct%gru)

    class is (gru_hru_time_intVec)
      do iGRU = 1, size(dataStruct%gru(:))
        do iHRU = 1, size(dataStruct%gru(iGRU)%hru(:))
          do iVar = 1, size(dataStruct%gru(iGRU)%hru(iHRU)%var(:))
            do iTim = 1, size(dataStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(:))
              deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iTim)%dat)
            end do ! Time
            deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim)
          end do ! var
          deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var)
        end do ! hru
        deallocate(dataStruct%gru(iGRU)%hru)
      end do ! gru
      deallocate(dataStruct%gru)

    class is (gru_hru_time_flagVec)
      do iGRU = 1, size(dataStruct%gru(:))
        do iHRU = 1, size(dataStruct%gru(iGRU)%hru(:))
          do iTim = 1, size(dataStruct%gru(iGRU)%hru(iHRU)%tim(:))
            deallocate(dataStruct%gru(iGRU)%hru(iHRU)%tim(iTim)%dat)
          end do ! Time
          deallocate(dataStruct%gru(iGRU)%hru(iHRU)%tim)
        end do ! hru
        deallocate(dataStruct%gru(iGRU)%hru)
      end do ! gru
      deallocate(dataStruct%gru)
  
    class is (gru_hru_time_int)
      do iGRU = 1, size(dataStruct%gru(:))
        do iHRU = 1, size(dataStruct%gru(iGRU)%hru(:))
          do iVar = 1, size(dataStruct%gru(iGRU)%hru(iHRU)%var(:))
            deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim)
          end do ! var
          deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var)
        end do ! hru
        deallocate(dataStruct%gru(iGRU)%hru)
      end do ! gru
      deallocate(dataStruct%gru)

    class is (gru_hru_int)
      do iGRU = 1, size(dataStruct%gru(:))
        do iHRU = 1, size(dataStruct%gru(iGRU)%hru(:))
          deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var)
        end do ! hru
        deallocate(dataStruct%gru(iGRU)%hru)
      end do ! gru
      deallocate(dataStruct%gru)

    class is (gru_hru_time_int8)
      do iGRU = 1, size(dataStruct%gru(:))
        do iHRU = 1, size(dataStruct%gru(iGRU)%hru(:))
          do iVar = 1, size(dataStruct%gru(iGRU)%hru(iHRU)%var(:))
            deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim)
          end do ! var
          deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var)
        end do ! hru
        deallocate(dataStruct%gru(iGRU)%hru)
      end do ! gru
      deallocate(dataStruct%gru)

    class is (gru_hru_time_double)
      do iGRU = 1, size(dataStruct%gru(:))
        do iHRU = 1, size(dataStruct%gru(iGRU)%hru(:))
          do iVar = 1, size(dataStruct%gru(iGRU)%hru(iHRU)%var(:))
            deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim)
          end do ! var
          deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var)
        end do ! hru
        deallocate(dataStruct%gru(iGRU)%hru)
      end do ! gru
      deallocate(dataStruct%gru)

    class is (gru_hru_double)
      do iGRU = 1, size(dataStruct%gru(:))
        do iHRU = 1, size(dataStruct%gru(iGRU)%hru(:))
          deallocate(dataStruct%gru(iGRU)%hru(iHRU)%var)
        end do ! hru
        deallocate(dataStruct%gru(iGRU)%hru)
      end do ! gru
      deallocate(dataStruct%gru)

    class is (gru_double)
      do iGRU = 1, size(dataStruct%gru(:))
          deallocate(dataStruct%gru(iGRU)%var)
      end do ! gru
      deallocate(dataStruct%gru)


  end select

end subroutine

end module