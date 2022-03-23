module summaActors_deallocateOuptutStruct
  USE nrtype
  implicit none
  public::deallocateOutputStruc

  contains
subroutine deallocateOutputStruc(err)
  USE globalData,only:outputStructure
  implicit none
  integer(i4b), intent(inout)   :: err

  err = 0
  ! Time
  call deallocateData_output(outputStructure(1)%timeStruct(1));    deallocate(outputStructure(1)%timeStruct)
  ! Forc
  call deallocateData_output(outputStructure(1)%forcStat(1));       deallocate(outputStructure(1)%forcStat)
  call deallocateData_output(outputStructure(1)%forcStruct(1));     deallocate(outputStructure(1)%forcStruct)
  ! prog
  call deallocateData_output(outputStructure(1)%progStat(1));       deallocate(outputStructure(1)%progStat)
  call deallocateData_output(outputStructure(1)%progStruct(1));     deallocate(outputStructure(1)%progStruct)
  ! diag
  call deallocateData_output(outputStructure(1)%diagStat(1));       deallocate(outputStructure(1)%diagStat)
  call deallocateData_output(outputStructure(1)%diagStruct(1));     deallocate(outputStructure(1)%diagStruct)
  ! flux
  call deallocateData_output(outputStructure(1)%fluxStat(1));       deallocate(outputStructure(1)%fluxStat)
  call deallocateData_output(outputStructure(1)%fluxStruct(1));     deallocate(outputStructure(1)%fluxStruct)
  ! indx
  call deallocateData_output(outputStructure(1)%indxStat(1));       deallocate(outputStructure(1)%indxStat)
  call deallocateData_output(outputStructure(1)%indxStruct(1));     deallocate(outputStructure(1)%indxStruct)
  ! bvar
  call deallocateData_output(outputStructure(1)%bvarStat(1));       deallocate(outputStructure(1)%bvarStat)
  call deallocateData_output(outputStructure(1)%bvarStruct(1));     deallocate(outputStructure(1)%bvarStruct)
  ! id
  call deallocateData_output(outputStructure(1)%idStruct(1));       deallocate(outputStructure(1)%idStruct)
  ! attr
  call deallocateData_output(outputStructure(1)%attrStruct(1));     deallocate(outputStructure(1)%attrStruct)
  ! type
  call deallocateData_output(outputStructure(1)%typeStruct(1));     deallocate(outputStructure(1)%typeStruct)
  ! mpar
  call deallocateData_output(outputStructure(1)%mparStruct(1));     deallocate(outputStructure(1)%mparStruct)
  ! bpar
  call deallocateData_output(outputStructure(1)%bparStruct(1));     deallocate(outputStructure(1)%bparStruct)
  ! dpar
  call deallocateData_output(outputStructure(1)%dparStruct(1));     deallocate(outputStructure(1)%dparStruct)
  ! finalize stats
  call deallocateData_output(outputStructure(1)%finalizeStats(1));  deallocate(outputStructure(1)%finalizeStats)

end subroutine deallocateOutputStruc

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