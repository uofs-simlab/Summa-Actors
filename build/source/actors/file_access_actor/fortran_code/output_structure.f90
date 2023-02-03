module output_structure_module
  USE nrtype
  USE data_types,only:summa_output_type
  USE, intrinsic :: iso_c_binding
  implicit none
  public::initOutputTimeStep
  public::initOutputStructure
  public::deallocateOutputStructure
  public::deallocateData_output
  
  
  type(summa_output_type),allocatable,save,public :: outputStructure(:) ! summa_OutputStructure(iFile)%struc%var(:)%dat(nTimeSteps) 
  
  contains

subroutine initOutputTimeStep(num_gru, err) bind(C, name="initOutputTimeStep") 
  USE globalData,only:outputTimeStep
  USE var_lookup,only:maxvarFreq                ! maximum number of output files
  implicit none
  integer(c_int), intent(in)  :: num_gru
  integer(c_int), intent(out) :: err
  ! local variables
  integer(i4b)                :: iGRU

  ! initalize outputTimeStep - keeps track of the step the GRU is writing for
  if (.not.allocated(outputTimeStep))then
    allocate(outputTimeStep(num_gru))
    do iGRU = 1, num_gru
      allocate(outputTimeStep(iGRU)%dat(maxVarFreq))
      outputTimeStep(iGRU)%dat(:) = 1
    end do
  end if

end subroutine initOutputTimeStep

subroutine initOutputStructure(handle_forcFileInfo, maxSteps, num_gru, err) bind(C, name="initOutputStructure")
  USE globalData,only:time_meta,forc_meta,attr_meta,type_meta ! metadata structures
  USE globalData,only:prog_meta,diag_meta,flux_meta,id_meta   ! metadata structures
  USE globalData,only:mpar_meta,indx_meta                     ! metadata structures
  USE globalData,only:bpar_meta,bvar_meta                     ! metadata structures
  USE globalData,only:statForc_meta                           ! child metadata for stats
  USE globalData,only:statProg_meta                           ! child metadata for stats
  USE globalData,only:statDiag_meta                           ! child metadata for stats
  USE globalData,only:statFlux_meta                           ! child metadata for stats
  USE globalData,only:statIndx_meta                           ! child metadata for stats
  USE globalData,only:statBvar_meta                           ! child metadata for stats
  USE globalData,only:gru_struc
  USE globalData,only:structInfo                              ! information on the data structures
  USE alloc_outputStructure,only:alloc_outputStruc
  USE multiconst,only:secprday               ! number of seconds in a day
  USE data_types,only:file_info_array
  USE var_lookup,only:maxvarFreq                ! maximum number of output files
  
  implicit none
  type(c_ptr), intent(in), value        :: handle_forcFileInfo
  integer(c_int), intent(in)            :: maxSteps
  integer(c_int), intent(in)            :: num_gru
  integer(c_int), intent(out)           :: err 

  ! local variables
  type(file_info_array), pointer        :: forcFileInfo

  integer(i4b)                          :: nVars
  integer(i4b)                          :: iGRU
  integer(i4b)                          :: iHRU
  integer(i4b)                          :: iStep
  integer(i4b)                          :: nSnow
  integer(i4b)                          :: nSoil
  integer(i4b)                          :: iStruct
  character(len=256)                    :: message
  integer(i4b)                          :: num_hru

  call c_f_pointer(handle_forcFileInfo, forcFileInfo)

  ! Allocate structure to hold output files
  if (.not.allocated(outputStructure))then
    allocate(outputStructure(1))
  else
    print*, "Already Allocated"
    return;
  end if

  ! Statistics Structures
  allocate(outputStructure(1)%forcStat(1))
  allocate(outputStructure(1)%progStat(1))
  allocate(outputStructure(1)%diagStat(1))
  allocate(outputStructure(1)%fluxStat(1))
  allocate(outputStructure(1)%indxStat(1))
  allocate(outputStructure(1)%bvarStat(1))
  allocate(outputStructure(1)%forcStat(1)%gru(num_gru))
  allocate(outputStructure(1)%progStat(1)%gru(num_gru))
  allocate(outputStructure(1)%diagStat(1)%gru(num_gru))
  allocate(outputStructure(1)%fluxStat(1)%gru(num_gru))
  allocate(outputStructure(1)%indxStat(1)%gru(num_gru))
  allocate(outputStructure(1)%bvarStat(1)%gru(num_gru))

  ! Primary Data Structures (scalars)
  allocate(outputStructure(1)%timeStruct(1))
  allocate(outputStructure(1)%forcStruct(1))
  allocate(outputStructure(1)%attrStruct(1))
  allocate(outputStructure(1)%typeStruct(1))
  allocate(outputStructure(1)%idStruct(1))
  allocate(outputStructure(1)%timeStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%forcStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%attrStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%typeStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%idStruct(1)%gru(num_gru))
  
  ! Primary Data Structures (variable length vectors)
  allocate(outputStructure(1)%indxStruct(1))
  allocate(outputStructure(1)%mparStruct(1))
  allocate(outputStructure(1)%progStruct(1))
  allocate(outputStructure(1)%diagStruct(1))
  allocate(outputStructure(1)%fluxStruct(1))
  allocate(outputStructure(1)%indxStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%mparStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%progStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%diagStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%fluxStruct(1)%gru(num_gru))

  ! Basin-Average structures
  allocate(outputStructure(1)%bparStruct(1))
  allocate(outputStructure(1)%bvarStruct(1))
  allocate(outputStructure(1)%bparStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%bvarStruct(1)%gru(num_gru))

  ! define the ancillary data structures
  allocate(outputStructure(1)%dparStruct(1))
  allocate(outputStructure(1)%dparStruct(1)%gru(num_gru))

  ! Finalize Stats for writing
  allocate(outputStructure(1)%finalizeStats(1))
  allocate(outputStructure(1)%finalizeStats(1)%gru(num_gru))
  
  
  do iGRU = 1, num_gru
    num_hru = gru_struc(iGRU)%hruCount
    ! Statistics Structures
    allocate(outputStructure(1)%forcStat(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%progStat(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%diagStat(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%fluxStat(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%indxStat(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%bvarStat(1)%gru(iGRU)%hru(num_hru))

    ! Primary Data Structures (scalars)
    allocate(outputStructure(1)%timeStruct(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%forcStruct(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%attrStruct(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%typeStruct(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%idStruct(1)%gru(iGRU)%hru(num_hru))
  
    ! Primary Data Structures (variable length vectors)
    allocate(outputStructure(1)%indxStruct(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%mparStruct(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%progStruct(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%diagStruct(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%fluxStruct(1)%gru(iGRU)%hru(num_hru))
  
    ! Basin-Average structures
    allocate(outputStructure(1)%bvarStruct(1)%gru(iGRU)%hru(num_hru))

   ! define the ancillary data structures
    allocate(outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(num_hru))

    ! Finalize Stats for writing
    allocate(outputStructure(1)%finalizeStats(1)%gru(iGRU)%hru(num_hru))

  end do

  do iGRU=1,num_gru
    do iHRU=1,gru_struc(iGRU)%hruCount

      ! Get the maximum number of steps needed to initalize the output structure
      nVars = maxval(forcFileInfo%ffile_list(:)%nVars)
      nSnow = gru_struc(iGRU)%hruInfo(iHRU)%nSnow
      nSoil = gru_struc(iGRU)%hruInfo(iHRU)%nSoil

      do iStruct=1,size(structInfo)
        ! allocate space structures
          select case(trim(structInfo(iStruct)%structName))    
            case('time')
              call alloc_outputStruc(time_meta,outputStructure(1)%timeStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,err=err,message=message)     ! model forcing data
            case('forc')
              ! Structure
              call alloc_outputStruc(forc_meta,outputStructure(1)%forcStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! model forcing data
              ! Statistics
              call alloc_outputStruc(statForc_meta(:)%var_info,outputStructure(1)%forcStat(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! model forcing data
            case('attr')
              call alloc_outputStruc(attr_meta,outputStructure(1)%attrStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! local attributes for each HRU
            case('type')
              call alloc_outputStruc(type_meta,outputStructure(1)%typeStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! classification of soil veg etc.
            case('id'  )
              call alloc_outputStruc(id_meta,outputStructure(1)%idStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);        ! local values of hru gru IDs
            case('mpar') ! model parameters
              call alloc_outputStruc(mpar_meta,outputStructure(1)%mparStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message); 

              call alloc_outputStruc(mpar_meta, outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,err=err,message=message)
            case('indx')
              ! Structure
              call alloc_outputStruc(indx_meta,outputStructure(1)%indxStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,str_name='indx',message=message);    ! model variables
              ! Statistics
              call alloc_outputStruc(statIndx_meta(:)%var_info,outputStructure(1)%indxStat(1)%gru(iGRU)%hru(1), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! index vars
            case('prog')
              ! Structure
              call alloc_outputStruc(prog_meta,outputStructure(1)%progStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! model prognostic (state) variables
              ! Statistics
              call alloc_outputStruc(statProg_meta(:)%var_info,outputStructure(1)%progStat(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! model prognostic 
            case('diag')
              ! Structure
              call alloc_outputStruc(diag_meta,outputStructure(1)%diagStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! model diagnostic variables
              print*
              ! Statistics
              call alloc_outputStruc(statDiag_meta(:)%var_info,outputStructure(1)%diagStat(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! model diagnostic
            case('flux')
              ! Structure
              call alloc_outputStruc(flux_meta,outputStructure(1)%fluxStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! model fluxes
              ! Statistics
              call alloc_outputStruc(statFlux_meta(:)%var_info,outputStructure(1)%fluxStat(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=nSnow,nSoil=nSoil,err=err,message=message);    ! model fluxes
            case('bpar')
              call alloc_outputStruc(bpar_meta,outputStructure(1)%bparStruct(1)%gru(iGRU), &
                                      nSteps=maxSteps,nSnow=0,nSoil=0,err=err,message=message);  ! basin-average params 
            case('bvar')
              ! Structure
              call alloc_outputStruc(bvar_meta,outputStructure(1)%bvarStruct(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=0,nSoil=0,err=err,message=message);  ! basin-average variables
              ! Statistics
              call alloc_outputStruc(statBvar_meta(:)%var_info,outputStructure(1)%bvarStat(1)%gru(iGRU)%hru(iHRU), &
                                      nSteps=maxSteps,nSnow=0,nSoil=0,err=err,message=message);  ! basin-average variables
            case('deriv');  cycle
            case('lookup'); cycle
            case default; err=20; message='unable to find structure name: '//trim(structInfo(iStruct)%structName)
        end select

        ! check errors
        if(err/=0)then
          message=trim(message)//'initOutputStruc.f90 - [structure =  '//trim(structInfo(iStruct)%structName)//']'
          print*, "Problem with structure: ", trim(structInfo(iStruct)%structName)
          return
        endif
      end do  ! looping through data structures
    
      ! Finalize stats structure for writing to output file
      allocate(outputStructure(1)%finalizeStats(1)%gru(iGRU)%hru(iHRU)%tim(maxSteps))
      do iStep = 1, maxSteps
        allocate(outputStructure(1)%finalizeStats(1)%gru(iGRU)%hru(iHRU)%tim(iStep)%dat(1:maxVarFreq))
      end do ! timeSteps
    end do ! Looping through GRUs
  end do


end subroutine initOutputStructure

subroutine deallocateOutputStructure(err) bind(C, name="deallocateOutputStructure")
  implicit none
  integer(i4b), intent(inout)   :: err

  err = 0
  ! Time
  deallocate(outputStructure)
  ! call deallocateData_output(outputStructure(1)%timeStruct(1));    deallocate(outputStructure(1)%timeStruct)
  ! ! Forc
  ! call deallocateData_output(outputStructure(1)%forcStat(1));       deallocate(outputStructure(1)%forcStat)
  ! call deallocateData_output(outputStructure(1)%forcStruct(1));     deallocate(outputStructure(1)%forcStruct)
  ! ! prog
  ! call deallocateData_output(outputStructure(1)%progStat(1));       deallocate(outputStructure(1)%progStat)
  ! call deallocateData_output(outputStructure(1)%progStruct(1));     deallocate(outputStructure(1)%progStruct)
  ! ! diag
  ! call deallocateData_output(outputStructure(1)%diagStat(1));       deallocate(outputStructure(1)%diagStat)
  ! call deallocateData_output(outputStructure(1)%diagStruct(1));     deallocate(outputStructure(1)%diagStruct)
  ! ! flux
  ! call deallocateData_output(outputStructure(1)%fluxStat(1));       deallocate(outputStructure(1)%fluxStat)
  ! call deallocateData_output(outputStructure(1)%fluxStruct(1));     deallocate(outputStructure(1)%fluxStruct)
  ! ! indx
  ! call deallocateData_output(outputStructure(1)%indxStat(1));       deallocate(outputStructure(1)%indxStat)
  ! call deallocateData_output(outputStructure(1)%indxStruct(1));     deallocate(outputStructure(1)%indxStruct)
  ! ! bvar
  ! call deallocateData_output(outputStructure(1)%bvarStat(1));       deallocate(outputStructure(1)%bvarStat)
  ! call deallocateData_output(outputStructure(1)%bvarStruct(1));     deallocate(outputStructure(1)%bvarStruct)
  ! ! id
  ! call deallocateData_output(outputStructure(1)%idStruct(1));       deallocate(outputStructure(1)%idStruct)
  ! ! attr
  ! call deallocateData_output(outputStructure(1)%attrStruct(1));     deallocate(outputStructure(1)%attrStruct)
  ! ! type
  ! call deallocateData_output(outputStructure(1)%typeStruct(1));     deallocate(outputStructure(1)%typeStruct)
  ! ! mpar
  ! call deallocateData_output(outputStructure(1)%mparStruct(1));     deallocate(outputStructure(1)%mparStruct)
  ! ! bpar
  ! call deallocateData_output(outputStructure(1)%bparStruct(1));     deallocate(outputStructure(1)%bparStruct)
  ! ! finalize stats
  ! call deallocateData_output(outputStructure(1)%finalizeStats(1));  deallocate(outputStructure(1)%finalizeStats)

end subroutine deallocateOutputStructure

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


end module output_structure_module