module output_structure_module
  USE nrtype
  ! USE data_types,only:summa_output_type
  USE data_types,only:&
                      ! final data vectors
                      dlength,             & ! var%dat
                      ilength,             & ! var%dat
                      ! no spatial dimension
                      var_i,               & ! x%var(:)            (i4b)
                      var_i8,              & ! x%var(:)            integer(8)
                      var_d,               & ! x%var(:)            (rkind)
                      var_flagVec,         & ! x%var(:)%dat        (logical)
                      var_ilength,         & ! x%var(:)%dat        (i4b)
                      var_dlength,         & ! x%var(:)%dat        (rkind)
                      ! gru dimension
                      gru_int,             & ! x%gru(:)%var(:)     (i4b)
                      gru_int8,            & ! x%gru(:)%var(:)     integer(8)
                      gru_double,          & ! x%gru(:)%var(:)     (rkind)
                      gru_intVec,          & ! x%gru(:)%var(:)%dat (i4b)
                      gru_doubleVec,       & ! x%gru(:)%var(:)%dat (rkind)
                      ! gru+hru dimension
                      gru_hru_int,         & ! x%gru(:)%hru(:)%var(:)     (i4b)
                      gru_hru_int8,        & ! x%gru(:)%hru(:)%var(:)     integer(8)
                      gru_hru_double,      & ! x%gru(:)%hru(:)%var(:)     (rkind)
                      gru_hru_intVec,      & ! x%gru(:)%hru(:)%var(:)%dat (i4b)
                      gru_hru_doubleVec,   & ! x%gru(:)%hru(:)%var(:)%dat (rkind)
                      ! gru+hru+z dimension
                      gru_hru_z_vLookup,   & ! x%gru(:)%hru(:)%z(:)%var(:)%lookup (rkind)
                      ! structures that hold the time dimension
                      var_time_i8,         & ! x%var(:)%tim(:)     integer(8)
                      var_time_i,          & ! x%var(:)%tim(:)     (i4b)
                      var_time_d,          & ! x%var(:)%tim(:)     (rkind)
                      var_time_ilength,    & ! x%var(:)%tim(:)     (i4b)
                      var_time_dlength,    & ! x%var(:)%tim(:)     (rkind)
                      gru_hru_time_doublevec, & ! x%gru(:)%hru(:)%var(:)%tim(:)%dat (rkind)
                      gru_hru_time_int,    &  ! x%gru(:)%hru(:)%var(:)%tim(:)     (i4b)
                      gru_hru_time_double, &  ! x%gru(:)%hru(:)%var(:)%tim(:)     (rkind)
                      gru_hru_time_intvec, &  ! x%gru(:)%hru(:)%var(:)%tim(:)%dat (i4b)
                      gru_hru_time_flagvec



  USE data_types,only:var_info
  USE globalData,only:integerMissing
  USE globalData,only:nBand                 ! number of spectral bands
  USE globalData,only:nTimeDelay            ! number of timesteps in the time delay histogram
  USE var_lookup,only:maxvarFreq             ! allocation dimension (output frequency)
  USE var_lookup,only:iLookVarType           ! look up structure for variable typed
  USE var_lookup,only:iLookINDEX
  USE, intrinsic :: iso_c_binding
  implicit none
  public::initOutputTimeStep
  public::initOutputStructure
  public::deallocateOutputStructure
  public::deallocateData_output
  public::alloc_outputStruc
  public::allocateDat_rkind
  public::allocateDat_int
  private::is_var_desired

  type, public :: summa_output_type
    ! define the statistics structures
    type(gru_hru_time_doubleVec),allocatable          :: forcStat(:)                   ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model forcing data
    type(gru_hru_time_doubleVec),allocatable          :: progStat(:)                   ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model prognostic (state) variables
    type(gru_hru_time_doubleVec),allocatable          :: diagStat(:)                   ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model diagnostic variables
    type(gru_hru_time_doubleVec),allocatable          :: fluxStat(:)                   ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model fluxes
    type(gru_hru_time_doubleVec),allocatable          :: indxStat(:)                   ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model indices
    type(gru_hru_time_doubleVec),allocatable          :: bvarStat(:)                   ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- basin-average variabl

    ! define the primary data structures (scalars)
    type(gru_hru_time_int),allocatable                :: timeStruct(:)                 ! x%gru(:)%hru(:)%var(:)%tim(:)     -- model time data
    type(gru_hru_time_double),allocatable             :: forcStruct(:)                 ! x%gru(:)%hru(:)%var(:)%tim(:)     -- model forcing data
    type(gru_hru_double),allocatable                  :: attrStruct(:)                 ! x%gru(:)%hru(:)%var(:)            -- local attributes for each HRU, DOES NOT CHANGE OVER TIMESTEPS
    type(gru_hru_int),allocatable                     :: typeStruct(:)                 ! x%gru(:)%hru(:)%var(:)%tim(:)     -- local classification of soil veg etc. for each HRU, DOES NOT CHANGE OVER TIMESTEPS
    type(gru_hru_int8),allocatable                    :: idStruct(:)                   ! x%gru(:)%hru(:)%var(:)

    ! define the primary data structures (variable length vectors)
    type(gru_hru_time_intVec),allocatable             :: indxStruct(:)                 ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model indices
    type(gru_hru_doubleVec),allocatable               :: mparStruct(:)                 ! x%gru(:)%hru(:)%var(:)%dat        -- model parameters, DOES NOT CHANGE OVER TIMESTEPS TODO: MAYBE
    type(gru_hru_time_doubleVec),allocatable          :: progStruct(:)                 ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model prognostic (state) variables
    type(gru_hru_time_doubleVec),allocatable          :: diagStruct(:)                 ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model diagnostic variables
    type(gru_hru_time_doubleVec),allocatable          :: fluxStruct(:)                 ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model fluxes

    ! define the basin-average structures
    type(gru_double),allocatable                      :: bparStruct(:)                 ! x%gru(:)%var(:)                   -- basin-average parameters, DOES NOT CHANGE OVER TIMESTEPS
    type(gru_hru_time_doubleVec),allocatable          :: bvarStruct(:)                 ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- basin-average variables

    ! define the ancillary data structures
    type(gru_hru_double),allocatable                  :: dparStruct(:)                 ! x%gru(:)%hru(:)%var(:)

    ! finalize stats structure
    type(gru_hru_time_flagVec),allocatable            :: finalizeStats(:)              ! x%gru(:)%hru(:)%tim(:)%dat -- flags on when to write to file

    integer(i4b)                                      :: nTimeSteps
  end type summa_output_type  
  
  
  type(summa_output_type),allocatable,save,public :: outputStructure(:) ! summa_OutputStructure(iFile)%struc%var(:)%dat(nTimeSteps) 
  
  contains

subroutine initOutputTimeStep(num_gru, err)
  USE globalData,only:outputTimeStep
  USE var_lookup,only:maxvarFreq                ! maximum number of output files
  implicit none
  integer(i4b), intent(in)  :: num_gru
  integer(i4b), intent(out) :: err
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

subroutine initOutputStructure(forcFileInfo, maxSteps, num_gru, err)
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
  USE multiconst,only:secprday               ! number of seconds in a day
  USE data_types,only:file_info_array
  USE var_lookup,only:maxvarFreq                ! maximum number of output files
  
  implicit none
  type(file_info_array),intent(in)      :: forcFileInfo
  integer(i4b), intent(in)              :: maxSteps
  integer(i4b), intent(in)              :: num_gru
  integer(i4b), intent(out)             :: err 

  ! local variables

  integer(i4b)                          :: nVars
  integer(i4b)                          :: iGRU
  integer(i4b)                          :: iHRU
  integer(i4b)                          :: iStep
  integer(i4b)                          :: nSnow
  integer(i4b)                          :: nSoil
  integer(i4b)                          :: iStruct
  character(len=256)                    :: message
  integer(i4b)                          :: num_hru


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
  deallocate(outputStructure)

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

logical function is_var_desired(metaStruct, iVar)
  implicit none
  type(var_info),intent(in) :: metaStruct(:)
  integer(i4b),intent(in)   :: iVar
  ! local
  integer(i4b)              :: iFreq
  ! initalize error control
  is_var_desired=.false.
  do iFreq=1,maxvarFreq
    if(metaStruct(iVar)%statIndex(iFreq) /= integerMissing)then
      is_var_desired=.true.
      exit
    end if
  end do

end function is_var_desired

subroutine alloc_outputStruc(metaStruct,dataStruct,nSteps,nSnow,nSoil,str_name,err,message)
  implicit none
  type(var_info),intent(in)            :: metaStruct(:)
  class(*),intent(inout)               :: dataStruct
  ! optional input
  integer(i4b),intent(in),optional     :: nSteps
  integer(i4b),intent(in),optional     :: nSnow          ! number of snow layers
  integer(i4b),intent(in),optional     :: nSoil          ! number of soil layers
  character(len=*),intent(in),optional :: str_name    ! name of the structure to allocate
  ! output
  integer(i4b),intent(inout)           :: err            ! error code
  character(*),intent(out)             :: message        ! error message
  ! local
  logical(lgt)                         :: check          ! .true. if the variables are allocated
  integer(i4b)                         :: nVars          ! number of variables in the metadata structure
  integer(i4b)                         :: nLayers        ! total number of layers
  integer(i4b)                         :: iVar
  integer(i4b)                         :: iStat          ! checks if we want this variable
  character(len=256)                   :: cmessage       ! error message of the downwind routine
  ! initalize error control
  message='alloc_outputStruc'

  nVars = size(metaStruct)
  if(present(nSnow) .or. present(nSoil))then
    ! check both are present
    if(.not.present(nSoil))then; err=20; message=trim(message)//'expect nSoil to be present when nSnow is present'; print*,message; return; end if
    if(.not.present(nSnow))then; err=20; message=trim(message)//'expect nSnow to be present when nSoil is present'; print*,message; return; end if
    nLayers = nSnow+nSoil
    ! It is possible that nSnow and nSoil are actually needed here, so we return an error if the optional arguments are missing when needed
  else
    select type(dataStruct)
      class is (var_time_ilength); err=20
      class is (var_time_dlength); err=20
    end select
    if(err/=0)then; message=trim(message)//'expect nSnow and nSoil to be present for variable-length data structures'; print*,message; return; end if
  end if

  check=.false.
  ! allocate the space for the variables and thier time steps in the output structure
  select type(dataStruct)
    ! ****************************************************
    class is (var_time_i)
      if(allocated(dataStruct%var))then
        check=.true.
      else 
        allocate(dataStruct%var(nVars),stat=err)
      end if
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar))then
          allocate(dataStruct%var(iVar)%tim(nSteps))
        end if
      end do
      return
    ! ****************************************************
    class is (var_time_i8)
      if(allocated(dataStruct%var))then 
        check=.true.
      else 
        allocate(dataStruct%var(nVars),stat=err) 
      end if 
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar))then
          allocate(dataStruct%var(iVar)%tim(nSteps))
        end if
      end do
      return
    ! ****************************************************
    class is (var_time_d)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars),stat=err)
      end if
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar))then
          allocate(dataStruct%var(iVar)%tim(nSteps))
        end if
      end do
      return
    ! ****************************************************   
    class is (var_d)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars),stat=err)
      end if
      return
    ! ****************************************************
    class is (var_i)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars),stat=err)
      end if
      return
    ! ****************************************************    
    class is (var_i8)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars), stat=err)
      end if
      return
    ! ****************************************************    
    class is (var_dlength)
      if(allocated(dataStruct%var))then
        check=.true.
      else
        allocate(dataStruct%var(nVars),stat=err)
        call allocateDat_rkind(metaStruct,dataStruct,nSnow,nSoil,err,cmessage)
      end if
    ! ****************************************************
    class is (var_time_ilength)
      if(allocated(dataStruct%var))then
        check=.true. 
      else 
        allocate(dataStruct%var(nVars),stat=err) 
      end if
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar) .or. (present(str_name) .and. &
         ((iVar == iLookINDEX%nLayers) .or. (iVar == iLookINDEX%nSnow) .or. (iVar == iLookINDEX%nSoil)) ))then
        allocate(dataStruct%var(iVar)%tim(nSteps))
          call allocateDat_int(metaStruct,dataStruct,nSnow,nSoil,nSteps,iVar,err,cmessage)
        end if
      end do
    ! ****************************************************
    class is (var_time_dlength)
      if(allocated(dataStruct%var))then
        check=.true.
      else 
        allocate(dataStruct%var(nVars),stat=err)
      end if
      do iVar=1, nVars
        ! Check if this variable is desired within any timeframe
        if(is_var_desired(metaStruct,iVar))then
          allocate(dataStruct%var(iVar)%tim(nSteps))
          call allocateDat_rkind_nSteps(metaStruct,dataStruct,nSnow,nSoil,nSteps,iVar,err,cmessage)
        end if
      end do
    ! ****************************************************
    class default; err=20; message=trim(message)//'unable to identify derived data type for the variable dimension'; print*,message;return
  end select
  ! check errors
  if(check) then; err=20; message=trim(message)//'structure was unexpectedly allocated already'; print*,message; return; end if
  if(err/=0)then; err=20; message=trim(message)//'problem allocating'; print*,message; return; end if

  ! check errors
  if(err/=0)then; message=trim(message)//trim(cmessage); print*, message; return; end if
end subroutine

subroutine allocateDat_rkind_nSteps(metadata,varData,nSnow, nSoil, &
  nSteps,iVar,err,message)

  USE get_ixName_module,only:get_varTypeName       ! to access type strings for error messages

  implicit none
  type(var_info),intent(in)            :: metadata(:)
  ! output variables
  type(var_time_dlength),intent(inout) :: varData     ! model variables for a local HRU
  integer(i4b),intent(in)              :: nSnow
  integer(i4b),intent(in)              :: nSoil
  integer(i4b),intent(in)              :: nSteps
  integer(i4b),intent(in)              :: iVar
  integer(i4b),intent(inout)           :: err         ! error code
  character(*),intent(inout)           :: message     ! error message

  ! local variables
  integer(i4b)                         :: iStep 
  integer(i4b)                         :: nLayers
  message='allocateDat_rkindAccessActor'

  nLayers = nSnow+nSoil
  do iStep=1, nSteps
    select case(metadata(iVar)%vartype)
      case(iLookVarType%scalarv); allocate(varData%var(iVar)%tim(iStep)%dat(1),stat=err)
      case(iLookVarType%wLength); allocate(varData%var(iVar)%tim(iStep)%dat(nBand),stat=err)
      case(iLookVarType%midSnow); allocate(varData%var(iVar)%tim(iStep)%dat(nSnow),stat=err)
      case(iLookVarType%midSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
      case(iLookVarType%midToto); allocate(varData%var(iVar)%tim(iStep)%dat(nLayers),stat=err)
      case(iLookVarType%ifcSnow); allocate(varData%var(iVar)%tim(iStep)%dat(0:nSnow),stat=err)
      case(iLookVarType%ifcSoil); allocate(varData%var(iVar)%tim(iStep)%dat(0:nSoil),stat=err)
      case(iLookVarType%ifcToto); allocate(varData%var(iVar)%tim(iStep)%dat(0:nLayers),stat=err)
      case(iLookVarType%parSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
      case(iLookVarType%routing); allocate(varData%var(iVar)%tim(iStep)%dat(nTimeDelay),stat=err)
      case(iLookVarType%outstat); allocate(varData%var(iVar)%tim(iStep)%dat(maxvarfreq*2),stat=err)
      case(iLookVarType%unknown); allocate(varData%var(iVar)%tim(iStep)%dat(0),stat=err)
      case default
      err=40; message=trim(message)//"1. unknownVariableType[name='"//trim(metadata(iVar)%varname)//"'; type='"//trim(get_varTypeName(metadata(iVar)%vartype))//"']"
      return
    end select
  end do ! (iStep)

end subroutine allocateDat_rkind_nSteps

subroutine allocateDat_rkind(metadata,varData,nSnow,nSoil,err,message)
  USE get_ixName_module,only:get_varTypeName       ! to access type strings for error messages
  implicit none
  type(var_info),intent(in)         :: metadata(:)
  ! output variables
  type(var_dlength),intent(inout)   :: varData     ! model variables for a local HRU
  integer(i4b),intent(in)           :: nSnow
  integer(i4b),intent(in)           :: nSoil
  
  integer(i4b),intent(inout)        :: err         ! error code
  character(*),intent(inout)        :: message     ! error message
  
  ! local variables
  integer(i4b)                      :: nVars
  integer(i4b)                      :: iVar
  integer(i4b)                      :: nLayers
  message='allocateDat_rkindAccessActor'

  nVars = size(metaData)
  nLayers = nSnow+nSoil
  do iVar=1, nVars
    select case(metadata(iVar)%vartype)
    case(iLookVarType%scalarv); allocate(varData%var(iVar)%dat(1),stat=err)
    case(iLookVarType%wLength); allocate(varData%var(iVar)%dat(nBand),stat=err)
    case(iLookVarType%midSnow); allocate(varData%var(iVar)%dat(nSnow),stat=err)
    case(iLookVarType%midSoil); allocate(varData%var(iVar)%dat(nSoil),stat=err)
    case(iLookVarType%midToto); allocate(varData%var(iVar)%dat(nLayers),stat=err)
    case(iLookVarType%ifcSnow); allocate(varData%var(iVar)%dat(0:nSnow),stat=err)
    case(iLookVarType%ifcSoil); allocate(varData%var(iVar)%dat(0:nSoil),stat=err)
    case(iLookVarType%ifcToto); allocate(varData%var(iVar)%dat(0:nLayers),stat=err)
    case(iLookVarType%parSoil); allocate(varData%var(iVar)%dat(nSoil),stat=err)
    case(iLookVarType%routing); allocate(varData%var(iVar)%dat(nTimeDelay),stat=err)
    case(iLookVarType%outstat); allocate(varData%var(iVar)%dat(maxvarfreq*2),stat=err)
    case(iLookVarType%unknown); allocate(varData%var(iVar)%dat(0),stat=err)
    case default
        err=40; message=trim(message)//"1. unknownVariableType[name='"//trim(metadata(iVar)%varname)//"'; type='"//trim(get_varTypeName(metadata(iVar)%vartype))//"']"
        return
    end select
  end do

end subroutine allocateDat_rkind

subroutine allocateDat_int(metadata,varData,nSnow, nSoil, &
                           nSteps,iVar,err,message)
  USE get_ixName_module,only:get_varTypeName       ! to access type strings for error messages

  implicit none
  type(var_info),intent(in)            :: metadata(:)
  ! output variables
  type(var_time_ilength),intent(inout) :: varData     ! model variables for a local HRU
  integer(i4b),intent(in)              :: nSnow
  integer(i4b),intent(in)              :: nSoil
  integer(i4b),intent(in)              :: nSteps
  integer(i4b),intent(in)              :: iVar  
  integer(i4b),intent(inout)           :: err         ! error code
  character(*),intent(inout)           :: message     ! error message
  ! local variables
  integer(i4b)                         :: iStep 
  integer(i4b)                         :: nLayers
  message='allocateDat_rkindAccessActor'

  nLayers = nSnow+nSoil
  do iStep=1, nSteps
    select case(metadata(iVar)%vartype)
      case(iLookVarType%scalarv); allocate(varData%var(iVar)%tim(iStep)%dat(1),stat=err)
      case(iLookVarType%wLength); allocate(varData%var(iVar)%tim(iStep)%dat(nBand),stat=err)
      case(iLookVarType%midSnow); allocate(varData%var(iVar)%tim(iStep)%dat(nSnow),stat=err)
      case(iLookVarType%midSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
      case(iLookVarType%midToto); allocate(varData%var(iVar)%tim(iStep)%dat(nLayers),stat=err)
      case(iLookVarType%ifcSnow); allocate(varData%var(iVar)%tim(iStep)%dat(0:nSnow),stat=err)
      case(iLookVarType%ifcSoil); allocate(varData%var(iVar)%tim(iStep)%dat(0:nSoil),stat=err)
      case(iLookVarType%ifcToto); allocate(varData%var(iVar)%tim(iStep)%dat(0:nLayers),stat=err)
      case(iLookVarType%parSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
      case(iLookVarType%routing); allocate(varData%var(iVar)%tim(iStep)%dat(nTimeDelay),stat=err)
      case(iLookVarType%outstat); allocate(varData%var(iVar)%tim(iStep)%dat(maxvarfreq*2),stat=err)
      case(iLookVarType%unknown); allocate(varData%var(iVar)%tim(iStep)%dat(0),stat=err)
      case default
      err=40; message=trim(message)//"1. unknownVariableType[name='"//trim(metadata(iVar)%varname)//"'; type='"//trim(get_varTypeName(metadata(iVar)%vartype))//"']"
      return
    end select
  end do ! loop through time steps
end subroutine


end module output_structure_module