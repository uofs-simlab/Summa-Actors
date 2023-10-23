module INIT_HRU_ACTOR
! used to declare and allocate summa data structures and initialize model state to known values
USE,intrinsic :: iso_c_binding
USE nrtype          ! variable types, etc.
USE data_types,only:&
                    ! no spatial dimension
                    var_i,               & ! x%var(:)            (i4b)
                    var_i8,              & ! x%var(:)            (i8b)
                    var_d,               & ! x%var(:)            (dp)
                    var_ilength,         & ! x%var(:)%dat        (i4b)
                    var_dlength,         & ! x%var(:)%dat        (dp)
                    zLookup,             & ! x%z(:)%var(:)%lookup(:) -- lookup tables
                    hru_type             ! hru_type
                    
! access missing values
USE globalData,only:integerMissing   ! missing integer
USE globalData,only:realMissing      ! missing double precision number
! named variables for run time options
USE globalData,only:iRunModeFull,iRunModeGRU,iRunModeHRU
! metadata structures
USE globalData,only:time_meta,forc_meta,attr_meta,type_meta ! metadata structures
USE globalData,only:prog_meta,diag_meta,flux_meta,id_meta   ! metadata structures
USE globalData,only:mpar_meta,indx_meta                     ! metadata structures
USE globalData,only:bpar_meta,bvar_meta                     ! metadata structures
USE globalData,only:averageFlux_meta                        ! metadata for time-step average fluxes
! USE globalData,only:lookup_meta 

! statistics metadata structures
USE globalData,only:statForc_meta                           ! child metadata for stats
USE globalData,only:statProg_meta                           ! child metadata for stats
USE globalData,only:statDiag_meta                           ! child metadata for stats
USE globalData,only:statFlux_meta                           ! child metadata for stats
USE globalData,only:statIndx_meta                           ! child metadata for stats
USE globalData,only:statBvar_meta                           ! child metadata for stats
! maxvarFreq 
USE var_lookup,only:maxVarFreq                               ! # of available output frequencies

! safety: set private unless specified otherwise
implicit none
private
public::initHRU
contains

 ! used to declare and allocate summa data structures and initialize model state to known values
 subroutine initHRU(indxGRU,            & !  Index of HRU's GRU parent
                    num_steps,          &
                    handle_hru_data,    &
                    err) bind(C,name='initHRU')
  ! ---------------------------------------------------------------------------------------
  ! * desired modules
  ! ---------------------------------------------------------------------------------------
  ! data types
  USE nrtype                                                  ! variable types, etc.
  ! subroutines and functions: allocate space
  USE allocspace_module,only:allocLocal
  ! timing variables
  USE globalData,only:startInit,endInit                       ! date/time for the start and end of the initialization
  USE globalData,only:elapsedRead                             ! elapsed time for the data read
  USE globalData,only:elapsedWrite                            ! elapsed time for the stats/write
  USE globalData,only:elapsedPhysics                          ! elapsed time for the physics
  ! miscellaneous global data
  USE globalData,only:gru_struc                               ! gru-hru mapping structures
  USE globalData,only:structInfo                              ! information on the data structures
  USE globalData,only:numtim
  USE globalData,only:startTime,finshTime,refTime,oldTime

  USE var_lookup,only:maxvarFreq                              ! maximum number of output files
  USE var_lookup,only:iLookFreq                               ! output frequency lookup table
  implicit none
  
  ! ---------------------------------------------------------------------------------------
  ! * variables from C++
  ! ---------------------------------------------------------------------------------------
  integer(c_int),intent(in)                  :: indxGRU                    ! indx of the parent GRU
  integer(c_int),intent(out)                 :: num_steps                  ! number of steps in model, local to the HRU                 
  type(c_ptr), intent(in), value             :: handle_hru_data            ! hru data structure (hru_type
  ! ancillary data structures
  integer(c_int),intent(inout)               :: err  
  ! ---------------------------------------------------------------------------------------
  ! * Fortran Variables For Conversion
  ! ---------------------------------------------------------------------------------------
  type(hru_type),pointer                     :: hru_data                   ! hru data structure (hru_type
  ! ---------------------------------------------------------------------------------------
  ! * Local Subroutine Variables
  ! ---------------------------------------------------------------------------------------
  character(LEN=256)                         :: message                    ! error message
  character(LEN=256)                         :: cmessage                   ! error message of downwind routine
  integer(i4b)                               :: iStruct                    ! looping variables
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_hru_data,   hru_data)
  ! ---------------------------------------------------------------------------------------
  ! initialize error control
  err=0; message='hru_init/'

  ! initialize the start of the initialization
  call date_and_time(values=startInit)

  ! initialize the elapsed time for cumulative quantities
  elapsedRead=0._dp
  elapsedWrite=0._dp
  elapsedPhysics=0._dp

  ! copy the number of the steps for the hru
  num_steps = numtim

  ! *****************************************************************************
  ! *** allocate space for data structures
  ! *****************************************************************************

  ! allocate time structures
  do iStruct=1,4
  select case(iStruct)
    case(1); call allocLocal(time_meta, hru_data%startTime_hru, err=err, message=cmessage)  ! start time for the model simulation
    case(2); call allocLocal(time_meta, hru_data%finishTime_hru, err=err, message=cmessage)  ! end time for the model simulation
    case(3); call allocLocal(time_meta, hru_data%refTime_hru,   err=err, message=cmessage)  ! reference time for the model simulation
    case(4); call allocLocal(time_meta, hru_data%oldTime_hru,   err=err, message=cmessage)  ! time from the previous step
  end select
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  end do  ! looping through time structures

  ! copy the time variables set up by the job_actor
  hru_data%startTime_hru%var(:) = startTime%var(:)
  hru_data%finishTime_hru%var(:) = finshTime%var(:)
  hru_data%refTime_hru%var(:) = refTime%var(:)
  hru_data%oldTime_hru%var(:) = oldTime%var(:)


  ! get the number of snow and soil layers
  associate(&
  nSnow => gru_struc(indxGRU)%hruInfo(1)%nSnow, & ! number of snow layers for each HRU
  nSoil => gru_struc(indxGRU)%hruInfo(1)%nSoil  ) ! number of soil layers for each HRU

  ! allocate other data structures
  do iStruct=1,size(structInfo)
  ! allocate space  
  select case(trim(structInfo(iStruct)%structName))    
    case('time'); call allocLocal(time_meta,hru_data%timeStruct,err=err,message=cmessage)     ! model time data
    case('forc'); call allocLocal(forc_meta,hru_data%forcStruct,nSnow,nSoil,err,cmessage);    ! model forcing data
    case('attr'); call allocLocal(attr_meta,hru_data%attrStruct,nSnow,nSoil,err,cmessage);    ! model attribute data
    case('type'); call allocLocal(type_meta,hru_data%typeStruct,nSnow,nSoil,err,cmessage);    ! model type data
    case('id'  ); call allocLocal(id_meta,hru_data%idStruct,nSnow,nSoil,err,cmessage);        ! model id data
    case('mpar'); call allocLocal(mpar_meta,hru_data%mparStruct,nSnow,nSoil,err,cmessage);    ! model parameters  
    case('indx'); call allocLocal(indx_meta,hru_data%indxStruct,nSnow,nSoil,err,cmessage);    ! model variables
    case('prog'); call allocLocal(prog_meta,hru_data%progStruct,nSnow,nSoil,err,cmessage);    ! model prognostic (state) variables
    case('diag'); call allocLocal(diag_meta,hru_data%diagStruct,nSnow,nSoil,err,cmessage);    ! model diagnostic variables
    case('flux'); call allocLocal(flux_meta,hru_data%fluxStruct,nSnow,nSoil,err,cmessage);    ! model fluxes
    case('bpar'); call allocLocal(bpar_meta,hru_data%bparStruct,nSnow=0,nSoil=0,err=err,message=cmessage);  ! basin-average variables
    case('bvar'); call allocLocal(bvar_meta,hru_data%bvarStruct,nSnow=0,nSoil=0,err=err,message=cmessage);  ! basin-average variables
    case('lookup'); cycle ! allocated in t2enthalpy.f90
    case('deriv'); cycle
    case default; err=20; message='unable to find structure name: '//trim(structInfo(iStruct)%structName)
  end select
  ! check errors
  if(err/=0)then
    message=trim(message)//trim(cmessage)//'[structure =  '//trim(structInfo(iStruct)%structName)//']'
    print*, message
    return
  endif
  end do  ! looping through data structures

  ! allocate space for default model parameters
	! NOTE: This is done here, rather than in the loop above, because dpar is not one of the "standard" data structures
	call allocLocal(mpar_meta,hru_data%dparStruct,nSnow,nSoil,err,cmessage);    ! default model parameters
	if(err/=0)then; message=trim(message)//trim(cmessage)//' [problem allocating dparStruct]'; print*,message;return;endif
	 


  ! *****************************************************************************
  ! *** allocate space for output statistics data structures
  ! *****************************************************************************
  ! loop through data structures
  do iStruct=1,size(structInfo)
    ! allocate space
    select case(trim(structInfo(iStruct)%structName))
      case('forc'); call allocLocal(statForc_meta(:)%var_info,hru_data%forcStat,nSnow,nSoil,err,cmessage);    ! model forcing data
      case('prog'); call allocLocal(statProg_meta(:)%var_info,hru_data%progStat,nSnow,nSoil,err,cmessage);    ! model prognostic 
      case('diag'); call allocLocal(statDiag_meta(:)%var_info,hru_data%diagStat,nSnow,nSoil,err,cmessage);    ! model diagnostic
      case('flux'); call allocLocal(statFlux_meta(:)%var_info,hru_data%fluxStat,nSnow,nSoil,err,cmessage);    ! model fluxes
      case('indx'); call allocLocal(statIndx_meta(:)%var_info,hru_data%indxStat,nSnow,nSoil,err,cmessage);    ! index vars
      case('bvar'); call allocLocal(statBvar_meta(:)%var_info,hru_data%bvarStat,nSnow=0,nSoil=0,err=err,message=cmessage);  ! basin-average variables
      case default; cycle
    end select
    ! check errors
    if(err/=0)then
      message=trim(message)//trim(cmessage)//'[statistics for =  '//trim(structInfo(iStruct)%structName)//']'
      print*, message
      return
    endif
  end do ! iStruct


  ! Intilaize the statistics data structures
  allocate(hru_data%statCounter%var(maxVarFreq), stat=err)
  allocate(hru_data%outputTimeStep%var(maxVarFreq), stat=err)
  allocate(hru_data%resetStats%dat(maxVarFreq), stat=err)
  allocate(hru_data%finalizeStats%dat(maxVarFreq), stat=err)
  hru_data%statCounter%var(1:maxVarFreq) = 1
  hru_data%outputTimeStep%var(1:maxVarFreq) = 1
  ! initialize flags to reset/finalize statistics
  hru_data%resetStats%dat(:)    = .true.   ! start by resetting statistics
  hru_data%finalizeStats%dat(:) = .false.  ! do not finalize stats on the first time step
  ! set stats flag for the timestep-level output
  hru_data%finalizeStats%dat(iLookFreq%timestep)=.true.

  ! identify the end of the initialization
  call date_and_time(values=endInit)

  ! end association to info in data structures
  end associate

 end subroutine initHRU

end module INIT_HRU_ACTOR
