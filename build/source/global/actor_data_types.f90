module actor_data_types
  USE, intrinsic :: iso_c_binding
  USE nrtype, integerMissing=>nr_integerMissing
  USE data_types
  implicit none
  private



  ! ** double precision type of for time series
  type, public :: time_dlength
    type(dlength),allocatable          :: tim(:)    ! tim(:)%dat
  endtype time_dlength
  ! ** integer type of for time series
  type, public :: time_ilength
    type(ilength),allocatable          :: tim(:)    ! tim(:)%dat
  endtype time_ilength
  ! ** double prcision type for time series of fixed length
  type, public :: time_d
    real(rkind),allocatable            :: tim(:)    ! tim(:)
  endtype time_d
  ! ** integer type for time series of fixed length
  type, public :: time_i
    integer(i4b),allocatable           :: tim(:)    ! tim(:)
  endtype time_i
  ! ** logical type for time series
  type, public :: time_flagVec
    type(flagVec),allocatable         :: tim(:)    ! tim(:)%dat
  end type time_flagVec

  ! ** double precision type of variable length with storage
  ! for each time_step
  type, public :: var_time_dlength
    type(time_dlength),allocatable     :: var(:)   ! var(:)%tim(:)%dat
  endtype var_time_dlength

  ! ** integer type of variable length with storage
  ! for each time_step
  type, public :: var_time_ilength
    type(time_ilength),allocatable     :: var(:)   ! var(:)%tim(:)%dat
  endtype var_time_ilength

  type, public :: time_i8
    type(var_i8),allocatable           :: tim(:)    ! tim(:)
  endtype time_i8

  type, public :: var_time_d
    type(time_d),allocatable           :: var(:)     ! var(:)%tim
  endtype var_time_d

  type, public :: var_time_i
    type(time_i),allocatable            :: var(:)     ! var(:)%tim
  endtype var_time_i

  type, public :: var_time_i8
    type(time_i8),allocatable           :: var(:)     ! var(:)%tim
  endtype var_time_i8

  ! ***********************************************************************************************************
  ! Type for handling lateral-flows
  ! ***********************************************************************************************************
  type,public :: var_dlength_array
    type(var_dlength), allocatable      :: struc(:) ! struc(:)var(:)%dat
  end type var_dlength_array   

    ! ** double precision type of variable length with timestep storage
  type, public :: hru_time_double
    type(var_time_d),allocatable      :: hru(:)     ! hru(:)%tim(:)%var
  endtype hru_time_double
  ! ** integer type of variable length with timestep storage
  type, public :: hru_time_int
    type(var_time_i),allocatable      :: hru(:)     ! hru(:)%tim(:)%var
  endtype hru_time_int
  ! ** integer type of variable length with timestep storage
  type, public :: hru_time_int8
    type(var_time_i8),allocatable     :: hru(:)     ! hru(:)%tim(:)%var
  endtype hru_time_int8

  ! ** double precission type of timestep variable length
  type, public :: hru_time_doubleVec
    type(var_time_dlength), allocatable :: hru(:)
  endtype hru_time_doubleVec

  type, public :: hru_time_intVec
    type(var_time_ilength), allocatable :: hru(:)
  endtype hru_time_intVec

  type, public :: hru_time_flagVec
    type(time_flagVec),allocatable       :: hru(:)  ! hru(:)%tim(:)%dat          
  endtype hru_time_flagVec

  type,public :: gru_hru_time_flagVec
    type(hru_time_flagVec),allocatable :: gru(:)  ! gru(:)%hru(:)%tim(:)%dat(:)
  endtype gru_hru_time_flagVec           

  type, public :: gru_hru_time_double
    type(hru_time_double),allocatable :: gru(:)
  endtype gru_hru_time_double

  type, public :: gru_hru_time_int
    type(hru_time_int), allocatable   :: gru(:)
  endtype gru_hru_time_int

  type, public :: gru_hru_time_int8
    type(hru_time_int8), allocatable  :: gru(:)  
  endtype gru_hru_time_int8 

  type, public :: gru_hru_time_doubleVec
    type(hru_time_doubleVec),allocatable :: gru(:)
  endtype gru_hru_time_doubleVec

  type, public :: gru_hru_time_intVec
    type(hru_time_intVec),allocatable    :: gru(:)
  endtype gru_hru_time_intVec

  type, public :: hru_type
#ifdef V4_ACTIVE
    type(zLookup),pointer                      :: lookupStruct               ! z(:)%var(:)%lookup(:) -- lookup tables
#endif
    type(var_dlength),pointer                  :: forcStat                   ! model forcing data
    type(var_dlength),pointer                  :: progStat                   ! model prognostic (state) variables
    type(var_dlength),pointer                  :: diagStat                   ! model diagnostic variables
    type(var_dlength),pointer                  :: fluxStat                   ! model fluxes
    type(var_dlength),pointer                  :: indxStat                   ! model indices
    type(var_dlength),pointer                  :: bvarStat                   ! basin-average variabl
    ! primary data structures (scalars)
    type(var_i),pointer                        :: timeStruct                 ! model time data
    type(var_d),pointer                        :: forcStruct                 ! model forcing data
    type(var_d),pointer                        :: attrStruct                 ! model attribute data
    type(var_i),pointer                        :: typeStruct                 ! model type data
    type(var_i8),pointer                       :: idStruct                   ! model id data
    ! primary data structures (variable length vectors)
    type(var_ilength),pointer                  :: indxStruct                 ! model indices
    type(var_dlength),pointer                  :: mparStruct                 ! model parameters
    type(var_dlength),pointer                  :: progStruct                 ! model prognostic (state) variables
    type(var_dlength),pointer                  :: diagStruct                 ! model diagnostic variables
    type(var_dlength),pointer                  :: fluxStruct                 ! model fluxes
    ! basin-average structures
    type(var_d),pointer                        :: bparStruct                 ! basin-average variables
    type(var_dlength),pointer                  :: bvarStruct                 ! basin-average variables
    type(var_d),pointer                        :: dparStruct                 ! default model parameters
    ! local HRU data structures
    type(var_i),pointer                        :: startTime_hru              ! start time for the model simulation
    type(var_i),pointer                        :: finishTime_hru             ! end time for the model simulation
    type(var_i),pointer                        :: refTime_hru                ! reference time for the model simulation
    type(var_i),pointer                        :: oldTime_hru                ! time from previous step

    ! Statistic flags 
    type(var_i), pointer                       :: statCounter                ! time counter for stats
    type(var_i), pointer                       :: outputTimeStep             ! timestep in output files
    type(flagVec), pointer                     :: resetStats                 ! flags to finalize statistics
    type(flagVec), pointer                     :: finalizeStats              ! flags to finalize statistics

    ! Julian Day Variables
    real(c_double)                             :: fracJulDay                 ! fractional julian days since the start of year
    real(c_double)                             :: tmZoneOffsetFracDay        ! time zone offset in fractional days
    integer(c_int)                             :: yearLength                 ! number of days in the current year
    ! Misc Variables
    integer(c_int)                             :: computeVegFlux             ! flag to indicate if we are computing fluxes over vegetation
    real(c_double)                             :: dt_init
    real(c_double)                             :: upArea
  end type hru_type

  type, public :: gru_type
    type(hru_type),allocatable :: hru(:)
    type(var_dlength),pointer  :: bvarStat
    type(var_dlength),pointer  :: bvarStruct
  end type gru_type
end module
