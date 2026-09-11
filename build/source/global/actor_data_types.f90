module actor_data_types
  USE, intrinsic :: iso_c_binding
  USE nr_type, integerMissing=>nr_integerMissing
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
    type(var_time_d),allocatable         :: hru(:)     ! hru(:)%tim(:)%var
  endtype hru_time_double
  ! ** integer type of variable length with timestep storage
  type, public :: hru_time_int
    type(var_time_i),allocatable         :: hru(:)     ! hru(:)%tim(:)%var
  endtype hru_time_int
  ! ** integer type of variable length with timestep storage
  type, public :: hru_time_int8
    type(var_time_i8),allocatable        :: hru(:)     ! hru(:)%tim(:)%var
  endtype hru_time_int8

  ! ** double precission type of timestep variable length
  type, public :: hru_time_doubleVec
    type(var_time_dlength), allocatable  :: hru(:)
  endtype hru_time_doubleVec

  type, public :: hru_time_intVec
    type(var_time_ilength), allocatable  :: hru(:)
  endtype hru_time_intVec

  type, public :: hru_time_flagVec
    type(time_flagVec),allocatable       :: hru(:)  ! hru(:)%tim(:)%dat          
  endtype hru_time_flagVec

  type,public :: gru_hru_time_flagVec
    type(hru_time_flagVec),allocatable   :: gru(:)  ! gru(:)%hru(:)%tim(:)%dat(:)
  endtype gru_hru_time_flagVec             

  type, public :: gru_hru_time_double
    type(hru_time_double),allocatable    :: gru(:)
  endtype gru_hru_time_double

  type, public :: gru_hru_time_int
    type(hru_time_int), allocatable      :: gru(:)
  endtype gru_hru_time_int

  type, public :: gru_hru_time_int8
    type(hru_time_int8), allocatable     :: gru(:)  
  endtype gru_hru_time_int8 

  type, public :: gru_hru_time_doubleVec
    type(hru_time_doubleVec),allocatable :: gru(:)
  endtype gru_hru_time_doubleVec

  type, public :: gru_hru_time_intVec
    type(hru_time_intVec),allocatable    :: gru(:)
  endtype gru_hru_time_intVec

  ! ***********************************************************************************************************
  ! Output-buffer wrappers that carry BOTH a domain (dom) and a buffer-timestep (tim) dimension
  ! ***********************************************************************************************************
  type, public :: dom_time_doubleVec
    type(var_time_dlength),allocatable    :: dom(:)   ! dom(:)%var(:)%tim(:)%dat
  endtype dom_time_doubleVec
  type, public :: dom_time_intVec
    type(var_time_ilength),allocatable    :: dom(:)   ! dom(:)%var(:)%tim(:)%dat
  endtype dom_time_intVec
  type, public :: hru_dom_time_doubleVec
    type(dom_time_doubleVec),allocatable  :: hru(:)   ! hru(:)%dom(:)%var(:)%tim(:)%dat
  endtype hru_dom_time_doubleVec
  type, public :: hru_dom_time_intVec
    type(dom_time_intVec),allocatable     :: hru(:)   ! hru(:)%dom(:)%var(:)%tim(:)%dat
  endtype hru_dom_time_intVec
  type, public :: gru_hru_dom_time_doubleVec
    type(hru_dom_time_doubleVec),allocatable :: gru(:) ! gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat
  endtype gru_hru_dom_time_doubleVec
  type, public :: gru_hru_dom_time_intVec
    type(hru_dom_time_intVec),allocatable :: gru(:)   ! gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat
  endtype gru_hru_dom_time_intVec

  type, public :: hru_type
    type(dom_z_vLookup),pointer                :: lookupStruct               ! dom(:)%z(:)%var(:)%lookup(:) -- lookup tables
    type(var_dlength),pointer                  :: forcStat                   ! model forcing data (HRU level)
    type(dom_doubleVec),pointer                :: progStat                   ! model prognostic (state) variables
    type(dom_doubleVec),pointer                :: diagStat                   ! model diagnostic variables
    type(dom_doubleVec),pointer                :: fluxStat                   ! model fluxes
    type(dom_doubleVec),pointer                :: indxStat                   ! model indices
    type(var_dlength),pointer                  :: bvarStat                   ! basin-average variabl
    ! primary data structures (scalars)
    type(var_i),pointer                        :: timeStruct                 ! model time data
    type(var_d),pointer                        :: forcStruct                 ! model forcing data (HRU level)
    type(var_d),pointer                        :: attrStruct                 ! model attribute data
    type(var_i),pointer                        :: typeStruct                 ! model type data
    type(var_i8),pointer                       :: idStruct                   ! model id data
    ! primary data structures (variable length vectors)
    type(dom_intVec),pointer                   :: indxStruct                 ! model indices          (per domain)
    type(dom_doubleVec),pointer                :: mparStruct                 ! model parameters       (per domain)
    type(dom_doubleVec),pointer                :: progStruct                 ! model prognostic vars  (per domain)
    type(dom_doubleVec),pointer                :: diagStruct                 ! model diagnostic vars  (per domain)
    type(dom_doubleVec),pointer                :: fluxStruct                 ! model fluxes           (per domain)
    ! basin-average structures
    type(var_d),pointer                        :: bparStruct                 ! basin-average variables
    type(var_dlength),pointer                  :: bvarStruct                 ! basin-average variables
    type(var_d),pointer                        :: dparStruct                 ! default model parameters (HRU level)
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
    type(dom_d)                                :: dt_init                    ! initial sub-step length for each domain -- dt_init%dom(:)
    real(c_double)                             :: upArea
  end type hru_type

  type, public :: gru_type
    type(hru_type),allocatable :: hru(:)
    type(var_dlength),pointer  :: bvarStat
    type(var_dlength),pointer  :: bvarStruct
    type(grid_double),pointer  :: gridStruct                  ! grid(:)%var(:)%dat2(:,:) -- basin glacier grids (may be size 0)
  end type gru_type

  ! Output Structure Type
  type, public :: summa_output_type
    type(gru_hru_dom_z_vLookup)                       :: lookupStruct                   ! x%gru(:)%hru(:)%dom(:)%z(:)%var(:)%lookup(:) -- lookup tables
    ! define the statistics structures
    type(gru_hru_time_doubleVec)                      :: forcStat                      ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- model forcing data (HRU level)
    type(gru_hru_dom_time_doubleVec)                  :: progStat                      ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat -- model prognostic (state) variables
    type(gru_hru_dom_time_doubleVec)                  :: diagStat                      ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat -- model diagnostic variables
    type(gru_hru_dom_time_doubleVec)                  :: fluxStat                      ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat -- model fluxes
    type(gru_hru_dom_time_doubleVec)                  :: indxStat                      ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat -- model indices
    type(gru_hru_time_doubleVec)                      :: bvarStat                      ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- basin-average variabl

    ! define the primary data structures (scalars)
    type(gru_hru_time_int)                            :: timeStruct                    ! x%gru(:)%hru(:)%var(:)%tim(:)     -- model time data
    type(gru_hru_time_double)                         :: forcStruct                    ! x%gru(:)%hru(:)%var(:)%tim(:)     -- model forcing data (HRU level)
    type(gru_hru_double)                              :: attrStruct                    ! x%gru(:)%hru(:)%var(:)            -- local attributes for each HRU, DOES NOT CHANGE OVER TIMESTEPS
    type(gru_hru_int)                                 :: typeStruct                    ! x%gru(:)%hru(:)%var(:)            -- local classification of soil veg etc. for each HRU, DOES NOT CHANGE OVER TIMESTEPS
    type(gru_hru_int8)                                :: idStruct                      ! x%gru(:)%hru(:)%var(:)

    ! define the primary data structures (variable length vectors)
    type(gru_hru_dom_time_intVec)                     :: indxStruct                    ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat -- model indices
    type(gru_hru_dom_doubleVec)                       :: mparStruct                    ! x%gru(:)%hru(:)%dom(:)%var(:)%dat        -- model parameters, DOES NOT CHANGE OVER TIMESTEPS
    type(gru_hru_dom_time_doubleVec)                  :: progStruct                    ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat -- model prognostic (state) variables
    type(gru_hru_dom_time_doubleVec)                  :: diagStruct                    ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat -- model diagnostic variables
    type(gru_hru_dom_time_doubleVec)                  :: fluxStruct                    ! x%gru(:)%hru(:)%dom(:)%var(:)%tim(:)%dat -- model fluxes

    ! define the basin-average structures
    type(gru_double)                                  :: bparStruct                    ! x%gru(:)%var(:)                   -- basin-average parameters, DOES NOT CHANGE OVER TIMESTEPS
    type(gru_hru_time_doubleVec)                      :: bvarStruct                    ! x%gru(:)%hru(:)%var(:)%tim(:)%dat -- basin-average variables
    ! grid (glacier) structures
    type(gru_grid_double)                             :: gridStruct                    ! x%gru(:)%grid(:)%var(:)%dat2(:,:) -- basin glacier grids
    ! define the ancillary data structures
    type(gru_hru_double)                              :: dparStruct                    ! x%gru(:)%hru(:)%dom(:)%var(:)

    ! finalize stats structure
    type(gru_hru_time_flagVec)                        :: finalizeStats                 ! x%gru(:)%hru(:)%tim(:)%dat -- flags on when to write to file

    type(gru_d)                                       :: upArea
    integer(i4b)                                      :: nTimeSteps
    integer(i4b)                                      :: nDOM                           ! maximum number of domains in any HRU (== maxDOM)
    logical(lgt), allocatable                         :: failedGrus(:)                  ! flag to indicate if the GRU failed
  end type summa_output_type
end module
