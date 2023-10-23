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
#ifdef V4_ACTIVE
USE globalData,only:lookup_meta 
#endif
! statistics metadata structures
USE globalData,only:statForc_meta                           ! child metadata for stats
USE globalData,only:statProg_meta                           ! child metadata for stats
USE globalData,only:statDiag_meta                           ! child metadata for stats
USE globalData,only:statFlux_meta                           ! child metadata for stats
USE globalData,only:statIndx_meta                           ! child metadata for stats
USE globalData,only:statBvar_meta                           ! child metadata for stats
! maxvarFreq 
USE var_lookup,only:maxVarFreq                               ! # of available output frequencies
! named variables
USE var_lookup,only:iLookATTR                               ! look-up values for local attributes
USE var_lookup,only:iLookTYPE                               ! look-up values for classification of veg, soils etc.
USE var_lookup,only:iLookPARAM                              ! look-up values for local column model parameters
USE var_lookup,only:iLookID                                 ! look-up values for local column model parameters

USE var_lookup,only:iLookPROG                               ! look-up values for local column model prognostic (state) variables
USE var_lookup,only:iLookDIAG                               ! look-up values for local column model diagnostic variables
USE var_lookup,only:iLookFLUX                               ! look-up values for local column model fluxes
USE var_lookup,only:iLookBVAR                               ! look-up values for basin-average model variables
USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions
USE globalData,only:urbanVegCategory                        ! vegetation category for urban areas

! named variables to define LAI decisions
USE mDecisions_module,only:&
 monthlyTable,& ! LAI/SAI taken directly from a monthly table for different vegetation classes
 specified      ! LAI/SAI computed from green vegetation fraction and winterSAI and summerLAI parameters

! safety: set private unless specified otherwise
implicit none
private
public::initHRU
public::setupHRUParam
public::SOIL_VEG_GEN_PARM
public::summa_readRestart
public::setIDATolerances
contains
! **************************************************************************************************
! public subroutine initHRU: ! used to declare and allocate summa data structures and initialize model state to known values
! **************************************************************************************************
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



! **************************************************************************************************
! public subroutine setupHRUParam: initializes parameter data structures (e.g. vegetation and soil parameters).
! **************************************************************************************************
subroutine setupHRUParam(indxGRU,                 & ! ID of hru
                         indxHRU,                 & ! Index of the parent GRU of the HRU 
                         handle_hru_data,         &
                         upArea,                  & ! area upslope of each HRU,
                         err) bind(C, name='setupHRUParam')
  ! ---------------------------------------------------------------------------------------
  ! * desired modules
  ! ---------------------------------------------------------------------------------------
  USE nrtype                                                  ! variable types, etc.
  USE output_structure_module,only:outputStructure
  ! subroutines and functions
  use time_utils_module,only:elapsedSec                       ! calculate the elapsed time
  USE mDecisions_module,only:mDecisions                       ! module to read model decisions
  USE paramCheck_module,only:paramCheck                       ! module to check consistency of model parameters
  USE pOverwrite_module,only:pOverwrite                       ! module to overwrite default parameter values with info from the Noah tables
  USE ConvE2Temp_module,only:E2T_lookup                       ! module to calculate a look-up table for the temperature-enthalpy conversion
#ifdef V4_ACTIVE  
  USE t2enthalpy_module,only:T2E_lookup                       ! module to calculate a look-up table for the temperature-enthalpy conversion
#endif
  USE var_derive_module,only:fracFuture                       ! module to calculate the fraction of runoff in future time steps (time delay histogram)
  USE module_sf_noahmplsm,only:read_mp_veg_parameters         ! module to read NOAH vegetation tables
  ! global data structures
  USE globalData,only:gru_struc                               ! gru-hru mapping structures
  USE globalData,only:localParFallback                        ! local column default parameters
  USE globalData,only:model_decisions                         ! model decision structure
  USE globalData,only:greenVegFrac_monthly                    ! fraction of green vegetation in each month (0-1)
  ! output constraints
  USE globalData,only:maxLayers                               ! maximum number of layers
  USE globalData,only:maxSnowLayers                           ! maximum number of snow layers
  ! timing variables
  USE globalData,only:startSetup,endSetup                     ! date/time for the start and end of the parameter setup
  USE globalData,only:elapsedSetup                            ! elapsed time for the parameter setup
  ! Noah-MP parameters
  USE NOAHMP_VEG_PARAMETERS,only:SAIM,LAIM                    ! 2-d tables for stem area index and leaf area index (vegType,month)
  USE NOAHMP_VEG_PARAMETERS,only:HVT,HVB                      ! height at the top and bottom of vegetation (vegType)

  ! ---------------------------------------------------------------------------------------
  ! * variables
  ! ---------------------------------------------------------------------------------------
  implicit none
  ! dummy variables
  ! calling variables
  integer(c_int),intent(in)                :: indxGRU              ! Index of the parent GRU of the HRU
  integer(c_int),intent(in)                :: indxHRU              ! ID to locate correct HRU from netcdf file 
  type(c_ptr), intent(in), value           :: handle_hru_data      ! pointer to the hru data structure (for error messages
  real(c_double),intent(inout)             :: upArea
  integer(c_int),intent(inout)             :: err

  ! local variables
  type(hru_type),pointer                   :: hru_data             ! local hru data structure

  integer(i4b)                             :: ivar                 ! loop counter
  integer(i4b)                             :: i_z                  ! loop counter
  character(len=256)                       :: message              ! error message
  character(len=256)                       :: cmessage             ! error message of downwind routine

  ! ---------------------------------------------------------------------------------------
  ! initialize error control
  err=0; message='setupHRUParam/'

  ! convert to fortran pointer from C++ pointer
  call c_f_pointer(handle_hru_data, hru_data)

  ! ffile_info and mDecisions moved to their own seperate subroutine call

  hru_data%oldTime_hru%var(:) = hru_data%startTime_hru%var(:)

  ! Copy the attrStruct
  hru_data%attrStruct%var(:) = outputStructure(1)%attrStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  ! Copy the typeStruct
  hru_data%typeStruct%var(:) = outputStructure(1)%typeStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  ! Copy the idStruct
  hru_data%idStruct%var(:) = outputStructure(1)%idStruct%gru(indxGRU)%hru(indxHRU)%var(:)

  ! Copy the mparStruct
  hru_data%mparStruct%var(:) = outputStructure(1)%mparStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  ! Copy the bparStruct
  hru_data%bparStruct%var(:) = outputStructure(1)%bparStruct%gru(indxGRU)%var(:)
  ! Copy the dparStruct
  hru_data%dparStruct%var(:) = outputStructure(1)%dparStruct%gru(indxGRU)%hru(indxHRU)%var(:)
  ! Copy the bvarStruct
  do ivar=1, size(outputStructure(1)%bvarStruct_init%gru(indxGRU)%var(:))
    hru_data%bvarStruct%var(ivar)%dat(:) = outputStructure(1)%bvarStruct_init%gru(indxGRU)%var(ivar)%dat(:)
  enddo
  ! Copy the lookup Struct if its allocated
#ifdef V4_ACTIVE
  if (allocated(outputStructure(1)%lookupStruct%gru(indxGRU)%hru(indxHRU)%z)) then
    do i_z=1, size(outputStructure(1)%lookupStruct%gru(indxGRU)%hru(indxHRU)%z(:))
      do iVar=1, size(outputStructure(1)%lookupStruct%gru(indxGRU)%hru(indxHRU)%z(i_z)%var(:))
        hru_data%lookupStruct%z(i_z)%var(ivar)%lookup(:) = outputStructure(1)%lookupStruct%gru(indxGRU)%hru(indxHRU)%z(i_z)%var(iVar)%lookup(:)
      end do
    end do
  endif
#endif
  ! Copy the progStruct_init
  do ivar=1, size(outputStructure(1)%progStruct_init%gru(indxGRU)%hru(indxHRU)%var(:))
    hru_data%progStruct%var(ivar)%dat(:) = outputStructure(1)%progStruct_init%gru(indxGRU)%hru(indxHRU)%var(ivar)%dat(:)
  enddo
  ! copy the indexStruct_init
  do ivar=1, size(outputStructure(1)%indxStruct_init%gru(indxGRU)%hru(indxHRU)%var(:))
    hru_data%indxStruct%var(ivar)%dat(:) = outputStructure(1)%indxStruct_init%gru(indxGRU)%hru(indxHRU)%var(ivar)%dat(:)
  enddo
end subroutine setupHRUParam



! **************************************************************************************************
! private subroutine SOIL_VEG_GEN_PARM: Read soil, vegetation and other model parameters (from NOAH)
! **************************************************************************************************
SUBROUTINE SOIL_VEG_GEN_PARM(FILENAME_VEGTABLE, FILENAME_SOILTABLE, FILENAME_GENERAL, MMINLU, MMINSL)
  !-----------------------------------------------------------------
  use module_sf_noahlsm, only : shdtbl, nrotbl, rstbl, rgltbl, &
  &                        hstbl, snuptbl, maxalb, laimintbl, &
  &                        bb, drysmc, f11, maxsmc, laimaxtbl, &
  &                        emissmintbl, emissmaxtbl, albedomintbl, &
  &                        albedomaxtbl, wltsmc, qtz, refsmc, &
  &                        z0mintbl, z0maxtbl, &
  &                        satpsi, satdk, satdw, &
  &                        theta_res, theta_sat, vGn_alpha, vGn_n, k_soil, &  ! MPC add van Genutchen parameters
  &                        fxexp_data, lvcoef_data, &
  &                        lutype, maxalb, &
  &                        slope_data, frzk_data, bare, cmcmax_data, &
  &                        cfactr_data, csoil_data, czil_data, &
  &                        refkdt_data, natural, refdk_data, &
  &                        rsmax_data, salp_data, sbeta_data, &
  &                        zbot_data, smhigh_data, smlow_data, &
  &                        lucats, topt_data, slcats, slpcats, sltype

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: FILENAME_VEGTABLE, FILENAME_SOILTABLE, FILENAME_GENERAL
  CHARACTER(LEN=*), INTENT(IN) :: MMINLU, MMINSL
  integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
  integer :: ierr
  INTEGER , PARAMETER :: OPEN_OK = 0

  character*128 :: mess , message

  !-----SPECIFY VEGETATION RELATED CHARACTERISTICS :
  !             ALBBCK: SFC albedo (in percentage)
  !                 Z0: Roughness length (m)
  !             SHDFAC: Green vegetation fraction (in percentage)
  !  Note: The ALBEDO, Z0, and SHDFAC values read from the following table
  !          ALBEDO, amd Z0 are specified in LAND-USE TABLE; and SHDFAC is
  !          the monthly green vegetation data
  !             CMXTBL: MAX CNPY Capacity (m)
  !             NROTBL: Rooting depth (layer)
  !              RSMIN: Mimimum stomatal resistance (s m-1)
  !              RSMAX: Max. stomatal resistance (s m-1)
  !                RGL: Parameters used in radiation stress function
  !                 HS: Parameter used in vapor pressure deficit functio
  !               TOPT: Optimum transpiration air temperature. (K)
  !             CMCMAX: Maximum canopy water capacity
  !             CFACTR: Parameter used in the canopy inteception calculati
  !               SNUP: Threshold snow depth (in water equivalent m) that
  !                     implies 100% snow cover
  !                LAI: Leaf area index (dimensionless)
  !             MAXALB: Upper bound on maximum albedo over deep snow
  !
  !-----READ IN VEGETAION PROPERTIES FROM VEGPARM.TBL
  !

  OPEN(19, FILE=trim(FILENAME_VEGTABLE),FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
  WRITE(message,FMT='(A)') &
  'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
  CALL wrf_error_fatal ( message )
  END IF

  LUMATCH=0

  FIND_LUTYPE : DO WHILE (LUMATCH == 0)
  READ (19,*,END=2002)
  READ (19,*,END=2002)LUTYPE
  READ (19,*)LUCATS,IINDEX

  IF(LUTYPE.EQ.MMINLU)THEN
  WRITE( mess , * ) 'LANDUSE TYPE = ' // TRIM ( LUTYPE ) // ' FOUND', LUCATS,' CATEGORIES'
  ! CALL wrf_message( mess )
  LUMATCH=1
  ELSE
  ! call wrf_message ( "Skipping over LUTYPE = " // TRIM ( LUTYPE ) )
  DO LC = 1, LUCATS+12
  read(19,*)
  ENDDO
  ENDIF
  ENDDO FIND_LUTYPE
  ! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(SHDTBL)       < LUCATS .OR. &
  SIZE(NROTBL)       < LUCATS .OR. &
  SIZE(RSTBL)        < LUCATS .OR. &
  SIZE(RGLTBL)       < LUCATS .OR. &
  SIZE(HSTBL)        < LUCATS .OR. &
  SIZE(SNUPTBL)      < LUCATS .OR. &
  SIZE(MAXALB)       < LUCATS .OR. &
  SIZE(LAIMINTBL)    < LUCATS .OR. &
  SIZE(LAIMAXTBL)    < LUCATS .OR. &
  SIZE(Z0MINTBL)     < LUCATS .OR. &
  SIZE(Z0MAXTBL)     < LUCATS .OR. &
  SIZE(ALBEDOMINTBL) < LUCATS .OR. &
  SIZE(ALBEDOMAXTBL) < LUCATS .OR. &
  SIZE(EMISSMINTBL ) < LUCATS .OR. &
  SIZE(EMISSMAXTBL ) < LUCATS ) THEN
  CALL wrf_error_fatal('Table sizes too small for value of LUCATS in module_sf_noahdrv.F')
  ENDIF

  IF(LUTYPE.EQ.MMINLU)THEN
  DO LC=1,LUCATS
  READ (19,*)IINDEX,SHDTBL(LC),                        &
  NROTBL(LC),RSTBL(LC),RGLTBL(LC),HSTBL(LC), &
  SNUPTBL(LC),MAXALB(LC), LAIMINTBL(LC),     &
  LAIMAXTBL(LC),EMISSMINTBL(LC),             &
  EMISSMAXTBL(LC), ALBEDOMINTBL(LC),         &
  ALBEDOMAXTBL(LC), Z0MINTBL(LC), Z0MAXTBL(LC)
  ENDDO

  READ (19,*)
  READ (19,*)TOPT_DATA
  READ (19,*)
  READ (19,*)CMCMAX_DATA
  READ (19,*)
  READ (19,*)CFACTR_DATA
  READ (19,*)
  READ (19,*)RSMAX_DATA
  READ (19,*)
  READ (19,*)BARE
  READ (19,*)
  READ (19,*)NATURAL
  ENDIF

  2002 CONTINUE

  CLOSE (19)
  IF (LUMATCH == 0) then
  CALL wrf_error_fatal ("Land Use Dataset '"//MMINLU//"' not found in VEGPARM.TBL.")
  ENDIF

  !
  !-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
  !
  OPEN(19, FILE=trim(FILENAME_SOILTABLE),FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
  WRITE(message,FMT='(A)') &
  'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
  CALL wrf_error_fatal ( message )
  END IF

  WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICATION = ', TRIM ( MMINSL )
  ! CALL wrf_message( mess )

  LUMATCH=0

  ! MPC add a new soil table
  FIND_soilTYPE : DO WHILE (LUMATCH == 0)
  READ (19,*)
  READ (19,*,END=2003)SLTYPE
  READ (19,*)SLCATS,IINDEX
  IF(SLTYPE.EQ.MMINSL)THEN
  WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ', TRIM ( SLTYPE ) , ' FOUND', &
  SLCATS,' CATEGORIES'
  ! CALL wrf_message ( mess )
  LUMATCH=1
  ELSE
  ! call wrf_message ( "Skipping over SLTYPE = " // TRIM ( SLTYPE ) )
  DO LC = 1, SLCATS
  read(19,*)
  ENDDO
  ENDIF
  ENDDO FIND_soilTYPE
  ! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(BB    ) < SLCATS .OR. &
  SIZE(DRYSMC) < SLCATS .OR. &
  SIZE(F11   ) < SLCATS .OR. &
  SIZE(MAXSMC) < SLCATS .OR. &
  SIZE(REFSMC) < SLCATS .OR. &
  SIZE(SATPSI) < SLCATS .OR. &
  SIZE(SATDK ) < SLCATS .OR. &
  SIZE(SATDW ) < SLCATS .OR. &
  SIZE(WLTSMC) < SLCATS .OR. &
  SIZE(QTZ   ) < SLCATS  ) THEN
  CALL wrf_error_fatal('Table sizes too small for value of SLCATS in module_sf_noahdrv.F')
  ENDIF

  ! MPC add new soil table
  select case(trim(SLTYPE))
  case('STAS','STAS-RUC')  ! original soil tables
  DO LC=1,SLCATS
  READ (19,*) IINDEX,BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
  REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
  WLTSMC(LC), QTZ(LC)
  ENDDO
  case('ROSETTA')          ! new soil table
  DO LC=1,SLCATS
  READ (19,*) IINDEX,&
  ! new soil parameters (from Rosetta)
  theta_res(LC), theta_sat(LC),        &
  vGn_alpha(LC), vGn_n(LC), k_soil(LC), &
  ! original soil parameters
  BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
  REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
  WLTSMC(LC), QTZ(LC)
  ENDDO
  case default
  CALL wrf_message( 'SOIL TEXTURE IN INPUT FILE DOES NOT ' )
  CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
  CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
  end select

  2003 CONTINUE

  CLOSE (19)

  IF(LUMATCH.EQ.0)THEN
  CALL wrf_message( 'SOIL TEXTURE IN INPUT FILE DOES NOT ' )
  CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
  CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
  ENDIF

  !
  !-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
  !
  OPEN(19, FILE=trim(FILENAME_GENERAL),FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
  IF(ierr .NE. OPEN_OK ) THEN
  WRITE(message,FMT='(A)') &
  'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
  CALL wrf_error_fatal ( message )
  END IF

  READ (19,*)
  READ (19,*)
  READ (19,*) NUM_SLOPE

  SLPCATS=NUM_SLOPE
  ! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
  IF ( SIZE(slope_data) < NUM_SLOPE ) THEN
  CALL wrf_error_fatal('NUM_SLOPE too large for slope_data array in module_sf_noahdrv')
  ENDIF

  DO LC=1,SLPCATS
  READ (19,*)SLOPE_DATA(LC)
  ENDDO

  READ (19,*)
  READ (19,*)SBETA_DATA
  READ (19,*)
  READ (19,*)FXEXP_DATA
  READ (19,*)
  READ (19,*)CSOIL_DATA
  READ (19,*)
  READ (19,*)SALP_DATA
  READ (19,*)
  READ (19,*)REFDK_DATA
  READ (19,*)
  READ (19,*)REFKDT_DATA
  READ (19,*)
  READ (19,*)FRZK_DATA
  READ (19,*)
  READ (19,*)ZBOT_DATA
  READ (19,*)
  READ (19,*)CZIL_DATA
  READ (19,*)
  READ (19,*)SMLOW_DATA
  READ (19,*)
  READ (19,*)SMHIGH_DATA
  READ (19,*)
  READ (19,*)LVCOEF_DATA
  CLOSE (19)

END SUBROUTINE SOIL_VEG_GEN_PARM



! **************************************************************************************************
! public subroutine summa_readRestart: read restart data and reset the model state
! **************************************************************************************************
subroutine summa_readRestart(indxGRU,         & ! index of GRU in gru_struc
                            indxHRU,          & ! index of HRU in gru_struc
                            handle_hru_data,  & ! data structure for the HRU
                            dt_init,          & ! used to initialize the length of the sub-step for each HRU
                            err) bind(C,name='summa_readRestart')
  ! ---------------------------------------------------------------------------------------
  ! * desired modules
  ! ---------------------------------------------------------------------------------------
  ! data types
  USE nrtype                                                  ! variable types, etc.
  ! functions and subroutines
  USE time_utils_module,only:elapsedSec                       ! calculate the elapsed time
  ! USE read_icond_module,only:read_icond               ! module to read initial conditions
  ! USE check_icond4chm_module,only:check_icond4chm             ! module to check initial conditions
  USE var_derive_module,only:calcHeight                       ! module to calculate height at layer interfaces and layer mid-point
  USE var_derive_module,only:v_shortcut                       ! module to calculate "short-cut" variables
  USE var_derive_module,only:rootDensty                       ! module to calculate the vertical distribution of roots
  USE var_derive_module,only:satHydCond                       ! module to calculate the saturated hydraulic conductivity in each soil layer
  ! global data structures
  USE globalData,only:model_decisions                         ! model decision structure
  ! timing variables
  USE globalData,only:startRestart,endRestart                 ! date/time for the start and end of reading model restart files
  USE globalData,only:elapsedRestart                          ! elapsed time to read model restart files
  ! model decisions
  USE mDecisions_module,only:&                                ! look-up values for the choice of method for the spatial representation of groundwater
  localColumn, & ! separate groundwater representation in each local soil column
  singleBasin    ! single groundwater store over the entire basin
  implicit none
  ! ---------------------------------------------------------------------------------------
  ! Dummy variables
  ! ---------------------------------------------------------------------------------------
  integer(c_int),intent(in)               :: indxGRU            !  index of GRU in gru_struc
  integer(c_int),intent(in)               :: indxHRU            !  index of HRU in gru_struc
  type(c_ptr), intent(in), value          :: handle_hru_data   !  data structure for the HRU

  real(c_double), intent(inout)           :: dt_init
  integer(c_int), intent(inout)           :: err
  ! ---------------------------------------------------------------------------------------
  ! Fortran Pointers
  ! ---------------------------------------------------------------------------------------
  type(hru_type),pointer                 :: hru_data
  ! ---------------------------------------------------------------------------------------
  ! local variables
  ! ---------------------------------------------------------------------------------------
  character(len=256)                     :: message            ! error message
  character(LEN=256)                     :: cmessage           ! error message of downwind routine
  character(LEN=256)                     :: restartFile        ! restart file name
  integer(i4b)                           :: nGRU
  ! ---------------------------------------------------------------------------------------

  call c_f_pointer(handle_hru_data, hru_data)

  ! initialize error control
  err=0; message='hru_actor_readRestart/'


  ! *****************************************************************************
  ! *** compute ancillary variables
  ! *****************************************************************************

  ! re-calculate height of each layer
  call calcHeight(hru_data%indxStruct,   & ! layer type
      hru_data%progStruct,   & ! model prognostic (state) variables for a local HRU
      err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! calculate vertical distribution of root density
  call rootDensty(hru_data%mparStruct,   & ! vector of model parameters
      hru_data%indxStruct,   & ! data structure of model indices
      hru_data%progStruct,   & ! data structure of model prognostic (state) variables
      hru_data%diagStruct,   & ! data structure of model diagnostic variables
      err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! calculate saturated hydraulic conductivity in each soil layer
  call satHydCond(hru_data%mparStruct,   & ! vector of model parameters
      hru_data%indxStruct,   & ! data structure of model indices
      hru_data%progStruct,   & ! data structure of model prognostic (state) variables
      hru_data%fluxStruct,   & ! data structure of model fluxes
      err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! calculate "short-cut" variables such as volumetric heat capacity
  call v_shortcut(hru_data%mparStruct,   & ! vector of model parameters
      hru_data%diagStruct,   & ! data structure of model diagnostic variables
      err,cmessage)                       ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! initialize canopy drip
  ! NOTE: canopy drip from the previous time step is used to compute throughfall for the current time step
  hru_data%fluxStruct%var(iLookFLUX%scalarCanopyLiqDrainage)%dat(1) = 0._dp  ! not used

  ! *****************************************************************************
  ! *** initialize aquifer storage
  ! *****************************************************************************

  ! initialize aquifer storage
  ! NOTE: this is ugly: need to add capabilities to initialize basin-wide state variables

  ! There are two options for groundwater:
  !  (1) where groundwater is included in the local column (i.e., the HRUs); and
  !  (2) where groundwater is included for the single basin (i.e., the GRUS, where multiple HRUS drain into a GRU).

  ! For water balance calculations it is important to ensure that the local aquifer storage is zero if groundwater is treated as a basin-average state variable (singleBasin);
  !  and ensure that basin-average aquifer storage is zero when groundwater is included in the local columns (localColumn).

  ! select groundwater option
  select case(model_decisions(iLookDECISIONS%spatial_gw)%iDecision)

  ! the basin-average aquifer storage is not used if the groundwater is included in the local column
  case(localColumn)
  hru_data%bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = 0._dp ! set to zero to be clear that there is no basin-average aquifer storage in this configuration

  ! the local column aquifer storage is not used if the groundwater is basin-average
  ! (i.e., where multiple HRUs drain to a basin-average aquifer)
  case(singleBasin)
  hru_data%bvarStruct%var(iLookBVAR%basin__AquiferStorage)%dat(1) = 1._dp
  hru_data%progStruct%var(iLookPROG%scalarAquiferStorage)%dat(1) = 0._dp  ! set to zero to be clear that there is no local aquifer storage in this configuration

  ! error check
  case default
  message=trim(message)//'unable to identify decision for regional representation of groundwater'
  return

  end select  ! groundwater option

  ! *****************************************************************************
  ! *** initialize time step
  ! *****************************************************************************

  ! initialize time step length
  dt_init = hru_data%progStruct%var(iLookPROG%dt_init)%dat(1) ! seconds


  ! *****************************************************************************
  ! *** finalize
  ! *****************************************************************************

end subroutine summa_readRestart

! Set the HRU's relative and absolute tolerances
subroutine setIDATolerances(handle_hru_data,    &
                            relTolTempCas,      &
                            absTolTempCas,      &
                            relTolTempVeg,      &
                            absTolTempVeg,      &
                            relTolWatVeg,       &
                            absTolWatVeg,       &
                            relTolTempSoilSnow, &
                            absTolTempSoilSnow, &
                            relTolWatSnow,      &
                            absTolWatSnow,      &
                            relTolMatric,       &
                            absTolMatric,       &
                            relTolAquifr,       &
                            absTolAquifr) bind(C, name="setIDATolerances")
  USE data_types,only:var_dlength
  USE var_lookup,only:iLookPARAM

  implicit none

  type(c_ptr), intent(in), value          :: handle_hru_data    !  model time data
  real(c_double),intent(in)               :: relTolTempCas
  real(c_double),intent(in)               :: absTolTempCas
  real(c_double),intent(in)               :: relTolTempVeg
  real(c_double),intent(in)               :: absTolTempVeg
  real(c_double),intent(in)               :: relTolWatVeg
  real(c_double),intent(in)               :: absTolWatVeg
  real(c_double),intent(in)               :: relTolTempSoilSnow
  real(c_double),intent(in)               :: absTolTempSoilSnow
  real(c_double),intent(in)               :: relTolWatSnow
  real(c_double),intent(in)               :: absTolWatSnow
  real(c_double),intent(in)               :: relTolMatric
  real(c_double),intent(in)               :: absTolMatric
  real(c_double),intent(in)               :: relTolAquifr
  real(c_double),intent(in)               :: absTolAquifr
  ! local variables
  type(hru_type),pointer                  :: hru_data          !  model time data

  call c_f_pointer(handle_hru_data, hru_data)

#ifdef SUNDIALS_ACTIVE
  hru_data%mparStruct%var(iLookPARAM%relTolTempCas)%dat(1)       = relTolTempCas 
  hru_data%mparStruct%var(iLookPARAM%absTolTempCas)%dat(1)       = absTolTempCas
  hru_data%mparStruct%var(iLookPARAM%relTolTempVeg)%dat(1)       = relTolTempVeg
  hru_data%mparStruct%var(iLookPARAM%absTolTempVeg)%dat(1)       = absTolTempVeg
  hru_data%mparStruct%var(iLookPARAM%relTolWatVeg)%dat(1)        = relTolWatVeg
  hru_data%mparStruct%var(iLookPARAM%absTolWatVeg)%dat(1)        = absTolWatVeg
  hru_data%mparStruct%var(iLookPARAM%relTolTempSoilSnow)%dat(1)  = relTolTempSoilSnow
  hru_data%mparStruct%var(iLookPARAM%absTolTempSoilSnow)%dat(1)  = absTolTempSoilSnow
  hru_data%mparStruct%var(iLookPARAM%relTolWatSnow)%dat(1)       = relTolWatSnow
  hru_data%mparStruct%var(iLookPARAM%absTolWatSnow)%dat(1)       = absTolWatSnow
  hru_data%mparStruct%var(iLookPARAM%relTolMatric)%dat(1)        = relTolMatric
  hru_data%mparStruct%var(iLookPARAM%absTolMatric)%dat(1)        = absTolMatric
  hru_data%mparStruct%var(iLookPARAM%relTolAquifr)%dat(1)        = relTolAquifr
  hru_data%mparStruct%var(iLookPARAM%absTolAquifr)%dat(1)        = absTolAquifr
#endif
end subroutine setIDATolerances
end module INIT_HRU_ACTOR
