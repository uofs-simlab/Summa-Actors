! SUMMA - Structure for Unifying Multiple Modeling Alternatives
! Copyright (C) 2014-2020 NCAR/RAL; University of Saskatchewan; University of Washington
!
! This file is part of SUMMA
!
! For more information see: http://www.ral.ucar.edu/projects/summa
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module SummaActors_setup
USE,intrinsic :: iso_c_binding

! initializes parameter data structures (e.g. vegetation and soil parameters).

USE data_types,only:&
                    ! no spatial dimension
                    var_i,               & ! x%var(:)            (i4b)
                    var_i8,              & ! x%var(:)            (i8b)
                    var_d,               & ! x%var(:)            (dp)
                    var_ilength,         & ! x%var(:)%dat        (i4b)
                    var_dlength,         & ! x%var(:)%dat        (dp)
                    zLookup

! access missing values
USE globalData,only:integerMissing   ! missing integer
USE globalData,only:realMissing      ! missing double precision number

! named variables
USE var_lookup,only:iLookATTR                               ! look-up values for local attributes
USE var_lookup,only:iLookTYPE                               ! look-up values for classification of veg, soils etc.
USE var_lookup,only:iLookPARAM                              ! look-up values for local column model parameters
USE var_lookup,only:iLookID                                 ! look-up values for local column model parameters
USE var_lookup,only:iLookBVAR                               ! look-up values for basin-average model variables
USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions
USE globalData,only:urbanVegCategory                        ! vegetation category for urban areas

! metadata structures
USE globalData,only:mpar_meta,bpar_meta                     ! parameter metadata structures

! named variables to define the decisions for snow layers
USE mDecisions_module,only:&
  sameRulesAllLayers, & ! SNTHERM option: same combination/sub-dividion rules applied to all layers
  rulesDependLayerIndex ! CLM option: combination/sub-dividion rules depend on layer index

! named variables to define LAI decisions
USE mDecisions_module,only:&
 monthlyTable,& ! LAI/SAI taken directly from a monthly table for different vegetation classes
 specified      ! LAI/SAI computed from green vegetation fraction and winterSAI and summerLAI parameters

! safety: set private unless specified otherwise
implicit none
private
public::setupHRUParam
public::SOIL_VEG_GEN_PARM
contains

! initializes parameter data structures (e.g. vegetation and soil parameters).
subroutine setupHRUParam(&
                  indxGRU,                 & ! ID of hru
                  indxHRU,                 & ! Index of the parent GRU of the HRU 
                  ! primary data structures (scalars)
                  handle_attrStruct,              & ! local attributes for each HRU
                  handle_typeStruct,              & ! local classification of soil veg etc. for each HRU
                  handle_idStruct,                & ! local classification of soil veg etc. for each HRU
                  ! primary data structures (variable length vectors)
                  handle_mparStruct,              & ! model parameters
                  handle_bparStruct,              & ! basin-average parameters
                  handle_bvarStruct,              & ! basin-average variables
                  handle_dparStruct,              & ! default model parameters
                  handle_lookupStruct,            & ! lookup tables
                  ! local HRU data
                  handle_startTime,               & ! start time for the model simulation
                  handle_oldTime,                 & ! time for the previous model time step
                  ! miscellaneous variables
                  upArea,                  & ! area upslope of each HRU,
                  err) bind(C, name='setupHRUParam')
   ! ---------------------------------------------------------------------------------------
   ! * desired modules
   ! ---------------------------------------------------------------------------------------
   USE nrtype                                                  ! variable types, etc.
   ! subroutines and functions
   use time_utils_module,only:elapsedSec                       ! calculate the elapsed time
   USE mDecisions_module,only:mDecisions                       ! module to read model decisions
   USE ffile_info_module,only:ffile_info                       ! module to read information on forcing datafile
   ! USE read_attribute_module,only:read_attribute               ! module to read local attributes
   USE paramCheck_module,only:paramCheck                       ! module to check consistency of model parameters
   USE pOverwrite_module,only:pOverwrite                       ! module to overwrite default parameter values with info from the Noah tables
   USE ConvE2Temp_module,only:E2T_lookup                       ! module to calculate a look-up table for the temperature-enthalpy conversion
   USE t2enthalpy_module,only:T2E_lookup                       ! module to calculate a look-up table for the temperature-enthalpy conversion
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

   ! USE globalData,only:startTime

   ! ---------------------------------------------------------------------------------------
   ! * variables
   ! ---------------------------------------------------------------------------------------
   implicit none
   ! dummy variables
   ! calling variables
   integer(c_int),intent(in)                :: indxGRU              ! Index of the parent GRU of the HRU
   integer(c_int),intent(in)                :: indxHRU              ! ID to locate correct HRU from netcdf file  
   type(c_ptr), intent(in), value           :: handle_attrStruct    ! local attributes for each HRU
   type(c_ptr), intent(in), value           :: handle_typeStruct    ! local classification of soil veg etc. for each HRU
   type(c_ptr), intent(in), value           :: handle_idStruct      !  
   type(c_ptr), intent(in), value           :: handle_mparStruct    ! model parameters
   type(c_ptr), intent(in), value           :: handle_bparStruct    ! basin-average parameters
   type(c_ptr), intent(in), value           :: handle_bvarStruct    ! basin-average variables
   type(c_ptr), intent(in), value           :: handle_dparStruct    ! default model parameters
   type(c_ptr), intent(in), value           :: handle_lookupStruct     ! start time for the model simulation
   type(c_ptr), intent(in), value           :: handle_startTime     ! start time for the model simulation
   type(c_ptr), intent(in), value           :: handle_oldTime       ! time for the previous model time step
   real(c_double),intent(inout)             :: upArea
   integer(c_int),intent(inout)             :: err
  
   ! local variables
   type(var_d),pointer                      :: attrStruct           ! local attributes for each HRU
   type(var_i),pointer                      :: typeStruct           ! local classification of soil veg etc. for each HRU
   type(var_i8),pointer                     :: idStruct             !
   type(var_dlength),pointer                :: mparStruct           ! model parameters
   type(var_d),pointer                      :: bparStruct           ! basin-average parameters
   type(var_dlength),pointer                :: bvarStruct           ! basin-average variables
   type(var_d),pointer                      :: dparStruct           ! default model parameters
   type(zLookup),pointer                    :: lookupStruct         ! default model parameters
   type(var_i),pointer                      :: startTime            ! start time for the model simulation
   type(var_i),pointer                      :: oldTime              ! time for the previous model time step
   character(len=256)                       :: message              ! error message
   character(len=256)                       :: cmessage             ! error message of downwind routine
   integer(i4b),dimension(8)                :: function1start       ! start time for the function
   integer(i4b),dimension(8)                :: function1end         ! end time for the function
   integer(i4b)                             :: function1elapsed     ! elapsed time for the function
   integer(i4b),dimension(8)                :: function2start       ! start time for the function
   integer(i4b),dimension(8)                :: function2end         ! end time for the function
   integer(i4b)                             :: function2elapsed     ! elapsed time for the function
   integer(i4b),dimension(8)                :: function3start       ! start time for the function
   integer(i4b),dimension(8)                :: function3end         ! end time for the function
   integer(i4b)                             :: function3elapsed     ! elapsed time for the function
   integer(i4b),dimension(8)                :: function4start       ! start time for the function
   integer(i4b),dimension(8)                :: function4end         ! end time for the function
   integer(i4b)                             :: function4elapsed     ! elapsed time for the function

   
   ! ---------------------------------------------------------------------------------------
   ! initialize error control
   err=0; message='setupHRUParam/'
   ! initialize the start of the initialization
   call date_and_time(values=startSetup)

   ! convert to fortran pointer from C++ pointer
   call c_f_pointer(handle_attrStruct, attrStruct)
   call c_f_pointer(handle_typeStruct, typeStruct)
   call c_f_pointer(handle_idStruct, idStruct)
   call c_f_pointer(handle_mparStruct, mparStruct)
   call c_f_pointer(handle_bparStruct, bparStruct)
   call c_f_pointer(handle_bvarStruct, bvarStruct)
   call c_f_pointer(handle_dparStruct, dparStruct)
   call c_f_pointer(handle_lookupStruct, lookupStruct)
   call c_f_pointer(handle_startTime, startTime)
   call c_f_pointer(handle_oldTime, oldTime)

   ! ffile_info and mDecisions moved to their own seperate subroutine call
   
   oldTime%var(:) = startTime%var(:)

   ! get the maximum number of snow layers
   select case(model_decisions(iLookDECISIONS%snowLayers)%iDecision)
      case(sameRulesAllLayers);    maxSnowLayers = 100
      case(rulesDependLayerIndex); maxSnowLayers = 5
      case default; err=20; 
         message=trim(message)//'unable to identify option to combine/sub-divide snow layers'
         print*, message
         return
   end select ! (option to combine/sub-divide snow layers)

   ! get the maximum number of layers
   maxLayers = gru_struc(1)%hruInfo(1)%nSoil + maxSnowLayers

   ! define monthly fraction of green vegetation
   greenVegFrac_monthly = (/0.01_dp, 0.02_dp, 0.03_dp, 0.07_dp, 0.50_dp, 0.90_dp, 0.95_dp, 0.96_dp, 0.65_dp, 0.24_dp, 0.11_dp, 0.02_dp/)

   ! define urban vegetation category
   select case(trim(model_decisions(iLookDECISIONS%vegeParTbl)%cDecision))
      case('USGS');                     urbanVegCategory =    1
      case('MODIFIED_IGBP_MODIS_NOAH'); urbanVegCategory =   13
      case('plumberCABLE');             urbanVegCategory = -999
      case('plumberCHTESSEL');          urbanVegCategory = -999
      case('plumberSUMMA');             urbanVegCategory = -999
      case default
         message=trim(message)//'unable to identify vegetation category'
         print*, message
         return
   end select

   ! *****************************************************************************
   ! *** compute derived model variables that are pretty much constant for the basin as a whole
   ! *****************************************************************************
   ! calculate the fraction of runoff in future time steps
   call date_and_time(values=function1start)
   call fracFuture(bparStruct%var,    &  ! vector of basin-average model parameters
                   bvarStruct,        &  ! data structure of basin-average variables
                   err,cmessage)                   ! error control
   if(err/=0)then
      message=trim(message)//trim(cmessage)
      print*, message
      return
   endif
   call date_and_time(values=function1end)
   function1elapsed = elapsedSec(function1start, function1end)
   print*, 'fracFuture: ', function1elapsed


   ! check that the parameters are consistent
   call date_and_time(values=function2start)
   call paramCheck(mparStruct,err,cmessage)
   if(err/=0)then
      message=trim(message)//trim(cmessage)
      print*, message
      return
   endif
   call date_and_time(values=function2end)
   function2elapsed = elapsedSec(function2start, function2end)
   print*, 'paramCheck: ', function2elapsed


   ! calculate a look-up table for the temperature-enthalpy conversion
   call date_and_time(values=function3start)
   call E2T_lookup(mparStruct,err,cmessage)
   if(err/=0)then
      message=trim(message)//trim(cmessage)
      print*, message
      return
   endif
   call date_and_time(values=function3end)
   function3elapsed = elapsedSec(function3start, function3end)
   print*, 'E2T_lookup: ', function3elapsed


   ! calculate a lookup table to compute enthalpy from temperature
   call T2E_lookup(gru_struc(indxGRU)%hruInfo(1)%nSoil,   &   ! intent(in):    number of soil layers
                   mparStruct,        &   ! intent(in):    parameter data structure
                   lookupStruct,      &   ! intent(inout): lookup table data structure
                   err,cmessage)                              ! intent(out):   error control
   if(err/=0)then; message=trim(message)//trim(cmessage)
      print*, message
      return; 
   endif

   ! overwrite the vegetation height
   HVT(typeStruct%var(iLookTYPE%vegTypeIndex)) = mparStruct%var(iLookPARAM%heightCanopyTop)%dat(1)
   HVB(typeStruct%var(iLookTYPE%vegTypeIndex)) = mparStruct%var(iLookPARAM%heightCanopyBottom)%dat(1)

   ! overwrite the tables for LAI and SAI
   if(model_decisions(iLookDECISIONS%LAI_method)%iDecision == specified)then
      SAIM(typeStruct%var(iLookTYPE%vegTypeIndex),:) = mparStruct%var(iLookPARAM%winterSAI)%dat(1)
      LAIM(typeStruct%var(iLookTYPE%vegTypeIndex),:) = mparStruct%var(iLookPARAM%summerLAI)%dat(1)*greenVegFrac_monthly
   endif

   ! compute total area of the upstream HRUS that flow into each HRU
   upArea = 0._dp
   ! Check if lateral flows exists within the HRU
   if(typeStruct%var(iLookTYPE%downHRUindex)==typeStruct%var(iLookID%hruId))then
      upArea = upArea + attrStruct%var(iLookATTR%HRUarea)
   endif


   ! identify the total basin area for a GRU (m2)
   associate(totalArea => bvarStruct%var(iLookBVAR%basin__totalArea)%dat(1) )
      totalArea = 0._dp
      totalArea = totalArea + attrStruct%var(iLookATTR%HRUarea)
   end associate

   ! identify the end of the initialization
   call date_and_time(values=endSetup)

   ! aggregate the elapsed time for the initialization
   elapsedSetup = elapsedSec(startSetup, endSetup)

end subroutine setupHRUParam


 ! =================================================================================================
 ! =================================================================================================
 ! =================================================================================================
 ! =================================================================================================
 ! =================================================================================================
 ! =================================================================================================

 ! **************************************************************************************************
 ! private subroutine SOIL_VEG_GEN_PARM: Read soil, vegetation and other model parameters (from NOAH)
 ! **************************************************************************************************
 !-----------------------------------------------------------------
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

!-----------------------------------------------------------------
END SUBROUTINE SOIL_VEG_GEN_PARM
!-----------------------------------------------------------------

end module SummaActors_setup