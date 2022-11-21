module cppwrap_fileAccess


  !======= Inclusions ===========
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE data_types
  USE globalData
  USE var_lookup,only:maxvarFreq                ! maximum number of output files


  implicit none
  public::initFailedHRUTracker
  
  contains


! Read in the inital parameters, from the txt files that are give to summa as input
! LocalParamInfo.txt
! BasinParamInfo.txt
subroutine read_pinit_C(err) bind(C, name='read_pinit_C')
  USE globalData,only:localParFallback                        ! local column default parameters
  USE globalData,only:basinParFallback                        ! basin-average default parameters
  USE summaFileManager,only:LOCALPARAM_INFO,BASINPARAM_INFO   ! files defining the default values and constraints for model parameters
  USE globalData,only:mpar_meta,bpar_meta                     ! parameter metadata structures
  USE read_pinit_module,only:read_pinit                       ! module to read initial model parameter values

  
  implicit none
  integer(c_int),intent(inout)        :: err                ! Error Code

  character(LEN=256)                  :: message           ! error message of downwind routine
  character(LEN=256)                  :: cmessage           ! error message of downwind routine


 ! *****************************************************************************
 ! *** read default model parameters
 ! *****************************************************************************

 ! read default values and constraints for model parameters (local column)
 call read_pinit(LOCALPARAM_INFO,.TRUE., mpar_meta,localParFallback,err,cmessage)
 if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

 ! read default values and constraints for model parameters (basin-average)
 call read_pinit(BASINPARAM_INFO,.FALSE.,bpar_meta,basinParFallback,err,cmessage)
 if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
end subroutine read_pinit_C

subroutine read_vegitationTables(err) bind(C, name="read_vegitationTables")
  USE SummaActors_setup,only:SOIL_VEG_GEN_PARM
  USE module_sf_noahmplsm,only:read_mp_veg_parameters         ! module to read NOAH vegetation tables
  USE summaFileManager,only:SETTINGS_PATH                     ! define path to settings files (e.g., parameters, soil and veg. tables)
  USE summaFileManager,only:GENPARM,VEGPARM,SOILPARM,MPTABLE  ! files defining the noah tables
  USE globalData,only:model_decisions                         ! model decision structure
  USE var_lookup,only:iLookDECISIONS                          ! look-up values for model decisions

  implicit none
  
  integer(c_int),intent(inout)              :: err                        ! Error Code
  err = 0

 ! read Noah soil and vegetation tables
  call soil_veg_gen_parm(trim(SETTINGS_PATH)//trim(VEGPARM),      & ! filename for vegetation table
      trim(SETTINGS_PATH)//trim(SOILPARM),                        & ! filename for soils table
      trim(SETTINGS_PATH)//trim(GENPARM),                         & ! filename for general table
      trim(model_decisions(iLookDECISIONS%vegeParTbl)%cDecision), & ! classification system used for vegetation
      trim(model_decisions(iLookDECISIONS%soilCatTbl)%cDecision))   ! classification system used for soils

  ! read Noah-MP vegetation tables
  call read_mp_veg_parameters(trim(SETTINGS_PATH)//trim(MPTABLE),                       & ! filename for Noah-MP table
       trim(model_decisions(iLookDECISIONS%vegeParTbl)%cDecision)) ! classification system used for vegetation
  
end subroutine

! allocate the failedHRU logical array and intialize it with all false values
subroutine initFailedHRUTracker(numGRU) bind(C, name="initFailedHRUTracker")
  USE globalData,only:failedHRUs
  implicit none
  integer(c_int), intent(in)        :: numGRU

  if (allocated(failedHRUs))then; deallocate(failedHRUs); endif;
  allocate(failedHRUs(numGRU))

  failedHRUs(:) = .false.


end subroutine

subroutine updateFailed(indxHRU) bind(C, name="updateFailed")
  USE globalData,only:failedHRUs
  implicit none
  integer(c_int), intent(in)        :: indxHRU

  failedHRUs(indxHRU) = .true.
end subroutine

subroutine resetFailedArray() bind(C, name="resetFailedArray")
  USE globalData,only:failedHRUs
  implicit none

  failedHRUs(:) = .false.

end subroutine


subroutine FileAccessActor_DeallocateStructures(handle_forcFileInfo, handle_ncid) bind(C,name="FileAccessActor_DeallocateStructures")
  USE netcdf_util_module,only:nc_file_close 
  USE globalData,only:structInfo                              ! information on the data structures
  USE globalData,only:failedHRUs
  implicit none
  type(c_ptr),intent(in), value        :: handle_forcFileInfo
  type(c_ptr),intent(in), value        :: handle_ncid
  type(var_i),pointer                  :: ncid

  type(file_info_array), pointer       :: forcFileInfo
  integer(i4b)                         :: iFreq
  character(LEN=256)                   :: cmessage
  character(LEN=256)                   :: message
  integer(i4b)                         :: err

  call c_f_pointer(handle_ncid, ncid)
  call c_f_pointer(handle_forcFileInfo, forcFileInfo)


  ! close the open output FIle
  do iFreq=1,maxvarFreq
    if (ncid%var(iFreq)/=integerMissing) then
        call nc_file_close(ncid%var(iFreq),err,cmessage)
        if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
    endif   
  end do
  
  deallocate(forcFileInfo)
  deallocate(ncid)
  deallocate(failedHRUs)
end subroutine FileAccessActor_DeallocateStructures


end module cppwrap_fileAccess



