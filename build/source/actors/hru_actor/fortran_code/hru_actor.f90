module hru_actor
USE,intrinsic :: iso_c_binding
USE nrtype
USE data_types,only:&
                    var_i,          &  
                    var_i8,         &
                    var_d,          &
                    var_ilength,    &
                    var_dlength,    &
                    flagVec,        &
                    hru_type
implicit none


public::setIDATolerances

real(dp),parameter  :: verySmall=1e-3_rkind      ! tiny number
real(dp),parameter  :: smallOffset=1.e-8_rkind   ! small offset (units=days) to force ih=0 at the start of the day

contains





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

end module hru_actor