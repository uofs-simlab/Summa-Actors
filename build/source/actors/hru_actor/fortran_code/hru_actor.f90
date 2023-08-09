module hru_actor
USE,intrinsic :: iso_c_binding
USE nrtype
USE data_types,only:&
                    var_i,          &  
                    var_i8,         &
                    var_d,          &
                    var_ilength,    &
                    var_dlength,    &
                    flagVec
implicit none

public::getFirstTimestep
public::setTimeZoneOffset
public::prepareOutput
public::updateCounters
public::setIDATolerances

real(dp),parameter  :: verySmall=1e-3_rkind      ! tiny number
real(dp),parameter  :: smallOffset=1.e-8_rkind   ! small offset (units=days) to force ih=0 at the start of the day

contains


! Find the first timestep within the forcing file
subroutine getFirstTimestep(iFile, iRead, err) bind(C, name="getFirstTimestep")
  USE globalData,only:forcingDataStruct         ! forcing structure
  USE globalData,only:vecTime                   ! time structure for forcing 
  USE globalData,only:dJulianStart              ! julian day of start time of simulation
  USE globalData,only:data_step                 ! length of the data step (s)
  USE globalData,only:refJulDay_data            ! reference time for data files (fractional julian days)
   
  USE multiconst,only:secprday                  ! number of seconds in a day
  
  USE nr_utility_module,only:arth               ! get a sequence of numbers

  implicit none

  integer(c_int),intent(in)                               :: iFile
  integer(c_int),intent(out)                              :: iRead
  integer(c_int),intent(out)                              :: err
  ! local variables
  character(len=256)                                      :: message
  real(dp)                                                :: timeVal(1)    ! single time value (restrict time read)
  real(dp),dimension(forcingDataStruct(iFile)%nTimeSteps) :: fileTime      ! array of time from netcdf file
  real(dp),dimension(forcingDataStruct(iFile)%nTimeSteps) :: diffTime      ! array of time differences

  err=0; message="hru_actor.f90 - getFirstTimeStep"

  ! get time vector & convert units based on offset and data step
  timeVal(1) = vecTime(iFile)%dat(1)
  fileTime = arth(0,1,forcingDataStruct(iFile)%nTimeSteps) * data_step/secprday + refJulDay_data &
              + timeVal(1)/forcingDataStruct(iFile)%convTime2Days

  ! find difference of fileTime from currentJulDay
  diffTime=abs(fileTime-dJulianStart)
  
  if(any(diffTime < verySmall))then
    iRead=minloc(diffTime,1)
  else
    iRead=-1 ! set to -1 to designinate this forcing file is not the start
  endif
  
end subroutine getFirstTimestep

! set the refTimeString and extract the time to set the tmZonOffsetFracDay
subroutine setTimeZoneOffset(iFile, tmZoneOffsetFracDay, err) bind(C, name="setTimeZoneOffset")
  USE globalData,only:forcingDataStruct         ! forcing structure


  USE time_utils_module,only:extractTime        ! extract time info from units string
  USE time_utils_module,only:fracDay            ! compute fractional day

  USE summafilemanager,only:NC_TIME_ZONE
  implicit none

  integer(c_int),intent(in)             :: iFile
  real(c_double),intent(out)            :: tmZoneOffsetFracDay
  integer(c_int),intent(out)            :: err

  ! local variables
  character(len=256)                    :: message
  character(len=256)                    :: cmessage
  integer(i4b)                          :: iyyy,im,id,ih,imin ! date
  integer(i4b)                          :: ih_tz,imin_tz      ! time zone information
  real(dp)                              :: dsec,dsec_tz       ! seconds

  err=0; message="hru_actor.f90 - setForcingTimeInfo";

 ! define the reference time for the model simulation
 call extractTime(forcingDataStruct(iFile)%refTimeString,& ! input  = units string for time data
                 iyyy,im,id,ih,imin,dsec,                & ! output = year, month, day, hour, minute, second
                 ih_tz, imin_tz, dsec_tz,                & ! output = time zone information (hour, minute, second)
                 err,cmessage)                             ! output = error code and error message
  if(err/=0)then; message=trim(message)//trim(cmessage); print*, "message"; return; end if
  
 ! set the timezone offset
 select case(trim(NC_TIME_ZONE))
    case('ncTime'); tmZoneOffsetFracDay = sign(1, ih_tz) * fracDay(ih_tz,   & ! time zone hour
                                                                  imin_tz, & ! time zone minute
                                                                  dsec_tz)   ! time zone second
    case('utcTime');   tmZoneOffsetFracDay = 0._dp
    case('localTime'); tmZoneOffsetFracDay = 0._dp
    case default; err=20; message=trim(message)//'unable to identify time zone info option'; return
  end select ! (option time zone option)

end subroutine setTimeZoneOffset


subroutine readForcingHRU(indxGRU, iStep, iRead, handle_timeStruct, handle_forcStruct, &
                          iFile, err) bind(C, name="readForcingHRU")
  USE multiconst,only:secprday                  ! number of seconds in a day
  ! global Data
  USE globalData,only:data_step                 ! length of the data step (s)
  USE globalData,only:dJulianStart              ! julian day of start time of simulation
  USE globalData,only:refJulDay_data            ! reference time for data files (fractional julian days)
  USE globalData,only:integerMissing            ! integer missing value
  USE globalData,only:vecTime
  USE globalData,only:forcingDataStruct
  USE globalData,only:time_meta,forc_meta
  USE var_lookup,only:iLookTIME,iLookFORCE
  USE data_types,only:var_i,var_d
  USE netcdf                                   ! used for nf90_max_name
  USE time_utils_module,only:compcalday                 ! convert julian day to calendar date

  implicit none

  integer(c_int),intent(in)               :: indxGRU          ! Index of the GRU in gru_struc
  integer(c_int),intent(in)               :: istep            ! Model Timestep
  integer(c_int),intent(in)               :: iRead            ! Model Timestep 
  type(c_ptr),intent(in),value            :: handle_timeStruct! vector of time data for a given time step
  type(c_ptr),intent(in),value            :: handle_forcStruct! model parameters
  integer(c_int),intent(in)               :: iFile            ! index of current forcing file from forcing file list 
  integer(c_int),intent(out)              :: err              ! Model Timestep
  ! local variables
  type(var_i),pointer                     :: timeStruct       !  model time data
  type(var_d),pointer                     :: forcStruct       !  model forcing data
  real(dp)                                :: currentJulDay    ! Julian day of current time step
  real(dp)                                :: dataJulDay       ! julian day of current forcing data step being read
  ! Counters
  integer(i4b)                            :: iline            ! loop through lines in the file
  integer(i4b)                            :: iVar
  integer(i4b)                            :: iNC
  ! other
  logical(lgt),dimension(size(forc_meta)) :: checkForce       ! flags to check forcing data variables exist
  
  real(dp)                                :: dsec             ! double precision seconds (not used)
  real(dp),parameter                      :: dataMin=-1._dp   ! minimum allowable data value (all forcing variables should be positive)
  character(len = nf90_max_name)          :: varName          ! dimenison name

  character(len=256)                      :: message          ! error message
  character(len=256)                      :: cmessage         ! error message

  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_timeStruct, timeStruct)
  call c_f_pointer(handle_forcStruct, forcStruct)

  err=0;message="hru_actor.f90 - readForcingHRU";

  ! determine the julDay of current model step (istep) we need to read
  currentJulDay = dJulianStart + (data_step*real(iStep-1,dp))/secprday

  timeStruct%var(:) = integerMissing
  dataJulDay = vecTime(iFile)%dat(iRead)/forcingDataStruct(iFile)%convTime2Days + refJulDay_data
 if(abs(currentJulDay - dataJulDay) > verySmall)then
    write(message,'(a,f18.8,a,f18.8)') trim(message)//'date for time step: ',dataJulDay,' differs from the expected date: ',currentJulDay
    print*, message
    err=40
    return
  end if

  ! convert julian day to time vector
  ! NOTE: use small offset to force ih=0 at the start of the day
  call compcalday(dataJulDay+smallOffset,         & ! input  = julian day
                  timeStruct%var(iLookTIME%iyyy),      & ! output = year
                  timeStruct%var(iLookTIME%im),        & ! output = month
                  timeStruct%var(iLookTIME%id),        & ! output = day
                  timeStruct%var(iLookTIME%ih),        & ! output = hour
                  timeStruct%var(iLookTIME%imin),dsec, & ! output = minute/second
                  err,cmessage)                     ! output = error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
  
  ! check to see if any of the time data is missing -- note that it is OK if ih_tz or imin_tz are missing
  if((timeStruct%var(iLookTIME%iyyy)==integerMissing) .or. (timeStruct%var(iLookTIME%im)==integerMissing) .or. (timeStruct%var(iLookTIME%id)==integerMissing) .or. (timeStruct%var(iLookTIME%ih)==integerMissing) .or. (timeStruct%var(iLookTIME%imin)==integerMissing))then
      do iline=1,size(timeStruct%var)
          if(timeStruct%var(iline)==integerMissing)then; err=40; message=trim(message)//"variableMissing[var='"//trim(time_meta(iline)%varname)//"']"; return; end if
      end do
  end if

  ! initialize flags for forcing data
  checkForce(:) = .false.
  checkForce(iLookFORCE%time) = .true.  ! time is handled separately

  do iNC=1,forcingDataStruct(iFile)%nVars
    ! check variable is desired
    if(forcingDataStruct(iFile)%var_ix(iNC)==integerMissing) cycle

    ! get index in forcing structure
    iVar = forcingDataStruct(iFile)%var_ix(iNC)
    checkForce(iVar) = .true.

    ! check individual data value
    if(forcingDataStruct(iFile)%var(ivar)%dataFromFile(indxGRU,iRead)<dataMin)then
      write(message,'(a,f13.5)') trim(message)//'forcing data for variable '//trim(varname)//' is less than minimum allowable value ', dataMin
      err=20; return
    endif
    ! put the data into structures
    forcStruct%var(ivar) = forcingDataStruct(iFile)%var(ivar)%dataFromFile(indxGRU,iRead)
  end do  ! loop through forcing variables
  
  ! check if any forcing data is missing
  if(count(checkForce)<size(forc_meta))then
    do iline=1,size(forc_meta)
    if(.not.checkForce(iline))then
      message=trim(message)//"checkForce_variableMissing[var='"//trim(forc_meta(iline)%varname)//"']"
      err=20; return
    endif    ! if variable is missing
    end do   ! looping through variables
  end if   ! if any variables are missing
end subroutine readForcingHRU 

! This is part 2: from the reading of forcing - separated it so we could call from C++
subroutine computeTimeForcingHRU(handle_timeStruct, handle_forcStruct, fracJulDay, yearLength, err) bind(C, name="computeTimeForcingHRU")
  USE var_lookup,only:iLookTIME,iLookFORCE
  USE time_utils_module,only:compJulDay         ! convert calendar date to julian day
  USE data_types,only:var_i,var_d
  USE multiconst,only:secprday                  ! number of seconds in a day
  USE globalData,only:refJulDay                 ! reference time (fractional julian days)

  implicit none
  type(c_ptr),intent(in),value        :: handle_timeStruct     ! vector of time data for a given time step
  type(c_ptr),intent(in),value        :: handle_forcStruct ! model parameters
  real(c_double),intent(out)          :: fracJulDay
  integer(c_int),intent(out)          :: yearLength    
  integer(c_int),intent(out)          :: err   
  ! local variables
  type(var_i),pointer                 :: timeStruct       !  model time data
  type(var_d),pointer                 :: forcStruct       !  model forcing data
  real(dp)                            :: startJulDay      ! julian day at the start of the year
  real(dp)                            :: currentJulDay    ! Julian day of current time step
  character(len=256)                  :: message          ! error message for downwind routine
  character(len=256)                  :: cmessage         ! error message for downwind routine
  logical(lgt),parameter              :: checkTime=.false.  ! flag to check the time

  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_timeStruct, timeStruct)
  call c_f_pointer(handle_forcStruct, forcStruct)

  err=0; message="hru_actor.f90 - computTimeForcingHRU/"

  ! compute the julian day at the start of the year
  call compjulday(timeStruct%var(iLookTIME%iyyy),          & ! input  = year
                  1, 1, 1, 1, 0._dp,                  & ! input  = month, day, hour, minute, second
                  startJulDay,err,cmessage)             ! output = julian day (fraction of day) + error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if

  ! compute the fractional julian day for the current time step
  call compjulday(timeStruct%var(iLookTIME%iyyy),           & ! input  = year
                  timeStruct%var(iLookTIME%im),             & ! input  = month
                  timeStruct%var(iLookTIME%id),             & ! input  = day
                  timeStruct%var(iLookTIME%ih),             & ! input  = hour
                  timeStruct%var(iLookTIME%imin),0._dp,     & ! input  = minute/second
                  currentJulDay,err,cmessage)            ! output = julian day (fraction of day) + error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
  ! compute the time since the start of the year (in fractional days)
  fracJulDay = currentJulDay - startJulDay
  ! set timing of current forcing vector (in seconds since reference day)
  ! NOTE: It is a bit silly to have time information for each HRU and GRU
  forcStruct%var(iLookFORCE%time) = (currentJulDay-refJulDay)*secprday

  ! compute the number of days in the current year
  yearLength = 365
  if(mod(timeStruct%var(iLookTIME%iyyy),4) == 0)then
    yearLength = 366
    if(mod(timeStruct%var(iLookTIME%iyyy),100) == 0)then
    yearLength = 365
    if(mod(timeStruct%var(iLookTIME%iyyy),400) == 0)then
      yearLength = 366
    end if
    end if
  end if

  ! test
  if(checkTime)then
    write(*,'(i4,1x,4(i2,1x),f9.3,1x,i4)')  timeStruct%var(iLookTIME%iyyy),           & ! year
                                            timeStruct%var(iLookTIME%im),             & ! month
                                            timeStruct%var(iLookTIME%id),             & ! day
                                            timeStruct%var(iLookTIME%ih),             & ! hour
                                            timeStruct%var(iLookTIME%imin),           & ! minute
                                            fracJulDay,                          & ! fractional julian day for the current time step
                                            yearLength                             ! number of days in the current year
    !pause ' checking time'
  end if
end subroutine computeTimeForcingHRU

! Prepare structure for being sent off to the file access actor
! call set alarms and calc stats
subroutine prepareOutput(&
                        modelTimeStep,      &
                        ! statistics variables
                        handle_forcStat,           & ! model forcing data
                        handle_progStat,           & ! model prognostic (state) variables
                        handle_diagStat,           & ! model diagnostic variables
                        handle_fluxStat,           & ! model fluxes
                        handle_indxStat,           & ! model indices
                        handle_bvarStat,           & ! basin-average variables
                        ! primary data structures (scalars)
                        handle_timeStruct,         & ! x%var(:)     -- model time data
                        handle_forcStruct,         & ! x%var(:)     -- model forcing data
                        handle_attrStruct,         & ! x%var(:)     -- local attributes for each HRU
                        handle_typeStruct,         & ! x%var(:)     -- local classification of soil veg etc. for each HRU
                        ! primary data structures (variable length vectors)
                        handle_indxStruct,         & ! x%var(:)%dat -- model indices
                        handle_mparStruct,         & ! x%var(:)%dat -- model parameters
                        handle_progStruct,         & ! x%var(:)%dat -- model prognostic (state) variables
                        handle_diagStruct,         & ! x%var(:)%dat -- model diagnostic variables
                        handle_fluxStruct,         & ! x%var(:)%dat -- model fluxes
                        ! basin-average structures
                        handle_bparStruct,         & ! x%var(:)     -- basin-average parameters
                        handle_bvarStruct,         & ! x%var(:)%dat -- basin-average variables
                        handle_statCounter,        &
                        handle_outputTimeStep,     & ! x%var(:)
                        handle_resetStats,         & ! x%var(:)
                        handle_finalizeStats,      & ! x%var(:)
                        handle_finshTime,          & ! x%var(:)    -- end time for the model simulation
                        handle_oldTime,            & ! x%var(:)    -- time for the previous model time step
                        err) bind(C, name="prepareOutput")
  USE globalData,only:structInfo
  USE globalData,only:startWrite,endWrite

  USE globalData,only:ixProgress                              ! define frequency to write progress
  USE globalData,only:ixRestart                               ! define frequency to write restart files
  USE globalData,only:gru_struc

  USE globalData,only:newOutputFile                           ! define option for new output files
  USE summa_alarms,only:summa_setWriteAlarms

  USE globalData,only:forc_meta,attr_meta,type_meta           ! metaData structures
  USE output_stats,only:calcStats                             ! module for compiling output statistics
  USE var_lookup,only:iLookTIME,iLookDIAG,iLookPROG,iLookINDEX, &
    iLookFreq,maxvarFreq ! named variables for time data structure
  USE globalData,only:time_meta,forc_meta,diag_meta,prog_meta,&
    flux_meta,indx_meta,bvar_meta,bpar_meta,mpar_meta           ! metadata on the model time
  USE globalData,only:statForc_meta,statProg_meta,statDiag_meta,&
    statFlux_meta,statIndx_meta,statBvar_meta             ! child metadata for stats
  ! index of the child data structure
  USE globalData,only:forcChild_map,progChild_map,diagChild_map,&
    fluxChild_map,indxChild_map,bvarChild_map             ! index of the child data structure: stats forc
  implicit none
  integer(i4b),intent(in)                  :: modelTimeStep   ! time step index
  type(c_ptr), intent(in), value           :: handle_forcStat !  model forcing data
  type(c_ptr), intent(in), value           :: handle_progStat !  model prognostic (state) variables
  type(c_ptr), intent(in), value           :: handle_diagStat !  model diagnostic variables
  type(c_ptr), intent(in), value           :: handle_fluxStat !  model fluxes
  type(c_ptr), intent(in), value           :: handle_indxStat !  model indices
  type(c_ptr), intent(in), value           :: handle_bvarStat !  basin-average variables
  ! primary data structures (scalars)
  type(c_ptr), intent(in), value           :: handle_timeStruct !  model time data
  type(c_ptr), intent(in), value           :: handle_forcStruct !  model forcing data
  type(c_ptr), intent(in), value           :: handle_attrStruct !  local attributes for each HRU
  type(c_ptr), intent(in), value           :: handle_typeStruct !  local classification of soil veg etc. for each HRU
  ! primary data structures (variable length vectors)
  type(c_ptr), intent(in), value           :: handle_indxStruct !  model indices
  type(c_ptr), intent(in), value           :: handle_mparStruct !  model parameters
  type(c_ptr), intent(in), value           :: handle_progStruct !  model prognostic (state) variables
  type(c_ptr), intent(in), value           :: handle_diagStruct !  model diagnostic variables
  type(c_ptr), intent(in), value           :: handle_fluxStruct !  model fluxes
  ! basin-average structures
  type(c_ptr), intent(in), value           :: handle_bparStruct !  basin-average parameters
  type(c_ptr), intent(in), value           :: handle_bvarStruct !  basin-average variables
  ! local HRU variables
  type(c_ptr), intent(in), value           :: handle_statCounter
  type(c_ptr), intent(in), value           :: handle_outputTimeStep
  type(c_ptr), intent(in), value           :: handle_resetStats
  type(c_ptr), intent(in), value           :: handle_finalizeStats
  type(c_ptr), intent(in), value           :: handle_finshTime    ! end time for the model simulation
  type(c_ptr), intent(in), value           :: handle_oldTime      ! time for the previous model time step
  ! run time variables
  integer(i4b),intent(out)                 :: err
  ! local vairiables for pointers
  type(var_dlength),pointer                :: forcStat        ! model forcing data
  type(var_dlength),pointer                :: progStat        ! model prognostic (state) variables
  type(var_dlength),pointer                :: diagStat        ! model diagnostic variables
  type(var_dlength),pointer                :: fluxStat        ! model fluxes
  type(var_dlength),pointer                :: indxStat        ! model indices
  type(var_dlength),pointer                :: bvarStat        ! basin-average variabl
  type(var_i),pointer                      :: timeStruct      ! model time data
  type(var_d),pointer                      :: forcStruct      ! model forcing data
  type(var_d),pointer                      :: attrStruct      ! local attributes for each HRU
  type(var_i),pointer                      :: typeStruct      ! local classification of soil veg etc. for each HRU
  type(var_ilength),pointer                :: indxStruct      ! model indices
  type(var_dlength),pointer                :: mparStruct      ! model parameters
  type(var_dlength),pointer                :: progStruct      ! model prognostic (state) variables
  type(var_dlength),pointer                :: diagStruct      ! model diagnostic variables
  type(var_dlength),pointer                :: fluxStruct      ! model fluxes
  type(var_d),pointer                      :: bparStruct      ! basin-average parameters
  type(var_dlength),pointer                :: bvarStruct      ! basin-average variables
  type(var_i),pointer                      :: statCounter     ! time counter for stats
  type(var_i),pointer                      :: outputTimeStep  ! timestep in output files
  type(flagVec),pointer                    :: resetStats      ! flags to reset statistics
  type(flagVec),pointer                    :: finalizeStats   ! flags to finalize statistics
  type(var_i),pointer                      :: finshTime       ! end time for the model simulation
  type(var_i),pointer                      :: oldTime         !
  ! local variables
  character(len=256)                       :: message 
  character(len=256)                       :: cmessage
  logical(lgt)                             :: defNewOutputFile=.false.
  integer(i4b)                             :: iFreq             ! index of the output frequency
  integer(i4b)                             :: iStruct           ! index of model structure
  logical(lgt)                             :: printProgress=.false.
  logical(lgt)                             :: printRestart=.false.
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_forcStat, forcStat)
  call c_f_pointer(handle_progStat, progStat)
  call c_f_pointer(handle_diagStat, diagStat)
  call c_f_pointer(handle_fluxStat, fluxStat)
  call c_f_pointer(handle_indxStat, indxStat)
  call c_f_pointer(handle_bvarStat, bvarStat)
  call c_f_pointer(handle_timeStruct, timeStruct)
  call c_f_pointer(handle_forcStruct, forcStruct)
  call c_f_pointer(handle_attrStruct, attrStruct)
  call c_f_pointer(handle_typeStruct, typeStruct)
  call c_f_pointer(handle_indxStruct, indxStruct)
  call c_f_pointer(handle_mparStruct, mparStruct)
  call c_f_pointer(handle_progStruct, progStruct)
  call c_f_pointer(handle_diagStruct, diagStruct)
  call c_f_pointer(handle_fluxStruct, fluxStruct)
  call c_f_pointer(handle_bparStruct, bparStruct)
  call c_f_pointer(handle_bvarStruct, bvarStruct)
  call c_f_pointer(handle_statCounter, statCounter)
  call c_f_pointer(handle_outputTimeStep, outputTimeStep)
  call c_f_pointer(handle_resetStats, resetStats)
  call c_f_pointer(handle_finalizeStats, finalizeStats)
  call c_f_pointer(handle_finshTime, finshTime);
  call c_f_pointer(handle_oldTime, oldTime)
  
  ! Start of Subroutine
  err=0; message='summa_manageOutputFiles/'
  ! identify the start of the writing
  call date_and_time(values=startWrite)

  ! initialize the statistics flags
  if(modelTimeStep==1)then

    ! initialize time step index
    allocate(statCounter%var(maxVarFreq))
    allocate(outputTimeStep%var(maxVarFreq))
    statCounter%var(1:maxVarFreq) = 1
    outputTimeStep%var(1:maxVarFreq) = 1

    allocate(resetStats%dat(maxVarFreq))
    allocate(finalizeStats%dat(maxVarFreq))
    ! initialize flags to reset/finalize statistics
    resetStats%dat(:)    = .true.   ! start by resetting statistics
    finalizeStats%dat(:) = .false.  ! do not finalize stats on the first time step

    ! set stats flag for the timestep-level output
    finalizeStats%dat(iLookFreq%timestep)=.true.
  endif  ! if the first time step

  ! Many variables get there values from summa4chm_util.f90:getCommandArguments()
  call summa_setWriteAlarms(oldTime%var, timeStruct%var, finshTime%var,  &   ! time vectors
                            newOutputFile,  defNewOutputFile,            &
                            ixRestart,      printRestart,                &   ! flag to print the restart file
                            ixProgress,     printProgress,               &   ! flag to print simulation progress
                            resetStats%dat, finalizeStats%dat,           &   ! flags to reset and finalize stats
                            statCounter%var,                             &   ! statistics counter
                            err, cmessage)                                  ! error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

 ! ****************************************************************************
 ! *** calculate output statistics
 ! ****************************************************************************
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc'); call calcStats(forcStat%var, forcStruct%var, statForc_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
      case('prog'); call calcStats(progStat%var, progStruct%var, statProg_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
      case('diag'); call calcStats(diagStat%var, diagStruct%var, statDiag_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
      case('flux'); call calcStats(fluxStat%var, fluxStruct%var, statFlux_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
      case('indx'); call calcStats(indxStat%var, indxStruct%var, statIndx_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)     
    end select
    if(err/=0)then; message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'; return; endif
  end do  ! (looping through structures)
    
  ! calc basin stats
  call calcStats(bvarStat%var(:), bvarStruct%var(:), statBvar_meta, resetStats%dat, finalizeStats%dat, statCounter%var, err, cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage)//'[bvar stats]'; return; endif

end subroutine prepareOutput


! Update all of the counters for time and output step
subroutine updateCounters(handle_timeStruct, handle_statCounter, handle_outputTimeStep, &
        handle_resetStats, handle_oldTime, handle_finalizeStats) bind(C, name="updateCounters")
  USE globalData,only:startWrite,endWrite,elapsedWrite
  USE var_lookup,only:maxvarFreq
  USE time_utils_module,only:elapsedSec                       ! calculate the elapsed time

  type(c_ptr), intent(in), value           :: handle_statCounter
  type(c_ptr), intent(in), value           :: handle_outputTimeStep
  type(c_ptr), intent(in), value           :: handle_resetStats
  type(c_ptr), intent(in), value           :: handle_oldTime      ! time for the previous model time step
  type(c_ptr), intent(in), value           :: handle_timeStruct !  model time data
  type(c_ptr), intent(in), value           :: handle_finalizeStats

  type(var_i),pointer                      :: statCounter     ! time counter for stats
  type(var_i),pointer                      :: outputTimeStep  ! timestep in output files
  type(flagVec),pointer                    :: resetStats      ! flags to reset statistics
  type(var_i),pointer                      :: oldTime         !
  type(var_i),pointer                      :: timeStruct      ! model time data
  type(flagVec),pointer                    :: finalizeStats   ! flags to finalize statistics

  integer(i4b)                             :: iFreq
  
  call c_f_pointer(handle_statCounter, statCounter)
  call c_f_pointer(handle_outputTimeStep, outputTimeStep)
  call c_f_pointer(handle_resetStats, resetStats)
  call c_f_pointer(handle_oldTime, oldTime)
  call c_f_pointer(handle_timeStruct, timeStruct)
  call c_f_pointer(handle_finalizeStats, finalizeStats)

  ! *****************************************************************************
  ! *** update counters
  ! *****************************************************************************
  ! increment output file timestep
  do iFreq = 1,maxvarFreq
    statCounter%var(iFreq) = statCounter%var(iFreq)+1
    if(finalizeStats%dat(iFreq)) outputTimeStep%var(iFreq) = outputTimeStep%var(iFreq) + 1
  end do

 ! if finalized stats, then reset stats on the next time step
 resetStats%dat(:) = finalizeStats%dat(:)

 ! save time vector
 oldTime%var(:) = timeStruct%var(:)

 ! *****************************************************************************
 ! *** finalize
 ! *****************************************************************************

 ! identify the end of the writing
 call date_and_time(values=endWrite)

 elapsedWrite = elapsedWrite + elapsedSec(startWrite, endWrite)
end subroutine updateCounters

! Set the HRU's relative and absolute tolerances
subroutine setIDATolerances(handle_mparStruct, rtol, atol) bind(C, name="setIDATolerances")
  USE data_types,only:var_dlength
  USE var_lookup,only:iLookPARAM

  implicit none

  type(c_ptr), intent(in), value           :: handle_mparStruct !  model parameters
  real(c_double),intent(in)               :: rtol              ! relative tolerance
  real(c_double),intent(in)               :: atol              ! absolute tolerance
  ! local variables
  type(var_dlength),pointer                :: mparStruct        ! model parameters

  call c_f_pointer(handle_mparStruct, mparStruct)

  mparStruct%var(iLookPARAM%relErrTol_ida)%dat(1) = rtol
  mparStruct%var(iLookPARAM%absErrTol_ida)%dat(1) = atol



end subroutine setIDATolerances

end module hru_actor