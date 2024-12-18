module output_buffer
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE globalData,only:integerMissing      ! missing integer value
  USE globalData,only:realMissing         ! missing double precision value
  USE data_types
  USE actor_data_types
  implicit none
  public::f_defOutput
  public::f_setChunkSize
  public::f_addFailedGru
  public::f_resetFailedGru
  public::f_resetOutputTimestep
  public::f_setFailedGruMissing
  public::f_allocateOutputBuffer
  public::f_deallocateOutputBuffer

  ! Parameters for the output NetCDF file
  character(len=64), parameter     :: summaVersion = ''
  character(len=64), parameter     :: buildTime = ''
  character(len=64), parameter     :: gitBranch = ''
  character(len=64), parameter     :: gitHash = ''

  type(summa_output_type),allocatable,save,public     :: summa_struct(:)    ! summa_OutputStructure(1)%struc%var(:)%dat(nTimeSteps) 
  type(ilength),allocatable,save,public               :: outputTimeStep(:)  ! timestep in output files


  contains

subroutine f_defOutput(handle_ncid, start_gru, num_gru, num_hru, file_gru, &
    use_extention, file_extention_c, err, message_r) bind(C, name="f_defOutput")
  USE data_types,only: var_i
  USE var_lookup,only:maxvarFreq                      ! maximum number of output files
  
  USE cppwrap_auxiliary,only:c_f_string               ! Convert C String to Fortran String
  USE C_interface_module,only:f_c_string_ptr          ! convert fortran string to c string
  USE summaFileManager,only:OUTPUT_PATH,OUTPUT_PREFIX ! define output file

  USE globalData,only:fileout,output_fileSuffix
  USE globalData,only:iRunMode,iRunModeFull,iRunModeGRU,iRunModeHRU ! define the running modes
  USE globalData,only:checkHRU
  USE globalData,only:gru_struc
  USE globalData,only:ncid
  USE globalData,only:nGRUrun,nHRUrun
  
  USE def_output_module,only:def_output               ! module to define model output
  implicit none
  ! Dummy Varaibles
  type(c_ptr),intent(in),value           :: handle_ncid
  integer(c_int),intent(in)              :: start_gru
  integer(c_int),intent(in)              :: num_gru
  integer(c_int),intent(in)              :: num_hru
  integer(c_int),intent(in)              :: file_gru
  logical(c_bool),intent(in)             :: use_extention
  character(kind=c_char,len=1),intent(in):: file_extention_c
  integer(c_int),intent(out)             :: err
  type(c_ptr),intent(out)                :: message_r
  ! Local Variables
  type(var_i),pointer                    :: output_ncid
  character(len=128)                     :: fmtGruOutput ! a format string used to write start and end GRU in output file names
  character(len=256)                     :: file_extention
  character(len=256)                     :: message ! error message

  err=0; message="f_defOutput/"
  call f_c_string_ptr(trim(message), message_r)
  call c_f_pointer(handle_ncid, output_ncid)
  call c_f_string(file_extention_c,file_extention, 256)
  file_extention = trim(file_extention)
  
  output_fileSuffix = ''
  if (output_fileSuffix(1:1) /= '_') output_fileSuffix='_'//trim(output_fileSuffix)
  if (output_fileSuffix(len_trim(output_fileSuffix):len_trim(output_fileSuffix)) == '_') output_fileSuffix(len_trim(output_fileSuffix):len_trim(output_fileSuffix)) = ' '
  select case (iRunMode)
    case(iRunModeGRU)
      ! left zero padding for startGRU and endGRU
      if (use_extention) then
        output_fileSuffix = trim(output_fileSuffix)//trim(file_extention)
      endif
      write(fmtGruOutput,"(i0)") ceiling(log10(real(file_gru)+0.1))                      ! maximum width of startGRU and endGRU
      fmtGruOutput = "i"//trim(fmtGruOutput)//"."//trim(fmtGruOutput)                   ! construct the format string for startGRU and endGRU
      fmtGruOutput = "('_G',"//trim(fmtGruOutput)//",'-',"//trim(fmtGruOutput)//")"
      write(output_fileSuffix((len_trim(output_fileSuffix)+1):len(output_fileSuffix)),fmtGruOutput) start_gru,start_gru+num_gru-1
  
    case(iRunModeHRU)
      write(output_fileSuffix((len_trim(output_fileSuffix)+1):len(output_fileSuffix)),"('_H',i0)") checkHRU
  end select

  nGRUrun = num_gru
  nHRUrun = num_hru
  fileout = trim(OUTPUT_PATH)//trim(OUTPUT_PREFIX)//trim("_")//trim(output_fileSuffix)
  ncid(:) = integerMissing
  call def_output(summaVersion, buildTime, gitBranch, gitHash, num_gru, &
                  num_hru, gru_struc(1)%hruInfo(1)%nSoil, fileout, &
                  err,message)
  if(err/=0)then; call f_c_string_ptr(trim(message), message_r); return; endif
  ! allocate space for the output file ID array
  if (.not.allocated(output_ncid%var))then
    allocate(output_ncid%var(maxVarFreq))
    output_ncid%var(:) = integerMissing
  endif
  ! copy ncid
  output_ncid%var(:) = ncid(:)

end subroutine f_defOutput

subroutine f_setChunkSize(chunk_size_in) bind(C, name="f_setChunkSize")
  USE globalData,only:chunksize                       ! chunk size for output file  
  implicit none
  ! Dummy Varaibles
  integer(c_int),intent(inout)              :: chunk_size_in

  if (chunk_size_in > 0 .and. chunk_size_in > chunksize) then 
    chunksize = chunk_size_in
  else
    chunk_size_in = chunksize
  endif 
end subroutine f_setChunkSize

subroutine f_addFailedGru(gru_index) bind(C, name="f_addFailedGru")
  implicit none
  ! Dummy Varaibles
  integer(c_int),intent(in)              :: gru_index
  if (allocated(summa_struct)) then
    summa_struct(1)%failedGrus(gru_index) = .true.
  endif
end subroutine f_addFailedGru

subroutine f_resetFailedGru() bind(C, name="f_resetFailedGru")
  implicit none
  if (allocated(summa_struct)) then
    summa_struct(1)%failedGrus(:) = .false.
  endif
end subroutine f_resetFailedGru

subroutine f_resetOutputTimestep(index_gru) bind(C, name="f_resetOutputTimestep")
  implicit none
  integer(c_int),intent(in)              :: index_gru

  outputTimeStep(index_gru)%dat(:) = 1

end subroutine f_resetOutputTimestep

subroutine f_setFailedGruMissing(start_gru, end_gru) bind(C, name="f_setFailedGruMissing")
  USE var_lookup,only:maxvarFreq                ! number of output frequencies
  USE var_lookup,only:iLookVarType              ! named variables for structure elements
  USE var_lookup,only:iLookStat                 ! index into stat structure

  USE globalData,only:realMissing
  USE globalData,only:integerMissing
  USE globalData,only:outFreq                   ! output file information
  USE globalData,only:gru_struc                 ! gru structure
  ! Meta Structures
  USE globalData,only:time_meta                 ! metadata on the model time
  USE globalData,only:forc_meta                 ! metadata on the model forcing data
  USE globalData,only:diag_meta                 ! metadata on the model diagnostic variables
  USE globalData,only:prog_meta                 ! metadata on the model prognostic variables
  USE globalData,only:flux_meta                 ! metadata on the model fluxes
  USE globalData,only:indx_meta                 ! metadata on the model index variables
  USE globalData,only:bvar_meta                 ! metadata on basin-average variables
  USE globalData,only:bpar_meta                 ! basin parameter metadata structure
  USE globalData,only:mpar_meta                 ! local parameter metadata structure
  ! index of the child data structure
  USE globalData,only:forcChild_map             ! index of the child data structure: stats forc
  USE globalData,only:progChild_map             ! index of the child data structure: stats prog
  USE globalData,only:diagChild_map             ! index of the child data structure: stats diag
  USE globalData,only:fluxChild_map             ! index of the child data structure: stats flux
  USE globalData,only:indxChild_map             ! index of the child data structure: stats indx
  USE globalData,only:bvarChild_map             ! index of the child data structure: stats bvar
  implicit none
  ! Dummy Varaibles
  integer(c_int),intent(in)              :: start_gru
  integer(c_int),intent(in)              :: end_gru
  ! local variables
  integer(i4b)                           :: iGRU
  integer(i4b)                           :: iHRU
  integer(i4b)                           :: iFreq
  integer(i4b)                           :: iVar      
  integer(i4b)                           :: iStat
  integer(i4b)                           :: iStep       

  if (.not.allocated(summa_struct)) then; return; endif

  do iGRU = start_gru, end_gru
    if (summa_struct(1)%failedGrus(iGRU)) then
      do iFreq=1, maxVarFreq
        if(.not. outFreq(iFreq)) cycle
        ! forc
        do iVar=1, size(forc_meta)
          if (forc_meta(iVar)%varName == 'time') then
            do iHRU=1, gru_struc(iGRU)%hruCount
              summa_struct(1)%forcStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(:) = realMissing
            end do
            cycle
          endif

          iStat = forc_meta(iVar)%statIndex(iFreq)
          if (iStat==integerMissing.or.trim(forc_meta(iVar)%varName)=='unknown') cycle

          do iHRU=1, gru_struc(iGRU)%hruCount
            if(forc_meta(iVar)%varType==iLookVarType%scalarv) then
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%forcStat%gru(iGRU)%hru(iHRU)%var(forcChild_map(iVar))%tim(iStep)%dat(iFreq) = realMissing
              end do ! iStep
            else ! vector
              summa_struct(1)%forcStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(:) = realMissing
            endif 
          end do ! iHRU
        end do ! ivar

        ! prog
        do iVar = 1, size(prog_meta)
          iStat = prog_meta(iVar)%statIndex(iFreq)
          if (iStat==integerMissing.or.trim(prog_meta(iVar)%varName)=='unknown') cycle
          
          do iHRU=1, gru_struc(iGRU)%hruCount
            if (prog_meta(iVar)%varType==iLookVarType%scalarv) then
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%progStat%gru(iGRU)%hru(iHRU)%var(progChild_map(iVar))%tim(iStep)%dat(iFreq) = realMissing
              end do ! iStep
            else ! vector
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%progStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iStep)%dat(:) = realMissing
              end do ! iStep
            endif
          end do ! iHRU
        end do ! iVar

        ! diag
        do iVar = 1, size(diag_meta)
          iStat = diag_meta(iVar)%statIndex(iFreq)
          if (iStat==integerMissing.or.trim(diag_meta(iVar)%varName)=='unknown') cycle
          do iHRU=1, gru_struc(iGRU)%hruCount
            if (diag_meta(iVar)%varType==iLookVarType%scalarv) then
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%diagStat%gru(iGRU)%hru(iHRU)%var(diagChild_map(iVar))%tim(iStep)%dat(iFreq) = realMissing
              end do ! iStep
            else ! vector
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%diagStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iStep)%dat(:) = realMissing
              end do ! iStep
            endif
          end do ! iHRU
        end do ! iVar

        ! flux
        do iVar = 1, size(flux_meta)
          iStat = flux_meta(iVar)%statIndex(iFreq)
          if (iStat==integerMissing.or.trim(flux_meta(iVar)%varName)=='unknown') cycle
          do iHRU=1, gru_struc(iGRU)%hruCount
            if (flux_meta(iVar)%varType==iLookVarType%scalarv) then
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%fluxStat%gru(iGRU)%hru(iHRU)%var(fluxChild_map(iVar))%tim(iStep)%dat(iFreq) = realMissing
              end do ! iStep
            else ! vector
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%fluxStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iStep)%dat(:) = realMissing
              end do ! iStep
            endif
          end do ! iHRU
        end do ! iVar

        ! indx
        do iVar = 1, size(indx_meta)
          iStat = indx_meta(iVar)%statIndex(iFreq)
          if (iStat==integerMissing.or.trim(indx_meta(iVar)%varName)=='unknown') cycle
          do iHRU=1, gru_struc(iGRU)%hruCount
            if (indx_meta(iVar)%varType==iLookVarType%scalarv) then
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%indxStat%gru(iGRU)%hru(iHRU)%var(indxChild_map(iVar))%tim(iStep)%dat(iFreq) = realMissing
              end do ! iStep
            else ! vector
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%indxStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iStep)%dat(:) = integerMissing
              end do ! iStep
            endif
          end do ! iHRU
        end do ! iVar

        ! bvar
        do iVar = 1, size(bvar_meta)
          iStat = bvar_meta(iVar)%statIndex(iFreq)
          if (iStat==integerMissing.or.trim(bvar_meta(iVar)%varName)=='unknown') cycle
          do iHRU=1, gru_struc(iGRU)%hruCount
            if (bvar_meta(iVar)%varType==iLookVarType%scalarv) then
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%bvarStat%gru(iGRU)%hru(iHRU)%var(bvarChild_map(iVar))%tim(iStep)%dat(iFreq) = realMissing
              end do ! iStep
            else ! vector
              do iStep=1, summa_struct(1)%nTimeSteps
                summa_struct(1)%bvarStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(iStep)%dat(:) = realMissing
              end do ! iStep
            endif
          end do ! iHRU
        end do ! iVar

        ! time
        do iVar=1,size(time_meta)
          if (time_meta(iVar)%statIndex(iFreq)/=iLookStat%inst) cycle
          do iHRU=1, gru_struc(iGRU)%hruCount
            summa_struct(1)%timeStruct%gru(iGRU)%hru(iHRU)%var(iVar)%tim(:) = realMissing
          end do ! iHRU
        end do ! iVar
      end do ! iFreq
    
    end if ! failed gru
  end do ! iGRU


end subroutine

subroutine f_allocateOutputBuffer(max_steps, num_gru, err, message_r) &
    bind(C, name="f_allocateOutputBuffer")
  USE C_interface_module,only:f_c_string_ptr          ! convert fortran string to c string
  USE var_lookup,only:maxvarFreq                      ! maximum number of output files
  implicit none
  ! Dummy Variables
  integer(c_int),intent(in)              :: max_steps
  integer(c_int),intent(in)              :: num_gru
  integer(c_int),intent(out)             :: err
  type(c_ptr),intent(out)                :: message_r
  ! Local Variables
  integer(c_int)                         :: iGRU
  character(len=256)                     :: message ! error message

  err=0; message="f_allocateOutputBuffer/"
  call f_c_string_ptr(trim(message), message_r)

  ! ****************************************************************************
  ! *** Initialize output time step
  ! ****************************************************************************
  if (.not.allocated(outputTimeStep)) then
    allocate(outputTimeStep(num_gru), stat=err)
    do iGRU = 1, num_gru
      allocate(outputTimeStep(iGRU)%dat(maxVarFreq), stat=err)
      outputTimeStep(iGRU)%dat(:) = 1
    end do
  end if
  ! if (err /= 0) call f_c_string_ptr(trim(message), message_r); return;


  ! ****************************************************************************
  ! *** Initialize output structure
  ! ****************************************************************************
  allocate(summa_struct(1))
  ! Statistics Structures
  allocate(summa_struct(1)%forcStat%gru(num_gru))
  allocate(summa_struct(1)%progStat%gru(num_gru))
  allocate(summa_struct(1)%diagStat%gru(num_gru))
  allocate(summa_struct(1)%fluxStat%gru(num_gru))
  allocate(summa_struct(1)%indxStat%gru(num_gru))
  allocate(summa_struct(1)%bvarStat%gru(num_gru))
  ! Primary Data Structures (scalars)
  allocate(summa_struct(1)%timeStruct%gru(num_gru))
  allocate(summa_struct(1)%forcStruct%gru(num_gru))
  allocate(summa_struct(1)%attrStruct%gru(num_gru))
  allocate(summa_struct(1)%typeStruct%gru(num_gru))
  allocate(summa_struct(1)%idStruct%gru(num_gru))
  ! Primary Data Structures (variable length vectors)
  allocate(summa_struct(1)%indxStruct%gru(num_gru))
  allocate(summa_struct(1)%mparStruct%gru(num_gru))
  allocate(summa_struct(1)%progStruct%gru(num_gru))
  allocate(summa_struct(1)%diagStruct%gru(num_gru))
  allocate(summa_struct(1)%fluxStruct%gru(num_gru))
  ! Basin-Average structures
  allocate(summa_struct(1)%bvarStruct%gru(num_gru))
  allocate(summa_struct(1)%bparStruct%gru(num_gru))
  allocate(summa_struct(1)%dparStruct%gru(num_gru))
  ! Finalize Stats for writing
  allocate(summa_struct(1)%finalizeStats%gru(num_gru))
  ! Extras
  allocate(summa_struct(1)%upArea%gru(num_gru))
  allocate(summa_struct(1)%failedGrus(num_gru))
  summa_struct(1)%failedGrus(:) = .false.
  summa_struct(1)%nTimeSteps = max_steps

end subroutine f_allocateOutputBuffer

subroutine f_deallocateOutputBuffer(handle_ncid) &
    bind(C, name="f_deallocateOutputBuffer")
  USE netcdf_util_module,only:nc_file_close 
  USE var_lookup,only:maxvarFreq
  implicit none
  ! Dummy Variables
  type(c_ptr),intent(in),value           :: handle_ncid
  ! Local Variables
  type(var_i),pointer                    :: output_ncid
  integer(c_int)                         :: iFreq
  character(LEN=256)                     :: message
  integer(i4b)                           :: err


  call c_f_pointer(handle_ncid, output_ncid)
  
  do iFreq = 1, maxVarFreq
    if (output_ncid%var(iFreq) /= integerMissing) then
      call nc_file_close(output_ncid%var(iFreq), err, message)
    end if
  end do

  deallocate(summa_struct)
  deallocate(outputTimeStep)
end subroutine


end module output_buffer