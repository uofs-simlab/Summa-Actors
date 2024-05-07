module file_access_actor


  !======= Inclusions ===========
  USE, intrinsic :: iso_c_binding
  USE nrtype
  USE data_types
  USE actor_data_types
  USE globalData
  USE globalData,only:integerMissing      ! missing integer value
  USE globalData,only:realMissing         ! missing double precision value



  implicit none
  public::fileAccessActor_init_fortran
  public::defOutputFortran
  public::FileAccessActor_DeallocateStructures

  character(len=64), parameter     :: summaVersion = ''
  character(len=64), parameter     :: buildTime = ''
  character(len=64), parameter     :: gitBranch = ''
  character(len=64), parameter     :: gitHash = ''
  
  contains

! Call the fortran routines that read data in and are associtated with the forcing structure
subroutine fileAccessActor_init_fortran(num_timesteps,& 
    num_timesteps_output_buffer, num_gru, err, message_r) &
    bind(C, name="fileAccessActor_init_fortran")
  USE output_structure_module,only:initOutputStructure        ! module to initialize output structure
  USE output_structure_module,only:initOutputTimeStep         ! module to initialize output timestep structure (tracks GRUs timestep for output)
  USE C_interface_module,only:f_c_string_ptr  ! convert fortran string to c string
  implicit none
  ! Dummy Variables
  integer(c_int),intent(out)             :: num_timesteps
  integer(c_int),intent(in)              :: num_timesteps_output_buffer
  integer(c_int),intent(out)             :: num_gru
  integer(c_int),intent(out)             :: err
  type(c_ptr),intent(out)                :: message_r
  ! local Variables
  character(len=256)                     :: message            ! error message for downwind routine


  err=0; message="fileAccessActor_init_fortran/"
  call f_c_string_ptr(trim(message), message_r)

  ! TODO: This can be moved to a simple getter the file_access_actor calls
  num_timesteps = numtim ! Returns to the file_access_actor

  ! *****************************************************************************
  ! *** Initialize output structure
  ! *****************************************************************************
  call initOutputStructure(num_timesteps_output_buffer, num_gru, err)
  if(err/=0)then; call f_c_string_ptr(trim(message), message_r); return; endif

  ! *****************************************************************************
  ! *** Initialize output time step
  ! *****************************************************************************
  call initOutputTimeStep(num_gru, err)
  if(err/=0)then; call f_c_string_ptr(trim(message), message_r); return; endif

end subroutine fileAccessActor_init_fortran

subroutine defOutputFortran(handle_output_ncid, start_gru, num_gru, num_hru, &
    file_gru, use_extention, file_extention_c, err) bind(C, name="defOutputFortran")
  USE globalData,only:nGRUrun,nHRUrun
  USE globalData,only:fileout,output_fileSuffix
  USE globalData,only:ncid
  USE globalData,only:integerMissing
  USE globalData,only:iRunMode,iRunModeFull,iRunModeGRU,iRunModeHRU ! define the running modes
  USE summaFileManager,only:OUTPUT_PATH,OUTPUT_PREFIX ! define output file
  USE var_lookup,only:maxvarFreq ! maximum number of output files
  USE def_output_module,only:def_output ! module to define model output
  USE cppwrap_auxiliary,only:c_f_string           ! Convert C String to Fortran String
  
  implicit none

  ! Dummy Variables
  type(c_ptr),intent(in), value          :: handle_output_ncid
  integer(c_int),intent(in)              :: start_gru
  integer(c_int),intent(in)              :: num_gru
  integer(c_int),intent(in)              :: num_hru
  integer(c_int),intent(in)              :: file_gru
  logical(c_bool),intent(in)             :: use_extention
  character(kind=c_char,len=1),intent(in):: file_extention_c
  integer(c_int),intent(out)             :: err
  ! Local Variables
  type(var_i),pointer                    :: output_ncid
  character(len=128)                     :: fmtGruOutput ! a format string used to write start and end GRU in output file names
  character(len=256)                     :: file_extention
  character(len=256)                     :: message ! error message


  call c_f_pointer(handle_output_ncid, output_ncid)
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
  fileout = trim(OUTPUT_PATH)//trim(OUTPUT_PREFIX)//trim(output_fileSuffix)
  ncid(:) = integerMissing
  call def_output(summaVersion,                  &
                  buildTime,                     &
                  gitBranch,                     &
                  gitHash,                       &
                  num_gru,                       &
                  num_hru,                       &
                  gru_struc(1)%hruInfo(1)%nSoil, &
                  fileout,                       &
                  err,message)
  if(err/=0)then; print*,trim(message); return; endif
  ! allocate space for the output file ID array
  if (.not.allocated(output_ncid%var))then
    allocate(output_ncid%var(maxVarFreq))
    output_ncid%var(:) = integerMissing
  endif
  ! copy ncid
  output_ncid%var(:) = ncid(:)


end subroutine defOutputFortran




subroutine FileAccessActor_DeallocateStructures(handle_ncid) bind(C,name="FileAccessActor_DeallocateStructures")
  USE netcdf_util_module,only:nc_file_close 
  USE globalData,only:structInfo                              ! information on the data structures
  USE output_structure_module,only:outputTimeStep
  USE output_structure_module,only:summa_struct
  USE var_lookup,only:maxvarFreq                ! maximum number of output files
   USE globalData,only:index_map 
  implicit none
  type(c_ptr),intent(in), value        :: handle_ncid

  type(var_i),pointer                  :: ncid
  integer(i4b)                         :: iFreq
  character(LEN=256)                   :: cmessage
  character(LEN=256)                   :: message
  integer(i4b)                         :: err

  call c_f_pointer(handle_ncid, ncid)
  ! close the open output FIle
  do iFreq=1,maxvarFreq
    if (ncid%var(iFreq)/=integerMissing) then
      call nc_file_close(ncid%var(iFreq),err,cmessage)
      if(err/=0)then; message=trim(message)//trim(cmessage); return; end if
    endif   
  end do

  deallocate(ncid)
  deallocate(outputTimeStep)
  deallocate(summa_struct)
  deallocate(index_map)
end subroutine FileAccessActor_DeallocateStructures

end module file_access_actor
