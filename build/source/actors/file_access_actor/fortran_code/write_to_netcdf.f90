module write_to_netcdf_module
USE, intrinsic :: iso_c_binding
USE nrtype
USE data_types

!TODO: This module is only used for writeParamToNetCDF the others are not.


implicit none
public::writeParamToNetCDF
public::writeDataToNetCDF
public::writeBasinToNetCDF
public::writeTimeToNetCDF
public::writeGRUStatistics

contains

! Subroutine that writes data from the HRU actor to be written to netcdf
subroutine writeParamToNetCDF(handle_ncid,    &
                          index_gru,          &
                          index_hru,          &
                          handle_attr_struct, &
                          handle_type_struct, &
                          handle_mpar_struct, &
                          handle_bpar_struct, &
                          err) bind(C, name="writeParamToNetCDF")
  USE globalData,only:attr_meta,type_meta,mpar_meta,bpar_meta ! meta structures
  USE globalData,only:gru_struc
  USE writeOutput_module,only:writeParm 
  USE globalData,only:structInfo                              ! information on the data structures
  implicit none
  ! dummy variables
  type(c_ptr),   intent(in), value  :: handle_ncid        ! ncid of the output file
  integer(c_int),intent(in)         :: index_gru          ! index of GRU in gru_struc
  integer(c_int),intent(in)         :: index_hru          ! index of HRU in gru_struc
  type(c_ptr),   intent(in), value  :: handle_attr_struct
  type(c_ptr),   intent(in), value  :: handle_type_struct
  type(c_ptr),   intent(in), value  :: handle_mpar_struct
  type(c_ptr),   intent(in), value  :: handle_bpar_struct 
  integer(c_int),intent(out)        :: err 
  ! local variables pointers
  type(var_i), pointer              :: ncid
  type(var_d), pointer              :: attr_struct
  type(var_i), pointer              :: type_struct
  type(var_dlength), pointer        :: mpar_struct
  type(var_d),pointer               :: bpar_struct
  ! local variables
  integer(i4b)                      :: iStruct
  character(LEN=256)                :: cmessage
  character(LEN=256)                :: message
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  message="file_access_actor.f90 - writeParamToNetCDF"
  call c_f_pointer(handle_ncid, ncid)
  call c_f_pointer(handle_attr_struct, attr_struct)
  call c_f_pointer(handle_type_struct, type_struct)
  call c_f_pointer(handle_mpar_struct, mpar_struct)
  call c_f_pointer(handle_bpar_struct, bpar_struct)

  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('attr'); call writeParm(ncid,gru_struc(index_gru)%hruInfo(index_hru)%hru_ix, &
        attr_struct,attr_meta,err,cmessage)
      case('type'); call writeParm(ncid,gru_struc(index_gru)%hruInfo(index_hru)%hru_ix, &
        type_struct,type_meta,err,cmessage)
      case('mpar'); call writeParm(ncid,gru_struc(index_gru)%hruInfo(index_hru)%hru_ix, &
        mpar_struct,mpar_meta,err,cmessage)
    end select
    if(err/=0)then
      message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
      print*, message
      return
    endif
  end do

  ! write GRU parameters
  call writeParm(ncid,index_gru,bpar_struct,bpar_meta,err,cmessage)
  if(err/=0)then
    message=trim(message)//trim(cmessage)
    print*, message
    return
  endif

end subroutine writeParamToNetCDF

subroutine writeDataToNetCDF(handle_ncid,          &
                             index_gru,             &
                             index_hru,             &
                             handle_finalize_stats, & 
                             handle_forc_stat,      &
                             handle_forc_struct,    &
                             handle_prog_stat,      &
                             handle_prog_struct,    &
                             handle_diag_stat,      &
                             handle_diag_struct,    &
                             handle_flux_stat,      &
                             handle_flux_struct,    &
                             handle_indx_stat,      &
                             handle_indx_struct,    &
                             handle_output_timestep,&
                             err) bind(C, name="writeDataToNetCDF")
  USE globalData, only:forc_meta, prog_meta, diag_meta, flux_meta, indx_meta
  USE globalData, only:forcChild_map, progChild_map, diagChild_map, fluxChild_map, indxChild_map
  USE globalData,only:maxLayers                               ! maximum number of layers
  USE globalData,only:structInfo
  USE writeOutput_module,only:writeParm 
  USE writeOutput_module,only:writeData

  implicit none
  ! dummy variables
  type(c_ptr),    intent(in), value  :: handle_ncid
  integer(c_int), intent(in)         :: index_gru
  integer(c_int), intent(in)         :: index_hru
  type(c_ptr),    intent(in), value  :: handle_finalize_stats
  type(c_ptr),    intent(in), value  :: handle_forc_stat
  type(c_ptr),    intent(in), value  :: handle_forc_struct
  type(c_ptr),    intent(in), value  :: handle_prog_stat
  type(c_ptr),    intent(in), value  :: handle_prog_struct
  type(c_ptr),    intent(in), value  :: handle_diag_stat
  type(c_ptr),    intent(in), value  :: handle_diag_struct
  type(c_ptr),    intent(in), value  :: handle_flux_stat
  type(c_ptr),    intent(in), value  :: handle_flux_struct
  type(c_ptr),    intent(in), value  :: handle_indx_stat
  type(c_ptr),    intent(in), value  :: handle_indx_struct
  type(c_ptr),    intent(in), value  :: handle_output_timestep
  integer(c_int), intent(out)        :: err
  ! local pointers
  type(var_i), pointer               :: ncid
  type(flagVec), pointer             :: finalize_stats
  type(var_dlength),pointer          :: forc_stat 
  type(var_d),pointer                :: forc_struct 
  type(var_dlength),pointer          :: prog_stat
  type(var_dlength),pointer          :: prog_struct 
  type(var_dlength),pointer          :: diag_stat
  type(var_dlength),pointer          :: diag_struct 
  type(var_dlength),pointer          :: flux_stat 
  type(var_dlength),pointer          :: flux_struct 
  type(var_dlength),pointer          :: indx_stat 
  type(var_ilength),pointer          :: indx_struct 
  type(var_i),pointer                :: output_timestep
  ! local_variables
  integer(i4b)                       :: numGRU=1
  integer(i4b)                       :: nSteps=1
  integer(i4b)                       :: iStruct
  character(LEN=256)                 :: cmessage
  character(LEN=256)                 :: message
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_ncid, ncid)
  call c_f_pointer(handle_finalize_stats, finalize_stats)
  call c_f_pointer(handle_forc_stat, forc_stat)
  call c_f_pointer(handle_forc_struct, forc_struct)
  call c_f_pointer(handle_prog_stat, prog_stat)
  call c_f_pointer(handle_prog_struct, prog_struct)
  call c_f_pointer(handle_diag_stat, diag_stat)
  call c_f_pointer(handle_diag_struct, diag_struct)
  call c_f_pointer(handle_flux_stat, flux_stat)
  call c_f_pointer(handle_flux_struct, flux_struct)
  call c_f_pointer(handle_indx_stat, indx_stat)
  call c_f_pointer(handle_indx_struct, indx_struct)
  call c_f_pointer(handle_output_timestep, output_timestep)

  message="file_access_actor.f90 - writeDataToNetCDF"
  do iStruct=1,size(structInfo)
    select case(trim(structInfo(iStruct)%structName))
      case('forc')
        call writeData(ncid, finalize_stats%dat(:), output_timestep%var(:),maxLayers,&
                      index_gru, numGRU, & 
                      forc_meta,forc_stat,forc_struct,'forc', &
                      forcChild_map,indx_struct,err,cmessage)
      case('prog')
        call writeData(ncid, finalize_stats%dat(:), output_timestep%var(:),maxLayers,&
                      index_gru, numGRU, &
                      prog_meta,prog_stat,prog_struct,'prog', &
                      progChild_map,indx_struct,err,cmessage)
      case('diag')
        call writeData(ncid, finalize_stats%dat(:), output_timestep%var(:),maxLayers,&
                      index_gru, numGRU, &
                      diag_meta, diag_stat, diag_struct,'diag', &
                      diagChild_map,indx_struct,err,cmessage)
      case('flux')
        call writeData(ncid, finalize_stats%dat(:), output_timestep%var(:),maxLayers,&
                      index_gru, numGRU, &
                      flux_meta,flux_stat,flux_struct,'flux', &
                      fluxChild_map,indx_struct,err,cmessage)
      case('indx')
        call writeData(ncid, finalize_stats%dat(:), output_timestep%var(:),maxLayers,&
                      index_gru, numGRU, &
                      indx_meta,indx_stat, indx_struct, 'indx', &
                      indxChild_map,indx_struct,err,cmessage)
    end select
    if(err/=0)then
      message=trim(message)//trim(cmessage)//'['//trim(structInfo(iStruct)%structName)//']'
      print*, message
      return
    endif
  end do  ! (looping through structures)
end subroutine writeDataToNetCDF

! subroutine setOutputStructure(index_gru, index_timestep, 
!   handle_finalize_stats, handle_output_timestep, handle_output_timestep, )

! end subroutine setOutputStructure

subroutine writeBasinToNetCDF(handle_ncid, index_gru, handle_finalize_stats, &
  handle_output_timestep, handle_bvar_stat, handle_bvar_struct, err) bind(C, name="writeBasinToNetCDF")
  USE writeOutput_module,only:writeBasin
  USE globalData,only:bvar_meta                 ! metadata on basin-average variables
  USE globalData,only:bvarChild_map             ! index of the child data structure: stats bvar
 
  implicit none
  ! dummy variables
  type(c_ptr),    intent(in), value  :: handle_ncid
  integer(c_int), intent(in)         :: index_gru
  type(c_ptr),    intent(in), value  :: handle_finalize_stats
  type(c_ptr),    intent(in), value  :: handle_output_timestep
  type(c_ptr),    intent(in), value  :: handle_bvar_stat
  type(c_ptr),    intent(in), value  :: handle_bvar_struct
  integer(c_int), intent(out)        :: err
  ! local pointers for dummy variables
  type(var_i), pointer               :: ncid
  type(flagVec), pointer             :: finalize_stats
  type(var_i),pointer                :: output_timestep
  type(var_dlength),pointer          :: bvar_stat
  type(var_dlength),pointer          :: bvar_struct
  ! local Variables
  character(len=256)                 :: message
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  ! ---------------------------------------------------------------------------------------
  call c_f_pointer(handle_ncid, ncid)
  call c_f_pointer(handle_finalize_stats, finalize_stats)
  call c_f_pointer(handle_output_timestep, output_timestep)
  call c_f_pointer(handle_bvar_stat, bvar_stat)
  call c_f_pointer(handle_bvar_struct, bvar_struct)
  message="file_access_actor.f90 - writeBasinToNetCDF"


  call writeBasin(ncid,index_gru,finalize_stats%dat(:),output_timestep%var(:),&
      bvar_meta, bvar_stat%var, bvar_struct%var, bvarChild_map, err, message)
  if(err/=0)then 
    message=trim(message)//'[bvar]'
    print*, message
    return
  endif 

end subroutine writeBasinToNetCDF

subroutine writeTimeToNetCDF(handle_ncid, handle_finalize_stats, handle_output_timestep, &
    handle_time_struct, err) bind(C, name="writeTimeToNetCDF")
  USE writeOutput_module,only:writeTime
  USE globalData,only:time_meta

  implicit none
  type(c_ptr), intent(in), value :: handle_ncid
  type(c_ptr), intent(in), value :: handle_finalize_stats
  type(c_ptr), intent(in), value :: handle_output_timestep
  type(c_ptr), intent(in), value :: handle_time_struct
  integer(c_int), intent(out)    :: err

  type(var_i), pointer           :: ncid
  type(flagVec), pointer         :: finalize_stats
  type(var_i),pointer            :: output_timestep
  type(var_i),pointer            :: time_struct
  character(len=256)             :: message

  call c_f_pointer(handle_ncid, ncid)
  call c_f_pointer(handle_finalize_stats, finalize_stats)
  call c_f_pointer(handle_output_timestep, output_timestep)
  call c_f_pointer(handle_time_struct, time_struct)
  message="file_access_actor.f90 - writeTimeToNetCDF"

  call writeTime(ncid, finalize_stats%dat(:),output_timestep%var(:),&
      time_meta, time_struct%var,err,message)
  if(err/=0)then 
    message=trim(message)//'writeTime'
    print*, message
    return
  endif 

end subroutine writeTimeToNetCDF

subroutine writeGRUStatistics(handle_ncid,      &
                              gru_var_ids,      &
                              gru_stats_vector, &
                              num_gru,          &
                              err) bind(C, name="WriteGRUStatistics")
  USE data_types,only:var_i,netcdf_gru_actor_info,serializable_netcdf_gru_actor_info
  USE var_lookup, only: maxvarFreq ! number of output frequencies
  USE netcdf
  implicit none
  ! Dummy Variables
  type(c_ptr), intent(in), value                      :: handle_ncid
  type(netcdf_gru_actor_info),intent(in)              :: gru_var_ids
  type(serializable_netcdf_gru_actor_info),intent(in) :: gru_stats_vector(num_gru)
  integer(c_int), intent(in)                          :: num_gru
  integer(c_int), intent(out)                         :: err
  
  ! Local Variables
  type(var_i), pointer                                :: ncid
  real(c_double), dimension(num_gru)                  :: run_time_array
  real(c_double), dimension(num_gru)                  :: init_time_array
  real(c_double), dimension(num_gru)                  :: forcing_time_array
  real(c_double), dimension(num_gru)                  :: run_physics_time_array
  real(c_double), dimension(num_gru)                  :: write_output_time_array

  integer(c_int), dimension(num_gru)                  :: successful_array
  integer(c_int), dimension(num_gru)                  :: num_attempts_array

  integer(c_int)                                      :: i
  integer(c_int)                                      :: iFreq         
  ! ---------------------------------------------------------------------------------------
  ! * Convert From C++ to Fortran
  call c_f_pointer(handle_ncid, ncid)

  ! Assemble fortran arrays
  do i=1,num_gru
    run_time_array(i) = gru_stats_vector(i)%run_time
    init_time_array(i) = gru_stats_vector(i)%init_duration
    forcing_time_array(i) = gru_stats_vector(i)%forcing_duration
    run_physics_time_array(i) = gru_stats_vector(i)%run_physics_duration
    write_output_time_array(i) = gru_stats_vector(i)%write_output_duration
    successful_array(i) = gru_stats_vector(i)%successful
    num_attempts_array(i) = gru_stats_vector(i)%num_attempts
  end do

  ! Write to NetCDF
  do iFreq=1, maxvarFreq
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%run_time_var_id, run_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%init_duration_var_id, init_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%forcing_duration_var_id, forcing_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%run_physics_duration_var_id, run_physics_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%write_output_duration_var_id, write_output_time_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%state_var_id, successful_array)
    err = nf90_put_var(ncid%var(iFreq), gru_var_ids%num_attempts_var_id, num_attempts_array)
  end do

end subroutine writeGRUStatistics


end module write_to_netcdf_module