#pragma once
#include "file_access_actor.hpp"

extern "C" {
  // initalizeFileAccessActor
  void fileAccessActor_init_fortran(void* handle_forcing_file_info, 
                               int* num_forcing_files, 
                               int* num_timesteps,
                               int* num_timesteps_output_buffer,
                               void* handle_output_ncid, 
                               int* startGRU,
                               int* numGRU, 
                               int* numHRU,
                               netcdf_gru_actor_info* actor_info,
                               int* err);

  // OutputStructure and Output functions
  void deallocateOutputStructure(int* err);
  void writeOutput_fortran(void* handle_ncid, int* num_steps, int* start_gru, int* max_gru, bool* writeParamFlag, int* err);

  void updateFailed(int* indxHRU);

  void resetFailedArray();

  void resetOutputCounter(int* indxGRU);

  void read_forcingFile(void* forcFileInfo, int* currentFile, int* stepsInFile,
         int* startGRU, int* numGRU, int* err);

  void FileAccessActor_DeallocateStructures(void* handle_forcFileInfo, void* handle_ncid);
  

  // Writing to NETCDF
  void writeParamToNetCDF(void* handle_ncid, int* index_gru, int* index_hru,
    void* handle_attr_struct, void* handle_type_struct, void* handle_mpar_struct,
    void* handle_bpar_struct, int* err);

  void writeDataToNetCDF(void* handle_ncid, int* index_gru, int* index_hru,
    void* handle_finalize_stats, void* handle_forc_stat, void* handle_forc_struct,
    void* handle_prog_stat, void* handle_prog_struct, void* handle_diag_stat,
    void* handle_diag_struct, void* handle_flux_stat, void* handle_flux_struct,
    void* handle_indx_stat, void* handle_indx_struct, void* handle_output_timestep,
    int* err);

  void writeBasinToNetCDF(void* handle_ncid, int* index_gru, void* handle_finalize_stats,
    void* handle_output_timestep, void* handle_bvar_stat, void* handle_bvar_struct, int* err);

  void writeTimeToNetCDF(void* handle_ncid, void* handle_finalize_stats, void* handle_output_timestep,
    void* handle_time_struct, int* err);

  void WriteGRUStatistics(void* handle_ncid, netcdf_gru_actor_info* actor_info,
    serializable_netcdf_gru_actor_info* gru_stats_vector, int* num_gru, int* err);

  






  
}
