#pragma once

extern "C" {
  // initalizeFileAccessActor
  void ffile_info(int* indxGRU, void* forcFileInfo, int* numFiles, int* err);
  void mDecisions(int* numSteps, int* err);
  void read_pinit_C(int* err);
  void read_vegitationTables(int* err);
  void initFailedHRUTracker(int* numGRU);
  void def_output(void* handle_ncid, int* startGRU, int* numGRU, int* numHRU, int* err);

  void initOutputStructure(void* handle_forcFileInfo, int* max_steps, int* numGRU, int* err);
  void deallocateOutputStructure(int* err);
  void writeOutput(void* handle_ncid, int* num_steps, int* start_gru, int* max_gru, int* err);
  
  // Attributes Files- called inside initalizeFileAccessActor
  void allocateAttributeStructures(int* index_gru, int* index_hru, void* handle_attr_struct, 
      void* handle_type_struct, void* handle_id_struct, int* err);
  void openAttributeFile(int* att_ncid, int* err);
  void getNumVarAttr(int* attr_ncid, int* num_var_attr, int* err);
  void closeAttributeFile(int* attr_ncid, int* err);
  void readAttributeFromNetCDF(int* attr_ncid, int* index_gru, int* index_hru, int* num_var,
    void* attr_array, void* type_array, void* id_array, int* err);
  
  // Parameters File - called inside initalizeFileAccessActor
  void allocateParamStructures(int* index_gru, int* index_hru, void* handle_dpar_struct,
      void* handle_mpar_struct, void* handle_bpar_struct, int* err);
  void openParamFile(int* param_ncid, bool* param_file_exists, int* err);
  void getNumVarParam(int* param_ncid, int* num_var_param, int* err);
  void closeParamFile(int* param_ncid, int* err);
  void getParamSizes(int* dpar_array_size, int* bpar_array_size, int* type_array_size);
  void overwriteParam(int* index_gru, int* index_hru,
    void* handle_type_struct, void* handle_dpar_struct, void* handle_mpar_struct, 
    void* handle_bpar_struct, int* err);
  void readParamFromNetCDF(int* param_ncid, int* index_gru, int* index_hru, int* start_index_gru,
    int* num_var_param, void* handle_mpar_struct, void* _handle_bpar_struct, int* err);

  // Set up global initial conditions
  void openInitCondFile(int* init_cond_ncid, int* err);
  void closeInitCondFile(int* init_cond_ncid, int* err);
  void readInitCond_prog(int* init_cond_ncid, int* start_gru, int* num_gru, int* err);
  void readInitCond_bvar(int* init_cond_ncid, int* start_gru, int* num_gru, int* err);

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






  
}
