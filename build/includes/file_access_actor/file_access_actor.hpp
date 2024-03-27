#pragma once

#include "caf/actor.hpp"
#include "forcing_file_info.hpp"
#include "timing_info.hpp"
#include "output_container.hpp"
#include "settings_functions.hpp"
#include "fortran_data_types.hpp"
#include "auxilary.hpp"
#include "global.hpp"
#include "message_atoms.hpp"
#include "forcing_file_info.hpp"
#include "json.hpp"


/*********************************************
 * File Access Actor Fortran Functions
 *********************************************/
extern "C" {
  void defOutputFortran(void* handle_ncid, int* start_gru, int* num_gru, 
      int* num_hru, int* file_gru, bool* use_extention, 
      char const* output_extention, int* err); 

  void fileAccessActor_init_fortran(void* handle_forcing_file_info, 
      int* num_forcing_files, int* num_timesteps, 
      int* num_timesteps_output_buffer, void* handle_output_ncid, int* startGRU,
      int* numGRU, int* numHRU, int* err);

  void writeOutput_fortran(void* handle_ncid, int* num_steps, int* start_gru, 
      int* max_gru, bool* writeParamFlag, int* err);

  void read_forcingFile(void* forcFileInfo, int* currentFile, int* stepsInFile,
      int* startGRU, int* numGRU, int* err);

  void FileAccessActor_DeallocateStructures(void* handle_forcFileInfo, 
      void* handle_ncid);

}

/*********************************************
 * File Access Actor state variables
 *********************************************/

namespace caf {
struct file_access_state {
  // Variables set on Spawn
  caf::actor parent; 
  int start_gru;
  int num_gru;

  NumGRUInfo num_gru_info;


  void *handle_forcing_file_info = new_handle_file_info(); // Handle for the forcing file information
  void *handle_ncid = new_handle_var_i();                  // output file ids
  int num_vectors_in_output_manager;
  int num_steps;
  int stepsInCurrentFile;
  int numFiles;
  int filesLoaded;
  int num_output_steps;

  Output_Container* output_container;

  File_Access_Actor_Settings file_access_actor_settings;

  std::vector<Forcing_File_Info> forcing_file_list; // list of steps in file

    // Timing Variables
  TimingInfo file_access_timing;



  bool write_params_flag = true;
};

// called to spawn a file_access_actor
behavior file_access_actor(stateful_actor<file_access_state>* self, 
    NumGRUInfo num_gru_info,
    File_Access_Actor_Settings file_access_actor_settings, actor parent);

/*********************************************
 * Functions for the file access actor
 *********************************************/

/* Setup and call the fortran routine that writes the output */
void writeOutput(stateful_actor<file_access_state>* self, 
    Output_Partition* partition);

 
} // end namespace