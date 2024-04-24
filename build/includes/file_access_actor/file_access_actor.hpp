#pragma once

#include "caf/actor.hpp"
#include "output_container.hpp"
#include "settings_functions.hpp"
#include "fortran_data_types.hpp"
#include "auxilary.hpp"
#include "global.hpp"
#include "message_atoms.hpp"
#include "forcing_file_info.hpp"
#include "json.hpp"

using namespace caf;

/*********************************************
 * File Access Actor Fortran Functions
 *********************************************/
extern "C" {
  void fileAccessActor_init_fortran( int* num_timesteps, 
      int* num_timesteps_output_buffer, void* handle_output_ncid, int* startGRU,
      int* numGRU, int* numHRU, int* err, void* message);

  void defOutputFortran(void* handle_ncid, int* start_gru, int* num_gru, 
      int* num_hru, int* file_gru, bool* use_extention, 
      char const* output_extention, int* err); 

  void writeOutput_fortran(void* handle_ncid, int* num_steps, int* start_gru, 
      int* max_gru, bool* writeParamFlag, int* err);

  void FileAccessActor_DeallocateStructures(void* handle_ncid);

}

/*********************************************
 * File Access Actor state variables
 *********************************************/
struct file_access_state {
  caf::actor parent; 
  int start_gru;
  int num_gru;

  NumGRUInfo num_gru_info;

  void *handle_ncid = new_handle_var_i();  // output file ids
  int num_vectors_in_output_manager;
  int num_steps;
  int stepsInCurrentFile;
  int numFiles;
  int filesLoaded;
  int num_output_steps;

  Output_Container* output_container;

  File_Access_Actor_Settings file_access_actor_settings;

  std::unique_ptr<forcingFileContainer> forcing_files;

  TimingInfo file_access_timing;

  bool write_params_flag = true;
};

// called to spawn a file_access_actor
behavior file_access_actor(stateful_actor<file_access_state>* self, 
    NumGRUInfo num_gru_info,
    File_Access_Actor_Settings file_access_actor_settings, actor parent);


behavior file_access_init(stateful_actor<file_access_state>* self);
/*********************************************
 * Functions for the file access actor
 *********************************************/

/* Setup and call the fortran routine that writes the output */
void writeOutput(stateful_actor<file_access_state>* self, 
    Output_Partition* partition);

 