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
// #include "summa_init_struc.hpp"

/*********************************************
 * File Access Actor Fortran Functions
 *********************************************/
extern "C" {
  void fileAccessActor_init_fortran(int* num_timesteps, 
                                    int* num_timesteps_output_buffer, 
                                    int* numGRU, int* err, void* message);

  void defOutputFortran(void* handle_ncid, int* start_gru, int* num_gru, 
                        int* num_hru, int* file_gru, bool* use_extention, 
                        char const* output_extention, int* err); 

  void writeOutput_fortran(void* handle_ncid, int* num_steps, int* start_gru, 
                          int* max_gru, bool* writeParamFlag, int* err);

  void writeRestart_fortran(void* handle_ncid, int* start_gru, int* max_gru, 
                            int* timestep, int* year, int* month, int* day, 
                            int* hour, int* err); 

  void FileAccessActor_DeallocateStructures(void* handle_ncid);
}

/*********************************************
 * File Access Actor state variables
 *********************************************/
struct file_access_state {
  TimingInfo file_access_timing;
  caf::actor parent; 
  int start_gru;
  int num_gru;
  int num_hru;

  NumGRUInfo num_gru_info;

  void *handle_ncid = new_handle_var_i();  // output file ids
  int num_steps;
  int num_output_steps;
  int err = 0; // this is to make compiler happy

  Output_Container* output_container;

  File_Access_Actor_Settings file_access_actor_settings;

  // std::unique_ptr<SummaInitStruc> summa_init_struc;
  std::unique_ptr<forcingFileContainer> forcing_files;

  bool write_params_flag = true;

  // Checkpointing variables
  int completed_checkpoints = 1;  // 
  std::vector<int> hru_checkpoints;
  std::vector<int> hru_timesteps;

};

// called to spawn a file_access_actor
caf::behavior file_access_actor(caf::stateful_actor<file_access_state>* self, 
                                NumGRUInfo num_gru_info,
                                File_Access_Actor_Settings file_access_actor_settings, 
                                caf::actor parent);


/*********************************************
 * Functions for the file access actor
 *********************************************/
/* Setup and call the fortran routine that writes the output */
void writeOutput(caf::stateful_actor<file_access_state>* self, 
                 Output_Partition* partition);

void writeRestart(caf::stateful_actor<file_access_state>* self, 
                  Output_Partition* partition, int start_gru, int num_gru, 
                  int timestep, int year, int month, int day, int hour);

 
