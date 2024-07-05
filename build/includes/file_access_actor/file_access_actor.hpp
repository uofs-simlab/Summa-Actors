#pragma once

#include "caf/actor.hpp"
#include "output_container.hpp"
#include "output_buffer.hpp"
#include "settings_functions.hpp"
#include "fortran_data_types.hpp"
#include "auxilary.hpp"
#include "message_atoms.hpp"
#include "forcing_file_info.hpp"
#include "json.hpp"

/*********************************************
 * File Access Actor Fortran Functions
 *********************************************/
extern "C" {
  void f_getNumTimeSteps(int& num_timesteps);

  void writeOutput_fortran(void* handle_ncid, int& num_steps, int& start_gru, 
                          int& max_gru, bool& writeParamFlag, int& err,
                          void* message);

  void writeRestart_fortran(void* handle_ncid, int& start_gru, int& max_gru, 
                            int& timestep, int& year, int& month, int& day, 
                            int& hour, int& err); 

  void FileAccessActor_DeallocateStructures(void* handle_ncid);
}

class FileAccessActor {
  caf::event_based_actor* self_;

  TimingInfo timing_info_;
  NumGRUInfo num_gru_info_;
  FileAccessActorSettings fa_settings_;
  caf::actor parent_;

  void *handle_ncid_ = new_handle_var_i();  // output file ids


  int start_gru_;
  int num_gru_;
  int num_hru_;

  int num_steps_;
  int num_output_steps_;
  bool write_params_flag_ = true;
  std::unique_ptr<forcingFileContainer> forcing_files_;
  std::unique_ptr<Output_Container> output_container_;
  std::unique_ptr<OutputBuffer> output_buffer_;
  
  // Checkpointing variables
  int completed_checkpoints_ = 1;   
  std::vector<int> hru_checkpoints_;
  std::vector<int> hru_timesteps_;


  public:
    FileAccessActor(caf::event_based_actor* self, NumGRUInfo num_gru_info, 
                    FileAccessActorSettings fa_settings, caf::actor parent) 
                    : self_(self), num_gru_info_(num_gru_info), 
                      fa_settings_(fa_settings), parent_(parent) {};

    caf::behavior make_behavior();

    void writeOutput(Output_Partition* partition);

    void writeRestart(Output_Partition* partition, int start_gru, int num_gru, 
                      int timestep, int year, int month, int day, int hour);

};







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


  std::unique_ptr<forcingFileContainer> forcing_files;

  bool write_params_flag = true;



};


