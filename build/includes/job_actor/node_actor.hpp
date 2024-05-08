#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"
#include "message_atoms.hpp"
#include "hru_batch_actor.hpp"
#include "file_access_actor.hpp"
#include "job_actor.hpp"
#include "timing_info.hpp"
#include "unordered_map"


namespace caf {

using chrono_time = std::chrono::time_point<std::chrono::system_clock>;


struct node_state {
  actor current_server;
  std::string hostname;
  
  TimingInfo node_timing;

  int start_gru;
  int num_gru_local;
  int num_gru_global;
  NumGRUInfo num_gru_info;

  chrono_time timestep_start_time;
  chrono_time timestep_end_time;

  caf::actor file_access_actor; // actor reference for the file_access_actor
  GRU_Container gru_container;

  std::unique_ptr<GruStruc> gru_struc;

  Distributed_Settings distributed_settings;
  File_Access_Actor_Settings file_access_actor_settings;
  Job_Actor_Settings job_actor_settings;
  HRU_Actor_Settings hru_actor_settings;

  int max_run_attempts;
  int dt_init_start_factor = 1; // Initial Factor for dt_init (coupled_em)

  std::vector<std::vector<double>> gru_walltimes;
  std::vector<std::pair<caf::actor, double>> hru_actor_walltimes;

  // <hru_actor, hru_batch_actor>
  std::unordered_map<caf::actor, caf::actor> 
      hru_to_batch_map; // Map from HRU to HRU Batch Actor
  std::vector<caf::actor> hru_actor_list;

  std::unordered_map<caf::actor, double> hru_walltimes;
  // Forcing information
  int iFile = 1; // index of current forcing file from forcing file list
  int stepsInCurrentFFile;
  int forcingStep = 1;
  int timestep = 1;
  int num_gru_done_timestep = 0;
  int num_steps = 0;

  int hru_batch_maps_received = 0;
};


behavior node_actor(stateful_actor<node_state>* self, std::string host, 
    actor parent, Distributed_Settings distributed_settings,
    File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, 
    HRU_Actor_Settings hru_actor_settings);

/*********************************************
 * Functions for the Job Actor
 *********************************************/
// Spawn HRU Actors Individually
void spawnHRUActors(stateful_actor<node_state>* self, bool normal_mode);
// Spawn HRU Batch Actors
void spawnHRUBatches(stateful_actor<node_state>* self);

} // namespace caf
