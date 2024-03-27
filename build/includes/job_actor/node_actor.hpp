#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"
#include "message_atoms.hpp"
#include "hru_batch_actor.hpp"
#include "file_access_actor.hpp"
#include "job_actor.hpp"
#include "timing_info.hpp"

namespace caf {

struct node_state {
  actor current_server;
  std::string hostname;
  
  TimingInfo node_timing;

  int start_gru;
  int num_gru_local;

  caf::actor file_access_actor; // actor reference for the file_access_actor
  GRU_Container gru_container;

  Distributed_Settings distributed_settings;
  File_Access_Actor_Settings file_access_actor_settings;
  Job_Actor_Settings job_actor_settings;
  HRU_Actor_Settings hru_actor_settings;

  int max_run_attempts;
  int dt_init_start_factor = 1; // Initial Factor for dt_init (coupled_em)


  // Forcing information
  int iFile = 1; // index of current forcing file from forcing file list
  int stepsInCurrentFFile;
  int forcingStep = 1;
  int timestep = 1;
  int num_gru_done_timestep = 0;
  int num_steps = 0;
};


behavior node_actor(stateful_actor<node_state>* self,
                    std::string host, // server will spawn this actor, if local do not try to connect via port
                    actor parent,
                    Distributed_Settings distributed_settings,
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
