#pragma once
#include "caf/all.hpp"
#include "settings_functions.hpp"
#include "unordered_map"



class GruBatchActor {
  caf::event_based_actor* self_;
  int netcdf_start_index_;
  int job_start_index_;
  int num_gru_;
  int num_steps_;

  HRUActorSettings hru_actor_settings_;
  caf::actor file_access_actor_;
  caf::actor parent_;

  std::vector<caf::actor> gru_actors_;
  int num_gru_done_ = 0;

  public:
    GruBatchActor(caf::event_based_actor* self, int netcdf_start_index, 
                  int job_start_index, int num_gru, int num_steps,
                  HRUActorSettings hru_actor_settings, 
                  caf::actor file_access_actor, caf::actor parent) 
      : self_(self), netcdf_start_index_(netcdf_start_index), 
        job_start_index_(job_start_index), num_gru_(num_gru), 
        num_steps_(num_steps), hru_actor_settings_(hru_actor_settings), 
        file_access_actor_(file_access_actor), parent_(parent) {
    }

    caf::behavior make_behavior();
};


// struct hru_batch_state {
//   // Actor References
// 	caf::actor file_access_actor;
// 	caf::actor parent;

//   HRU_Actor_Settings hru_actor_settings;
//   std::vector<caf::actor> hru_actors;

//   std::unordered_map<caf::actor, double> hru_actor_walltimes;

//   std::vector<double> hru_walltimes;

//   int num_done = 0;

// };

// behavior hru_batch_actor(stateful_actor<hru_batch_state>* self, 
//     int start_gru_local, int start_gru_global, int num_gru, 
//     HRU_Actor_Settings hru_actor_settings, caf::actor file_access_actor, 
//     caf::actor parent);

