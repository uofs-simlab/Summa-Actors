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
  std::string restart_;

  HRUActorSettings hru_actor_settings_;
  int num_steps_output_buffer_;
  caf::actor file_access_actor_;
  caf::actor parent_;

  std::vector<caf::actor> gru_actors_;
  int num_gru_done_ = 0;

  public:
    GruBatchActor(caf::event_based_actor* self, int netcdf_start_index, 
                  int job_start_index, int num_gru, int num_steps,
                  HRUActorSettings hru_actor_settings, int num_output_steps, 
                  caf::actor file_access_actor, caf::actor parent, std::string restart) 
      : self_(self), netcdf_start_index_(netcdf_start_index), 
        job_start_index_(job_start_index), num_gru_(num_gru), 
        num_steps_(num_steps), hru_actor_settings_(hru_actor_settings),
        num_steps_output_buffer_(num_output_steps), 
        file_access_actor_(file_access_actor), parent_(parent), restart_(restart) {
    }

    caf::behavior make_behavior();
};


