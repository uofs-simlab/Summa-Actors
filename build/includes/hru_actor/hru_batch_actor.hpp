#pragma once
#include "caf/all.hpp"
#include "hru_actor.hpp"

namespace caf {

struct hru_batch_state {
  // Actor References
	caf::actor file_access_actor;
	caf::actor parent;

  HRU_Actor_Settings hru_actor_settings;

  std::vector<caf::actor> hru_actors;
  int num_done = 0;
};

behavior hru_batch_actor(stateful_actor<hru_batch_state>* self, 
                         int start_gru_local, int start_gru_global, int num_gru, 
                         HRU_Actor_Settings hru_actor_settings,
                         caf::actor file_access_actor, caf::actor parent);


} // namespace caf