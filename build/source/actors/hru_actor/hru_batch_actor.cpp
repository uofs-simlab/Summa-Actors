#include "hru_batch_actor.hpp"

namespace caf {

behavior hru_batch_actor(stateful_actor<hru_batch_state>* self,
                         int start_gru_local, int start_gru_global, int num_gru,
                         HRU_Actor_Settings hru_actor_settings,
                         caf::actor file_access_actor, caf::actor parent) {
  
  self->state.file_access_actor = file_access_actor;
  self->state.parent = parent;

  
  for (int i = 0; i < num_gru; i++) {
    self->state.hru_actors.push_back(
      self->spawn(hru_actor, start_gru_global + i, start_gru_local + i, 
                  hru_actor_settings, file_access_actor, self)
    );
  }



  return {
    [=](update_timeZoneOffset, int iFile) {
      // aout(self) << "HRU Batch Actor - Update Time Zone Offset\n";
      for (auto& hru_actor : self->state.hru_actors) {
        self->send(hru_actor, update_timeZoneOffset_v, iFile);
      }
    },

    [=](update_hru, int timestep, int forcingstep) {
      // aout(self) << "HRU Batch Actor - Update HRU\n";
      for (auto& hru_actor : self->state.hru_actors) {
        self->send(hru_actor, update_hru_v, timestep, forcingstep);
      }
    },

    [=](done_update) {
      // aout(self) << "HRU Batch Actor - Done Update\n";
      self->state.num_done++;
      if (self->state.num_done == self->state.hru_actors.size()) {
        self->send(self->state.parent, done_update_v);
        self->state.num_done = 0;
      }
    },

    [=](exit_msg) {
      for(auto& hru_actor : self->state.hru_actors) {
        self->send_exit(hru_actor, exit_reason::user_shutdown);
      }
      self->quit();
    }
  };

}
                            


}