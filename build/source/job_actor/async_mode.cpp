#include "job_actor.hpp"
namespace caf {

behavior async_mode(stateful_actor<job_state>* self) {
  aout(self) << "Async Mode Started\n";

  return {
    /*** From file access actor after it spawns ***/
    [=](file_access_actor_ready, int num_timesteps) {
      aout(self) << "Async Mode: init_file_access_actor\n";
      self->state.num_steps = num_timesteps;
      spawnHRUActors(self);
      for(auto& gru : self->state.gru_container.gru_list) {
        self->send(gru->getGRUActor(), init_hru_v);
        self->send(gru->getGRUActor(), update_hru_async_v);
      }
    },

    [=](done_hru, int local_gru_index) {
      aout(self) << "HRU Done: " << local_gru_index << "\n";
      handleFinishedGRU(self, local_gru_index);
    },

    [=](finalize) { finalizeJob(self); },
    
  };
}

} // End of Namespace