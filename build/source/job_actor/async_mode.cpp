#include "job_actor.hpp"

using namespace caf;

behavior async_mode(stateful_actor<job_state>* self) {
  aout(self) << "Async Mode Started\n";

  return {
    /*** From file access actor after it spawns ***/
    [=](file_access_actor_ready, int num_timesteps) {
      aout(self) << "Async Mode: init_file_access_actor\n";
      self->state.num_steps = num_timesteps;
      spawnHRUActors(self);
    },

    [=](done_hru, int gru_job_index) {
      handleFinishedGRU(self, gru_job_index);
    },

    [=](finalize) { finalizeJob(self); },

    /**Error Handling Functions*/
    [=](err_atom, int err_code) {
      aout(self) << "Async Mode: Error: " << err_code << "\n";
    }
    
  };
}
