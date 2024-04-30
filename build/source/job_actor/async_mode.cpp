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

    [=] (restart_failures) {
      aout(self) << "Async Mode: Restarting GRUs that Failed\n";
    },

    [=](finalize) { finalizeJob(self); },

    /**Error Handling Functions*/
    [=](err_atom, int err_code, int gru_job_index) {
      if (gru_job_index == 0) {
        aout(self) << "Async Mode: File_Access_Actor Error: " 
                   << err_code << "\n";
        self->send(self, finalize_v);
        return;
      } 
      aout(self) << "Async Mode: GRU Error: " << err_code << "\n";
      handleGRUError(self, err_code, gru_job_index);
    }
    
  };
}
