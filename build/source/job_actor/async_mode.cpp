#include "job_actor.hpp"

using namespace caf;

behavior async_mode(stateful_actor<job_state>* self) {
  self->state.logger.log("Async Mode: Started");
  aout(self) << "Async Mode Started\n";

  return {
    /*** From file access actor after it spawns ***/
    [=](file_access_actor_ready, int num_timesteps) {
      self->state.logger.log("Async Mode: File Access Actor Ready");
      aout(self) << "Async Mode: File Access Actor Ready\n";
      self->state.num_steps = num_timesteps;
      spawnHRUActors(self);    
    },

    [=](done_hru, int gru_job_index) {
      handleFinishedGRU(self, gru_job_index);
    },

    [=] (restart_failures) {
      self->state.logger.log("Async Mode: Restarting GRUs that Failed");
      aout(self) << "Async Mode: Restarting GRUs that Failed\n";
      if (self->state.hru_actor_settings.rel_tol > 0 && 
          self->state.hru_actor_settings.abs_tol > 0) {
        self->state.hru_actor_settings.rel_tol /= 10;
        self->state.hru_actor_settings.abs_tol /= 10;
      } else {
        self->state.hru_actor_settings.dt_init_factor *= 2;
      }

      // notify file_access_actor
      self->send(self->state.file_access_actor, restart_failures_v); 

      while(self->state.gru_struc->getNumGRUFailed() > 0) {
        int job_index = self->state.gru_struc->getFailedIndex();
        self->state.logger.log("Async Mode: Restarting GRU: " + 
                               std::to_string(job_index));
        aout(self) << "Async Mode: Restarting GRU: " << job_index << "\n";
        int netcdf_index = job_index + self->state.gru_struc->getStartGru() - 1;
        auto gru =  self->spawn(hru_actor, netcdf_index, job_index, 
                                self->state.hru_actor_settings, 
                                self->state.file_access_actor, self);
        self->send(gru, init_hru_v);
        self->send(gru, update_hru_async_v);
        self->state.gru_struc->decrementNumGRUFailed();
        std::unique_ptr<GRU> gru_obj = std::make_unique<GRU>(
            netcdf_index, job_index, gru, self->state.dt_init_start_factor, 
            self->state.hru_actor_settings.rel_tol, 
            self->state.hru_actor_settings.abs_tol, 
            self->state.job_actor_settings.max_run_attempts);
        self->state.gru_struc->addGRU(std::move(gru_obj));
      }
      self->state.gru_struc->decrementRetryAttempts();

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
      std::string err_msg = "Async Mode: GRU Error: " + 
                            std::to_string(gru_job_index);
      self->state.logger.log(err_msg);
      aout(self) << err_msg << "\n";
      handleGRUError(self, err_code, gru_job_index);
    }
    
  };
}
