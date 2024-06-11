#include "job_actor.hpp"

using namespace caf;

behavior JobActor::async_mode() {
  self_->println("Async Mode: Started");
  return {
    [this](file_access_actor_ready, int num_timesteps) {
      self_->println("Async Mode: File Access Actor Ready");
      logger_->log("Async Mode: File Access Actor Ready");
      num_steps_ = num_timesteps;
      spawnGRUActors();
    },

    [this](done_hru, int job_index) {
      handleFinishedGRU(job_index);
    },

    [this](restart_failures) {
      logger_->log("Async Mode: Restarting Failed GRUs");
      aout(self_) << "Async Mode: Restarting Failed GRUs\n";
      if (hru_actor_settings_.rel_tol_ > 0 && 
          hru_actor_settings_.abs_tol_ > 0) {
        hru_actor_settings_.rel_tol_ /= 10;
        hru_actor_settings_.abs_tol_ /= 10;
      } else {
        hru_actor_settings_.dt_init_factor_ *= 2;
      }

      // notify file_access_actor
      self_->send(file_access_actor_, restart_failures_v);
      err_logger_->nextAttempt();
      success_logger_->nextAttempt();

      while(gru_struc_->getNumGRUFailed() > 0) {
        int job_index = gru_struc_->getFailedIndex();
        logger_->log("Async Mode: Restarting GRU: " + 
            std::to_string(job_index));
        self_->println("Async Mode: Restarting GRU: " + 
            std::to_string(job_index));
        int netcdf_index = job_index + gru_struc_->getStartGru() - 1;
        auto gru_actor =  self_->spawn(actor_from_state<GruActor>, netcdf_index, 
                                 job_index, num_steps_, hru_actor_settings_, 
                                 file_access_actor_, self_);
        gru_struc_->decrementNumGRUFailed();
        std::unique_ptr<GRU> gru_obj = std::make_unique<GRU>(
            netcdf_index, job_index, gru_actor, 
            hru_actor_settings_.dt_init_factor_, 
            hru_actor_settings_.rel_tol_, 
            hru_actor_settings_.abs_tol_, 
            job_actor_settings_.max_run_attempts_);
        gru_struc_->addGRU(std::move(gru_obj));
      }
      gru_struc_->decrementRetryAttempts();
    },

    [this](finalize) {
      finalizeJob();
    },

    [this](err_atom, int job_index, int timestep, int err_code, 
           std::string err_msg) {
      (job_index == 0) ? 
        handleFileAccessError(err_code, err_msg) :
        handleGRUError(err_code, job_index, timestep, err_msg);
    }
  };
}
