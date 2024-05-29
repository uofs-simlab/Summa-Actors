#include "job_actor.hpp"

using namespace caf;

void spawnHRUActors(stateful_actor<job_state>* self) {
  auto& gru_struc = self->state.gru_struc;
  for (int i = 0; i < gru_struc->getNumGrus(); i++) {
    auto netcdf_index = gru_struc->getStartGru() + i;
    auto job_index = i + 1;
    caf::actor gru;
    if (gru_struc->getNumHruPerGru(i) > 1) {
      gru = self->spawn(gru_actor, netcdf_index, job_index, 
                        self->state.num_steps, 
                        self->state.hru_actor_settings, 
                        self->state.file_access_actor, self);
    } else {
      gru = self->spawn(hru_actor, netcdf_index, job_index, 
                        self->state.hru_actor_settings, 
                        self->state.file_access_actor, self);
      self->send(gru, init_hru_v);
      self->send(gru, update_hru_async_v);
    }
    
    // Save information about the GRU
    std::unique_ptr<GRU> gru_obj = std::make_unique<GRU>(
        netcdf_index, job_index, gru, self->state.dt_init_start_factor, 
        self->state.hru_actor_settings.rel_tol, 
        self->state.hru_actor_settings.abs_tol, 
        self->state.job_actor_settings.max_run_attempts);
    gru_struc->addGRU(std::move(gru_obj));
  }
  gru_struc->decrementRetryAttempts();    
}

void spawnHRUBatches(stateful_actor<job_state>* self) {
  aout(self) << "Job_Actor: Spawning HRU Batches\n";
  int batch_size;

  auto& gru_container = self->state.gru_container;
  gru_container.gru_start_time = std::chrono::high_resolution_clock::now();
  gru_container.run_attempts_left = self->state.job_actor_settings.max_run_attempts;
  gru_container.run_attempts_left--;

  if (self->state.job_actor_settings.batch_size == 9999) {
    batch_size = std::ceil(gru_container.num_gru_in_run_domain / 
                           (std::thread::hardware_concurrency() * 2));
  } else {
    batch_size = self->state.job_actor_settings.batch_size;
  }

  // Correct when number of batches is greater than number of HRUs
  if (batch_size == 0) {
    batch_size = 1; 
  }

  // Correct if number of GRUs is less than the desired batch size
  aout(self) << "Job_Actor: Batch Size=" << batch_size << "\n";

  int remaining_hru_to_batch = gru_container.num_gru_in_run_domain;
  int start_hru_global = self->state.batch.getStartHRU();
  int start_hru_local = 1;

  while (remaining_hru_to_batch > 0) {
    int current_batch_size = std::min(batch_size, remaining_hru_to_batch);
    auto gru_batch = self->spawn(hru_batch_actor, start_hru_local,
                                 start_hru_global, current_batch_size, 
                                 self->state.hru_actor_settings,
                                 self->state.file_access_actor, self);
    auto& job_settings = self->state.job_actor_settings;
    gru_container.gru_list.push_back(new GRU(start_hru_global, 
                                     start_hru_local, gru_batch, 
                                     self->state.dt_init_start_factor, 
                                     self->state.hru_actor_settings.rel_tol,
                                     self->state.hru_actor_settings.abs_tol, 
                                     job_settings.max_run_attempts));  

    remaining_hru_to_batch -= current_batch_size;
    start_hru_local += current_batch_size;
    start_hru_global += current_batch_size;
  }
  aout(self) << "Number of HRU_Batch_Actors: " 
             << gru_container.gru_list.size() << "\n";
}

void finalizeJob(stateful_actor<job_state>* self) {
  self->request(self->state.file_access_actor, infinite, finalize_v).await(
    [=](std::tuple<double, double> read_write_duration) {
      int err = 0;
      self->state.job_timing.updateEndPoint("total_duration");
      aout(self) << "\n________________" 
                 << "PRINTING JOB_ACTOR TIMING INFO RESULTS"
                 << "________________\n"
                 << "Total Duration = "
                 << self->state.job_timing.getDuration("total_duration")
                      .value_or(-1.0) << " Seconds\n"
                 << "Total Duration = " 
                 << self->state.job_timing.getDuration("total_duration")
                      .value_or(-1.0) / 60 << " Minutes\n"
                 << "Total Duration = " 
                 << (self->state.job_timing.getDuration("total_duration")
                    .value_or(-1.0) / 60) / 60 << " Hours\n"
                 << "Job Init Duration = " 
                 << self->state.job_timing.getDuration("init_duration")
                      .value_or(-1.0) << " Seconds\n"
                 << "_________________________________" 
                 << "_______________________________________\n\n";
            
      // Tell Parent we are done
      auto total_duration = self->state.job_timing.getDuration("total_duration")
          .value_or(-1.0);
      auto num_failed_grus = self->state.gru_struc->getNumGRUFailed();    
      self->send(self->state.parent, done_job_v, num_failed_grus, 
                 total_duration, std::get<0>(read_write_duration), 
                 std::get<1>(read_write_duration));
      self->quit();
    });
}

void handleFinishedGRU(stateful_actor<job_state>* self, int gru_job_index) {
  auto& gru_struc = self->state.gru_struc;
  gru_struc->incrementNumGRUDone();
  gru_struc->getGRU(gru_job_index)->setSuccess();

  auto& success_logger = self->state.success_logger;
  success_logger->logSuccess(gru_struc->getGRU(gru_job_index)->getIndexNetcdf(), 
                             gru_struc->getGRU(gru_job_index)->getIndexJob(),
                             self->state.hru_actor_settings.rel_tol,
                             self->state.hru_actor_settings.abs_tol);

  std::string update_str = 
      "GRU Finished: " + std::to_string(gru_struc->getNumGrusDone()) + "/" + 
      std::to_string(gru_struc->getNumGrus()) + " -- GlobalGRU=" + 
      std::to_string(gru_struc->getGRU(gru_job_index)->getIndexNetcdf()) + 
      " -- LocalGRU=" + 
      std::to_string(gru_struc->getGRU(gru_job_index)->getIndexJob()) + 
      " -- NumFailed=" + std::to_string(gru_struc->getNumGRUFailed());

  self->state.logger->log(update_str);
  aout(self) << update_str << "\n";

  if (gru_struc->isDone()) {
    gru_struc->hasFailures() && gru_struc->shouldRetry() ? 
        self->send(self, restart_failures_v) : self->send(self, finalize_v);
  }
}

void handleGRUError(stateful_actor<job_state>* self, int err_code, 
                    int gru_job_index, int timestep, std::string& err_msg) {
  // Error Handling Part
  auto& gru_struc = self->state.gru_struc;
  gru_struc->getGRU(gru_job_index)->setFailed();
  gru_struc->incrementNumGRUFailed();
  auto& err_logger = self->state.err_logger;
  err_logger->logError(gru_struc->getGRU(gru_job_index)->getIndexNetcdf(), 
                       gru_struc->getGRU(gru_job_index)->getIndexJob(), 
                       timestep, self->state.hru_actor_settings.rel_tol,
                       self->state.hru_actor_settings.abs_tol, err_code, 
                       err_msg);
  
  // Logging Part
  std::string job_err_msg = "Job Actor: GRU Failure -- " + 
      std::to_string(gru_job_index) + " Error: " + err_msg;
  self->state.logger->log(job_err_msg);
  aout(self) << job_err_msg;
  std::string update_str = 
      "\tGRU Finished: " + std::to_string(gru_struc->getNumGrusDone()) + "/" + 
      std::to_string(gru_struc->getNumGrus()) + " -- GlobalGRU=" + 
      std::to_string(gru_struc->getGRU(gru_job_index)->getIndexNetcdf()) + 
      " -- LocalGRU=" + 
      std::to_string(gru_struc->getGRU(gru_job_index)->getIndexJob()) + 
      " -- NumFailed=" + std::to_string(gru_struc->getNumGRUFailed());
  self->state.logger->log(update_str);
  aout(self) << update_str << "\n";

  self->send(self->state.file_access_actor, run_failure_v, gru_job_index);
  if (gru_struc->isDone()) {
    gru_struc->hasFailures() && gru_struc->shouldRetry() ? 
        self->send(self, restart_failures_v) : self->send(self, finalize_v);
  }
}

void handleFileAccessError(stateful_actor<job_state>* self, int err_code, 
                           std::string& err_msg) {
  // Logging Part
  self->state.logger->log("Job Actor: File_Access_Actor Error:" + err_msg);
  aout(self) << "Job Actor: File_Access_Actor Error:" << err_msg << "\n";
  if (err_code != -1) {
    self->state.logger->log("Job_Actor: Have to Quit");
    aout(self) << "Job_Actor: Have to Quit\n";
    self->quit();
    return;
  }
}