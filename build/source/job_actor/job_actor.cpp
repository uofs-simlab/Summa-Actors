#include "job_actor.hpp"

using json = nlohmann::json;
using chrono_time = std::chrono::time_point<std::chrono::high_resolution_clock>;
using namespace caf;

// ------------------------Behaviors------------------------
behavior JobActor::make_behavior() {
  std::string err_msg;
  self_->println("JobActor Started");

  gethostname(hostname_, HOST_NAME_MAX);

  // Timing Information
  timing_info_ = TimingInfo(); 
  timing_info_.addTimePoint("total_duration");
  timing_info_.updateStartPoint("total_duration");
  timing_info_.addTimePoint("init_duration");
  timing_info_.updateStartPoint("init_duration");

  // Create Loggers
  if (enable_logging_) {
    logger_ = std::make_unique<Logger>(batch_.getLogDir() + "batch_" + 
                                       std::to_string(batch_.getBatchID()));
    err_logger_ = std::make_unique<ErrorLogger>(batch_.getLogDir());
    success_logger_ = std::make_unique<SuccessLogger>(batch_.getLogDir()); 
  } else {
    logger_ = std::make_unique<Logger>("");
    err_logger_ = std::make_unique<ErrorLogger>("");
    success_logger_ = std::make_unique<SuccessLogger>("");
  }

  // GruStruc Initialization
  gru_struc_ = std::make_unique<GruStruc>(batch_.getStartHRU(), 
      batch_.getNumHRU(), job_actor_settings_.max_run_attempts_);
  if (gru_struc_->readDimension()) {
    err_msg = "ERROR: Job_Actor - ReadDimension\n";
    self_->mail(err_atom_v, -2, err_msg).send(parent_);
    return {};
  }
  if (gru_struc_->readIcondNlayers()) {
    err_msg = "ERROR: Job_Actor - ReadIcondNlayers\n";
    self_->mail(err_atom_v, -2, err_msg).send(parent_);
    return {};
  }
  // todo: check if this is necessary
  gru_struc_->getNumHrusPerGru();

  // SummaInitStruc Initialization
  summa_init_struc_ = std::make_unique<SummaInitStruc>();
  if (summa_init_struc_->allocate(batch_.getNumHRU()) != 0) {
    err_msg = "ERROR -- Job_Actor: SummaInitStruc allocation failed\n";
    self_->mail(err_atom_v, -2, err_msg).send(parent_);
    return {};
  }
  if (summa_init_struc_->summa_paramSetup() != 0) {
    err_msg = "ERROR -- Job_Actor: SummaInitStruc paramSetup failed\n";
    self_->mail(err_atom_v, -2, err_msg).send(parent_);
    return {};
  }
  if (summa_init_struc_->summa_readRestart()!= 0) {
    err_msg = "ERROR -- Job_Actor: SummaInitStruc readRestart failed\n";
    self_->mail(err_atom_v, -2, err_msg).send(parent_);
    return {};
  }

  summa_init_struc_->getInitTolerance(rel_tol_, abs_tol_);
  
  num_gru_info_ = NumGRUInfo(batch_.getStartHRU(), batch_.getStartHRU(), 
                             batch_.getNumHRU(), batch_.getNumHRU(), 
                             gru_struc_->getFileGru(), false);
  
  // Start File Access Actor and Become User Selected Mode
  file_access_actor_ = self_->spawn(actor_from_state<FileAccessActor>, 
                                    num_gru_info_, fa_actor_settings_, self_);

  self_->mail(init_file_access_actor_v, gru_struc_->getFileGru(),
              gru_struc_->getNumHru())
      .request(file_access_actor_, caf::infinite)
      .await([=](int num_timesteps){
    if (num_timesteps < 0) {
      std::string err_msg = "ERROR: Job_Actor: File Access Actor Not Ready\n";
      self_->mail(err_atom_v, -2, err_msg).send(parent_);
      self_->quit();
      return;
    }
    timing_info_.updateEndPoint("init_duration");

    // Start JobActor in User Selected Mode
    logger_->log("JobActor Initialized");
    self_->println("JobActor Initialized: Running {} Steps", num_timesteps);
    job_actor_settings_.data_assimilation_mode_ ? 
        self_->become(data_assimilation_mode()) : 
        self_->become(async_mode());
    
    self_->mail(file_access_actor_ready_v, num_timesteps).send(self_);
  });

  return {};
}


behavior JobActor::async_mode() {
  self_->println("Async Mode: Started");
  return {
    [this](file_access_actor_ready, int num_timesteps) {
      self_->println("Async Mode: File Access Actor Ready");
      logger_->log("Async Mode: File Access Actor Ready");
      num_steps_ = num_timesteps;
      spawnGruActors();
    },

    [this](done_hru, int job_index) {
      handleFinishedGRU(job_index);
    },

    [this](restart_failures) {
      logger_->log("Async Mode: Restarting Failed GRUs");
      self_->println("Async Mode: Restarting Failed GRUs\n");
      if (rel_tol_ > 0 && abs_tol_ > 0) {
        rel_tol_ /= 10;
        abs_tol_ /= 10;
      } else {
        dt_init_factor_ *= 2;
      }

      // notify file_access_actor
      self_->mail(restart_failures_v).send(file_access_actor_);
      err_logger_->nextAttempt();
      success_logger_->nextAttempt();

      while(gru_struc_->getNumGruFailed() > 0) {
        int job_index = gru_struc_->getFailedIndex();
        logger_->log("Async Mode: Restarting GRU: " + 
            std::to_string(job_index));
        self_->println("Async Mode: Restarting GRU: " + 
            std::to_string(job_index));
        int netcdf_index = job_index + gru_struc_->getStartGru() - 1;
        auto gru_actor = self_->spawn(actor_from_state<GruActor>, netcdf_index, 
            job_index, num_steps_, hru_actor_settings_,
            job_actor_settings_.data_assimilation_mode_, 
            fa_actor_settings_.num_timesteps_in_output_buffer_,
            file_access_actor_, self_);
        gru_struc_->decrementNumGruFailed();
        std::unique_ptr<GRU> gru_obj = std::make_unique<GRU>(
            netcdf_index, job_index, gru_actor, dt_init_factor_, rel_tol_, 
            abs_tol_, job_actor_settings_.max_run_attempts_);
        gru_struc_->addGRU(std::move(gru_obj));
      }
      gru_struc_->decrementRetryAttempts();
    },

    [this](finalize) {
      finalizeJob();
    },

    // Error Handling
    [this](err_atom, int job_index, int timestep, int err_code, 
           std::string err_msg) {
      (job_index == 0) ? 
        handleFileAccessError(err_code, err_msg) :
        handleGRUError(err_code, job_index, timestep, err_msg);
    },

    [this](const down_msg& dm) {
      self_->println("Lost Connection With A Connected Actor\nReason: {}",
                     to_string(dm.reason));
    },

    [this](const caf::exit_msg& em) {
      self_->println("Exit Reason: {}", to_string(em.reason));
    }
  };
}

behavior JobActor::data_assimilation_mode() {
  logger_->log("Data Assimilation Mode: Started");
  self_->println("Data Assimilation Mode: Started");
  return {
    [this](file_access_actor_ready, int num_timesteps) {
      self_->println("JobActor: File Access Actor Ready");
      num_steps_ = num_timesteps;
      spawnGruBatches();
      self_->println("JobActor: GRUs Initialized");
      self_->mail(access_forcing_v, iFile_, self_).send(file_access_actor_);
    },

    [this](new_forcing_file, int num_steps_iFile, int next_file) {
      self_->println("JobActor: New Forcing File");
      iFile_ = next_file;
      steps_in_ffile_ = num_steps_iFile;
      forcing_step_ = 1;
      
      for (auto& gru : gru_struc_->getGruInfo()) {
        self_->mail(update_timeZoneOffset_v, iFile_).send(gru->getActorRef());
      }

      self_->mail(update_hru_v).send(self_);
    },

    [this](update_hru) {
      for (auto& gru : gru_struc_->getGruInfo()) {
        self_->mail(update_hru_v, timestep_, forcing_step_)
            .send(gru->getActorRef());
      }
    },

    [this](done_update) {
      num_gru_done_timestep_++;
      if (num_gru_done_timestep_ >= gru_struc_->getGruInfo().size()) {
        if (hru_actor_settings_.print_output_ && 
            timestep_ % hru_actor_settings_.output_frequency_ == 0) {
          self_->println("JobActor: Done Update for timestep: {}", 
                       timestep_);
        }

        // write output
        int steps_to_write = 1;
        int start_gru = 1;
        self_->mail(write_output_v, steps_to_write, start_gru, 
                    batch_.getNumHRU())
            .request(file_access_actor_, caf::infinite)
            .await([=](int err) {
              if (err != 0) {
                self_->println("JobActor: Error Writing Output");
                for (auto& gru : gru_struc_->getGruInfo()) {
                  self_->mail(exit_msg_v).send(gru->getActorRef());
                }
                self_->send_exit(file_access_actor_, 
                                 exit_reason::user_shutdown);
                self_->quit();
              }
            });

        timestep_++;
        forcing_step_++;

        // Check if we are done
        if (timestep_ > num_steps_) {
          self_->println("JobActor: Done");
          for (auto& gru : gru_struc_->getGruInfo()) {
            self_->mail(exit_reason::user_shutdown)
                .send(gru->getActorRef());
          }
          self_->mail(finalize_v).send(self_);
        
        } else if (forcing_step_ > steps_in_ffile_) {
          self_->println("JobActor: Getting New Forcing File");
          self_->mail(access_forcing_v, iFile_ + 1, self_)
              .send(file_access_actor_);
        } else {
          self_->mail(update_hru_v).send(self_);
        }
        num_gru_done_timestep_ = 0;
      }    
    },

    [this](finalize) {
      finalizeJob();
    },

    [this](const down_msg& dm) {
      self_->println("Lost Connection With A Connected Actor\nReason: {}",
                     to_string(dm.reason));
    },
    
    [this](const caf::exit_msg& em) {
      self_->println("Exit Reason: {}", to_string(em.reason));
    }
  };
}



// ------------------------ Member Functions ------------------------
void JobActor::spawnGruActors() {
  self_->println("JobActor: Spawning GRU Actors");
  for (int i = 0; i < gru_struc_->getNumGru(); i++) {
    auto netcdf_index = gru_struc_->getStartGru() + i;
    auto job_index = i + 1;
    auto gru_actor = self_->spawn(actor_from_state<GruActor>, netcdf_index, 
        job_index, num_steps_, hru_actor_settings_,
        job_actor_settings_.data_assimilation_mode_,
        fa_actor_settings_.num_timesteps_in_output_buffer_, file_access_actor_, 
        self_);
    std::unique_ptr<GRU> gru_obj = std::make_unique<GRU>(
        netcdf_index, job_index, gru_actor, dt_init_factor_, rel_tol_, 
        abs_tol_, job_actor_settings_.max_run_attempts_);
    gru_struc_->addGRU(std::move(gru_obj));
    
    if (!job_actor_settings_.data_assimilation_mode_) {
      self_->mail(update_hru_async_v).send(gru_actor);
    }
  }
  gru_struc_->decrementRetryAttempts();
}



void JobActor::spawnGruBatches() {
  self_->println("JobActor: Spawning GRU Batch Actors");
  int batch_size;

  if (job_actor_settings_.batch_size_ < 0) {
    // Automatically determine batch size
    batch_size = std::ceil(gru_struc_->getNumGru() / 
                           (std::thread::hardware_concurrency() * 2));
  } else {
    // Use the user selected batch size
    batch_size = job_actor_settings_.batch_size_;
  }

  self_->println("JobActor: Batch Size {}", batch_size);
  
  if (batch_size == 0 || batch_size == 1) {
    batch_size = 1;
    // Batch Size of 1 is same as having no batch actor
    spawnGruActors();
    return;
  }
  int remaining_hru_to_batch = gru_struc_->getNumGru();
  int start_hru_global = batch_.getStartHRU();
  int start_hru_local = 1;

  while (remaining_hru_to_batch > 0) {
    int current_batch_size = std::min(batch_size, remaining_hru_to_batch);
    auto gru_batch = self_->spawn(actor_from_state<GruBatchActor>, 
        start_hru_local, start_hru_global, current_batch_size, num_steps_,
        hru_actor_settings_, fa_actor_settings_.num_timesteps_in_output_buffer_,
        file_access_actor_, self_);
    std::unique_ptr<GRU> gru_obj = std::make_unique<GRU>(
        start_hru_global, start_hru_local, gru_batch, dt_init_factor_, rel_tol_, 
        abs_tol_, job_actor_settings_.max_run_attempts_);
    gru_struc_->addGRU(std::move(gru_obj));
    remaining_hru_to_batch -= current_batch_size;
    start_hru_local += current_batch_size;
    start_hru_global += current_batch_size;
  }
  self_->println("JobActor: Assembled GRUs into Batches");
}



void JobActor::handleFinishedGRU(int job_index) {
  gru_struc_->incrementNumGruDone();
  gru_struc_->getGRU(job_index)->setSuccess();
  success_logger_->logSuccess(gru_struc_->getGRU(job_index)->getIndexNetcdf(),
                              gru_struc_->getGRU(job_index)->getIndexJob(),
                              rel_tol_, abs_tol_);
  std::string update_str =
      "GRU Finished: " + std::to_string(gru_struc_->getNumGruDone()) + "/" + 
      std::to_string(gru_struc_->getNumGru()) + " -- GlobalGRU=" + 
      std::to_string(gru_struc_->getGRU(job_index)->getIndexNetcdf()) + 
      " -- LocalGRU=" + 
      std::to_string(gru_struc_->getGRU(job_index)->getIndexJob()) + 
      " -- NumFailed=" + std::to_string(gru_struc_->getNumGruFailed());
  logger_->log(update_str);
  self_->println(update_str);

  if (gru_struc_->isDone()) {
    gru_struc_->hasFailures() && gru_struc_->shouldRetry() ?
        self_->mail(restart_failures_v).send(self_) 
        : self_->mail(finalize_v).send(self_);
  }
}



void JobActor::finalizeJob() {
  self_->mail(finalize_v).request(file_access_actor_, infinite).await(
    [=](std::tuple<double, double> read_write_duration) {
      int err = 0;
      timing_info_.updateEndPoint("total_duration");
      self_->println(
          "\n_____________PRINTING JOB_ACTOR TIMING INFO RESULTS____________\n"
          "Total Duration = {} Seconds\n"
          "Total Duration = {} Minutes\n"
          "Total Duration = {} Hours\n" 
          "Job Init Duration = {} Seconds\n" 
          "_________________________________________________________________\n\n",
          timing_info_.getDuration("total_duration").value_or(-1.0),
          timing_info_.getDuration("total_duration").value_or(-1.0) / 60,
          (timing_info_.getDuration("total_duration").value_or(-1.0) / 60) / 60,
          timing_info_.getDuration("init_duration").value_or(-1.0));
      
        // Tell Parent we are done
        auto total_duration = timing_info_.getDuration("total_duration").
            value_or(-1.0);
        auto num_failed_grus = gru_struc_->getNumGruFailed();    
        self_->mail(done_job_v, num_failed_grus, total_duration, 
                    std::get<0>(read_write_duration), 
                    std::get<1>(read_write_duration))
            .send(parent_);
        self_->quit();
    });
}



// ------------------------ERROR HANDLING FUNCTIONS ------------------------
void JobActor::handleGRUError(int err_code, int job_index, int timestep, 
                              std::string& err_msg) {
  gru_struc_->getGRU(job_index)->setFailed();
}


void JobActor::handleFileAccessError(int err_code, std::string& err_msg) {
  logger_->log("JobActor: File_Access_Actor Error:" + err_msg);
  self_->println("JobActor: File_Access_Actor Error: {}", err_msg);
  if (err_code != -1) {
    logger_->log("JobActor: Have to Quit");
    self_->println("JobActor: Have to Quit");
    self_->quit();
    return;
  }
}
