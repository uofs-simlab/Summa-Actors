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
  summa_init_struc_->getInitBEStepsIDATol(be_steps_, rel_tol_, abs_tolWat_, abs_tolNrg_);
  
  num_gru_info_ = NumGRUInfo(batch_.getStartHRU(), batch_.getStartHRU(), 
                             batch_.getNumHRU(), batch_.getNumHRU(), 
                             gru_struc_->getFileGru(), false);


  // Set the file_access_actor settings depending on data assimilation mode
  if (job_actor_settings_.data_assimilation_mode_) {
    fa_actor_settings_.num_partitions_in_output_buffer_ = 1;
    fa_actor_settings_.num_timesteps_in_output_buffer_ = 2;
  } 
  
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
      logger_->log("Async Mode: Restarting Failed GRUs (only works for >=V4)");
      self_->println("Async Mode: Restarting Failed GRUs (only works for >=V4)\n");
      if (rel_tol_ > 0 && abs_tolWat_ > 0 && abs_tolNrg_ > 0) {
        logger_->log("Reducing IDA Tolerances by * 0.1");
        self_->println("Reducing IDA Tolerances by * 0.1\n");
        rel_tol_ /= 10;
        hru_actor_settings_.rel_tol_ = rel_tol_;
        abs_tolWat_ /= 10;
        hru_actor_settings_.abs_tolWat_ = abs_tolWat_;
        abs_tolNrg_ /= 10;
        hru_actor_settings_.abs_tolNrg_ = abs_tolNrg_;
        be_steps_ = 1;
        hru_actor_settings_.be_steps = be_steps_;
      } else if (be_steps_ >0) {
        logger_->log("Increasing BE Steps by * 2");
        self_->println("Increasing BE Steps by * 2\n");
        be_steps_ *= 2;
        hru_actor_settings_.be_steps = be_steps_;
      } else {
        logger_->log("Initial IDA Tolerances and BE Steps not set, increasing dt_init_factor by * 2 (not recommended)");
        self_->println("Initial IDA Tolerances and BE Steps not set, increasing dt_init_factor by * 2 (not recommended)\n");
        dt_init_factor_ *= 2;
      }


      // notify file_access_actor
      self_->mail(restart_failures_v).send(file_access_actor_);

      // Give time for the file_access_actor to reconstruct 
      std::this_thread::sleep_for(std::chrono::seconds(2));

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
            abs_tolWat_, abs_tolNrg_, job_actor_settings_.max_run_attempts_);
        gru_struc_->addGRU(std::move(gru_obj));
        self_->mail(update_hru_async_v).send(gru_actor);
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
        self_->mail(update_hru_v, timestep_, forcing_step_, output_step_ + 1)
            .send(gru->getActorRef());
      }
    },

    [this](done_update) {
      num_gru_done_timestep_++;
      if (num_gru_done_timestep_ < gru_struc_->getGruInfo().size()) {
        return;
      }

      if (hru_actor_settings_.print_output_ && 
          timestep_ % hru_actor_settings_.output_frequency_ == 0) {
        self_->println("JobActor: Done Update for timestep: {}", 
                      timestep_);
      }

      // write output
      int steps_to_write = 1;
      int start_gru = 1;
      self_->mail(write_output_v, output_step_ + 1).send(file_access_actor_);
      num_write_msgs_++;
      
      timestep_++;
      forcing_step_++;
      output_step_ = (output_step_ + 1) % fa_actor_settings_.num_timesteps_in_output_buffer_;
      num_gru_done_timestep_ = 0;

      if (num_write_msgs_ >= fa_actor_settings_.num_timesteps_in_output_buffer_) {
        self_->println("JobActor: Waiting for Write Output to Finish");
        da_paused_ = true;
        return;
      }
       
      processTimestep();
    },
    
    [this](write_output, int err) {
      if (err != 0) {
        self_->println("JobActor: Error Writing Output");
        for (auto& gru : gru_struc_->getGruInfo())
          self_->mail(exit_msg_v).send(gru->getActorRef());
        self_->send_exit(file_access_actor_, exit_reason::user_shutdown);
        self_->quit();
      }
      num_write_msgs_--;

      if (!da_paused_) return;
      da_paused_ = false;

      // We need to unpause
      processTimestep();
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
    // TODO: Implement f_getRelTol and f_getAbsTol
  if (hru_actor_settings_.rel_tol_ > 0) {
    // f_getRelTol();
    rel_tol_ = hru_actor_settings_.rel_tol_;
  }

  if (hru_actor_settings_.abs_tolWat_ > 0) {
    // f_getAbsTol();
    abs_tolWat_ = hru_actor_settings_.abs_tolWat_;
  }

  if (hru_actor_settings_.abs_tolNrg_ > 0) {
    // f_getAbsTol();
    abs_tolNrg_ = hru_actor_settings_.abs_tolNrg_;
  }

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
        abs_tolWat_, abs_tolNrg_, job_actor_settings_.max_run_attempts_);
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

  // TODO: Implement f_getRelTol and f_getAbsTol
  if (hru_actor_settings_.rel_tol_ <= 0) {
    // f_getRelTol();
    rel_tol_ = hru_actor_settings_.rel_tol_;
  }

  if (hru_actor_settings_.abs_tolWat_ <= 0) {
    // f_getAbsTol();
    abs_tolWat_ = hru_actor_settings_.abs_tolWat_;
  }

  if (hru_actor_settings_.abs_tolNrg_ <= 0) {
    // f_getAbsTol();
    abs_tolNrg_ = hru_actor_settings_.abs_tolNrg_;
  }

  // if (rel_tol_ <= 0) {
  // }

  // if (abs_tolWat_ <= 0) {
  //   f_getAbsTol();
  // }

  if (job_actor_settings_.batch_size_ < 0) {
    // Automatically determine batch size
    batch_size = std::ceil(static_cast<double>(gru_struc_->getNumGru()) / 
        static_cast<double>(std::thread::hardware_concurrency()));
  } else {
    // Use the user selected batch size
    batch_size = job_actor_settings_.batch_size_;
  }

  self_->println("JobActor: Batch Size {}", batch_size);
  
  if (batch_size <= 1) {
    spawnGruActors();
    return;
  }

  int remaining_hru_to_batch = gru_struc_->getNumGru();
  int netcdf_start_index = batch_.getStartHRU();
  int job_start_index = 1;

  while (remaining_hru_to_batch > 0) {
    int current_batch_size = std::min(batch_size, remaining_hru_to_batch);
    auto gru_batch = self_->spawn(actor_from_state<GruBatchActor>, 
        netcdf_start_index, job_start_index, current_batch_size, num_steps_,
        hru_actor_settings_, fa_actor_settings_.num_timesteps_in_output_buffer_,
        file_access_actor_, self_);

    std::unique_ptr<GRU> gru_obj = std::make_unique<GRU>(
        netcdf_start_index, job_start_index, gru_batch, dt_init_factor_, 
        rel_tol_, abs_tolWat_, abs_tolNrg_, job_actor_settings_.max_run_attempts_);
    gru_struc_->addGRU(std::move(gru_obj));
    remaining_hru_to_batch -= current_batch_size;
    job_start_index += current_batch_size;
    netcdf_start_index += current_batch_size;
  }
  self_->println("JobActor: Assembled GRUs into Batches");
}

void JobActor::processTimestep() {
  if (timestep_ > num_steps_) {
    self_->println("JobActor: Done");
    for (auto& gru : gru_struc_->getGruInfo()) {
      self_->mail(exit_reason::user_shutdown)
          .send(gru->getActorRef());
    }
    self_->mail(finalize_v).send(self_);
  // Check if new forcing file is needed
  } else if (forcing_step_ > steps_in_ffile_) {
    self_->println("JobActor: Requesting New Forcing File");
    self_->mail(access_forcing_v, iFile_ + 1, self_)
        .send(file_access_actor_);
  // Just update the HRUs
  } else {
    self_->mail(update_hru_v).send(self_);
  }
}

void JobActor::handleFinishedGRU(int job_index) {
  gru_struc_->incrementNumGruDone();
  gru_struc_->getGRU(job_index)->setSuccess();
  success_logger_->logSuccess(gru_struc_->getGRU(job_index)->getIndexNetcdf(),
                              gru_struc_->getGRU(job_index)->getIndexJob(),
                              rel_tol_, abs_tolWat_, abs_tolNrg_);
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
      auto num_failed_grus = gru_struc_->getNumGruFailed();    
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

      // Deallocate GRU_Struc
      gru_struc_.reset();    
      summa_init_struc_.reset();
  
      // Tell Parent we are done
      auto total_duration = timing_info_.getDuration("total_duration").
          value_or(-1.0);
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
  gru_struc_->incrementNumGruFailed();
  self_->mail(run_failure_v, job_index).send(file_access_actor_);
  if (gru_struc_->isDone()) {
    gru_struc_->hasFailures() && gru_struc_->shouldRetry() ?
        self_->mail(restart_failures_v).send(self_) 
        : self_->mail(finalize_v).send(self_);
  }
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
