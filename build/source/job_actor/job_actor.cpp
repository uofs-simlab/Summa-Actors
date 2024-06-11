#include "job_actor.hpp"

using json = nlohmann::json;
using chrono_time = std::chrono::time_point<std::chrono::system_clock>;
using namespace caf;


behavior JobActor::make_behavior() {
  std::string err_msg;
  self_->println("Job Actor Started");
  self_->set_down_handler([=](const down_msg& dm) {
    self_->println("Lost Connection With A Connected Actor\nReason: {}",
                   to_string(dm.reason));
  });
  self_->set_exit_handler([=](const caf::exit_msg& em) {
    self_->println("Exit Reason: {}", to_string(em.reason));
  });

  gethostname(hostname_, HOST_NAME_MAX);

  // Timing Information
  timing_info_ = TimingInfo(); 
  timing_info_.addTimePoint("total_duration");
  timing_info_.updateStartPoint("total_duration");
  timing_info_.addTimePoint("init_duration");
  timing_info_.updateStartPoint("init_duration");

  // Create Loggers
  logger_ = std::make_unique<Logger>(batch_.getLogDir() + "batch_" + 
                                     std::to_string(batch_.getBatchID()));
  err_logger_ = std::make_unique<ErrorLogger>(batch_.getLogDir());
  success_logger_ = std::make_unique<SuccessLogger>(batch_.getLogDir()); 

  // GruStruc Initialization
  gru_struc_ = std::make_unique<GruStruc>(batch_.getStartHRU(), 
      batch_.getNumHRU(), job_actor_settings_.max_run_attempts_);
  if (gru_struc_->ReadDimension()) {
    err_msg = "ERROR: Job_Actor - ReadDimension\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  if (gru_struc_->ReadIcondNlayers()) {
    err_msg = "ERROR: Job_Actor - ReadIcondNlayers\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  gru_struc_->getNumHrusPerGru();  

  // SummaInitStruc Initialization
  summa_init_struc_ = std::make_unique<SummaInitStruc>();
  if (summa_init_struc_->allocate(batch_.getNumHRU()) != 0) {
    err_msg = "ERROR -- Job_Actor: SummaInitStruc allocation failed\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  if (summa_init_struc_->summa_paramSetup() != 0) {
    err_msg = "ERROR -- Job_Actor: SummaInitStruc paramSetup failed\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  if (summa_init_struc_->summa_readRestart()!= 0) {
    err_msg = "ERROR -- Job_Actor: SummaInitStruc readRestart failed\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  summa_init_struc_->getInitTolerance(hru_actor_settings_);
  
  num_gru_info_ = NumGRUInfo(batch_.getStartHRU(), batch_.getStartHRU(), 
                             batch_.getNumHRU(), batch_.getNumHRU(), 
                             gru_struc_->get_file_gru(), false);
  
  // Start File Access Actor and Become User Selected Mode
  file_access_actor_ = self_->spawn(actor_from_state<FileAccessActor>, 
                                    num_gru_info_, fa_actor_settings_, self_);

  self_->request(file_access_actor_, caf::infinite,
                 init_file_access_actor_v, gru_struc_->get_file_gru(),
                 gru_struc_->getNumHrus()).await([=](int num_timesteps){
    if (num_timesteps < 0) {
      std::string err_msg = "ERROR: Job_Actor: File Access Actor Not Ready\n";
      self_->send(parent_, err_atom_v, -2, err_msg);
      self_->quit();
      return;
    }
    timing_info_.updateEndPoint("init_duration");
    logger_->log("Job Actor Initialized");
    self_->println("Job Actor Initialized: Running {} Steps", num_timesteps);
    job_actor_settings_.data_assimilation_mode_ ? 
        self_->become(data_assimilation_mode()) : 
        self_->become(async_mode());
    
    self_->send(self_, file_access_actor_ready_v, num_timesteps);
  });

  return {};
}



void JobActor::spawnGRUActors() {
  self_->println("Job Actor: Spawning GRU Actors");
  for (int i = 0; i < gru_struc_->getNumGrus(); i++) {
    auto netcdf_index = gru_struc_->getStartGru() + i;
    auto job_index = i + 1;
    auto gru_actor = self_->spawn(actor_from_state<GruActor>, netcdf_index, 
                                  job_index, num_steps_, hru_actor_settings_, 
                                  file_access_actor_, self_);
  std::unique_ptr<GRU> gru_obj = std::make_unique<GRU>(
      netcdf_index, job_index, gru_actor, 
      hru_actor_settings_.dt_init_factor_,
      hru_actor_settings_.rel_tol_, hru_actor_settings_.abs_tol_,
      job_actor_settings_.max_run_attempts_);
  gru_struc_->addGRU(std::move(gru_obj));
  }
  gru_struc_->decrementRetryAttempts();
}



void JobActor::handleFinishedGRU(int job_index) {
  gru_struc_->incrementNumGRUDone();
  gru_struc_->getGRU(job_index)->setSuccess();
  success_logger_->logSuccess(gru_struc_->getGRU(job_index)->getIndexNetcdf(),
                              gru_struc_->getGRU(job_index)->getIndexJob(),
                              hru_actor_settings_.rel_tol_,
                              hru_actor_settings_.abs_tol_);
  std::string update_str =
      "GRU Finished: " + std::to_string(gru_struc_->getNumGrusDone()) + "/" + 
      std::to_string(gru_struc_->getNumGrus()) + " -- GlobalGRU=" + 
      std::to_string(gru_struc_->getGRU(job_index)->getIndexNetcdf()) + 
      " -- LocalGRU=" + 
      std::to_string(gru_struc_->getGRU(job_index)->getIndexJob()) + 
      " -- NumFailed=" + std::to_string(gru_struc_->getNumGRUFailed());
  logger_->log(update_str);
  self_->println(update_str);

  if (gru_struc_->isDone()) {
    gru_struc_->hasFailures() && gru_struc_->shouldRetry() ?
      self_->send(self_, restart_failures_v) : self_->send(self_, finalize_v);
  }
}



void JobActor::finalizeJob() {
  self_->request(file_access_actor_, infinite, finalize_v).await(
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
        auto num_failed_grus = gru_struc_->getNumGRUFailed();    
        self_->send(parent_, done_job_v, num_failed_grus, 
                    total_duration, std::get<0>(read_write_duration), 
                    std::get<1>(read_write_duration));
        self_->quit();
    });
}



// ERROR HANDLING FUNCTIONS

void JobActor::handleGRUError(int err_code, int job_index, int timestep, 
                              std::string& err_msg) {
  gru_struc_->getGRU(job_index)->setFailed();
}



void JobActor::handleFileAccessError(int err_code, std::string& err_msg) {
  logger_->log("Job Actor: File_Access_Actor Error:" + err_msg);
  self_->println("Job Actor: File_Access_Actor Error: {}", err_msg);
  if (err_code != -1) {
    logger_->log("Job_Actor: Have to Quit");
    self_->println("Job_Actor: Have to Quit");
    self_->quit();
    return;
  }
}
