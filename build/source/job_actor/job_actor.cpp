#include "job_actor.hpp"

using json = nlohmann::json;
using chrono_time = std::chrono::time_point<std::chrono::system_clock>;
using namespace caf;


behavior JobActor::make_behavior() {
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

  gru_struc_ = std::make_unique<GruStruc>(batch_.getStartHRU(), 
      batch_.getNumHRU(), job_actor_settings_.max_run_attempts_);
  if (gru_struc_->ReadDimension()) {
    std::string err_msg = "ERROR: Job_Actor - ReadDimension\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  if (gru_struc_->ReadIcondNlayers()) {
    std::string err_msg = "ERROR: Job_Actor - ReadIcondNlayers\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  gru_struc_->getNumHrusPerGru();  

  summa_init_struc_ = std::make_unique<SummaInitStruc>();
  if (summa_init_struc_->allocate(batch_.getNumHRU()) != 0) {
    std::string err_msg = "ERROR -- Job_Actor: SummaInitStruc allocation failed\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  if (summa_init_struc_->summa_paramSetup() != 0) {
    std::string err_msg = "ERROR -- Job_Actor: SummaInitStruc paramSetup failed\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  if (summa_init_struc_->summa_readRestart()!= 0) {
    std::string err_msg = "ERROR -- Job_Actor: SummaInitStruc readRestart failed\n";
    self_->send(parent_, err_atom_v, -2, err_msg);
    return {};
  }
  summa_init_struc_->getInitTolerance(hru_actor_settings_);
  

  num_gru_info_ = NumGRUInfo(batch_.getStartHRU(), batch_.getStartHRU(), 
                            batch_.getNumHRU(), batch_.getNumHRU(), 
                            gru_struc_->get_file_gru(), false);
  file_access_actor_ = self_->spawn(actor_from_state<FileAccessActor>, 
                                    num_gru_info_, fa_actor_settings_, self_);

//   file_access_actor_ = self->spawn(
//       file_access_actor, self->state.num_gru_info, 
//       self->state.file_access_actor_settings, self);

//   self->request(self->state.file_access_actor, caf::infinite, 
//                 init_file_access_actor_v, gru_struc->get_file_gru(),
//                 gru_struc->getNumHrus())
//       .await([=](int num_timesteps){
//     if (num_timesteps < 0) {
//       std::string err_msg = "ERROR: Job_Actor: File Access Actor Not Ready\n";
//       self->send(self->state.parent, err_atom_v, -2, err_msg);
//       self->quit();
//       return;
//     } 
//     self->state.job_timing.updateEndPoint("init_duration");
    
//     self->state.logger->log("Job Actor Initialized");
//     aout(self) << "Job Actor Initialized \n";

//     job_actor_settings.data_assimilation_mode ? 
//         self->become(data_assimilation_mode(self)) : 
//         self->become(async_mode(self));
    
//     // Start the specific mode
//     self->send(self, file_access_actor_ready_v, num_timesteps);
//   });
      
//   return {};

  return {};
}


