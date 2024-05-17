#include "job_actor.hpp"

using json = nlohmann::json;
using chrono_time = std::chrono::time_point<std::chrono::system_clock>;
using namespace caf;

// First Actor that is spawned that is not the Coordinator Actor.
behavior job_actor(stateful_actor<job_state>* self, int start_gru, int num_gru, 
                   File_Access_Actor_Settings file_access_actor_settings, 
                   Job_Actor_Settings job_actor_settings, 
                   HRU_Actor_Settings hru_actor_settings, caf::actor parent) {
    
  self->set_down_handler([=](const down_msg& dm) {
      aout(self) << "\n\n ********** DOWN HANDLER ********** \n"
                 << "Lost Connection With A Connected Actor\n"
                 << "Reason: " << to_string(dm.reason) << "\n";
  });

  self->set_exit_handler([=](const caf::exit_msg& em) {
      aout(self) << "\n\n ********** EXIT HANDLER ********** \n"
                 << "Exit Reason: " << to_string(em.reason) << "\n";
  });

  // Timing Information
  self->state.job_timing = TimingInfo(); 
  self->state.job_timing.addTimePoint("total_duration");
  self->state.job_timing.updateStartPoint("total_duration");
  self->state.job_timing.addTimePoint("init_duration");
  self->state.job_timing.updateStartPoint("init_duration");
  // Set Job Variables
  self->state.start_gru = start_gru;
  self->state.num_gru = num_gru;
  self->state.parent = parent;
  // Set the settings variables
  self->state.file_access_actor_settings = file_access_actor_settings;
  self->state.job_actor_settings = job_actor_settings;
  self->state.hru_actor_settings = hru_actor_settings;
  self->state.max_run_attempts = job_actor_settings.max_run_attempts;
  // Init the GRU Container
  self->state.gru_container.num_gru_in_run_domain = num_gru;

  char host[HOST_NAME_MAX];
  gethostname(host, HOST_NAME_MAX);
  self->state.hostname = host;

  auto& gru_struc = self->state.gru_struc;
  gru_struc = std::make_unique<GruStruc>(self->state.start_gru, 
                                         self->state.num_gru,
                                         job_actor_settings.max_run_attempts);
  if (gru_struc->ReadDimension()) {
    aout(self) << "ERROR: Job_Actor - ReadDimension\n";
    return {};
  }
  if (gru_struc->ReadIcondNlayers()) {
    aout(self) << "ERROR: Job_Actor - ReadIcondNlayers\n";
    return {};
  }
  gru_struc->getNumHrusPerGru();

  self->state.summa_init_struc = std::make_unique<SummaInitStruc>();
  if (self->state.summa_init_struc->allocate(self->state.num_gru) != 0) {
    aout(self) << "ERROR -- Job_Actor: SummaInitStruc allocation failed\n";
    return {};
  }
  if (self->state.summa_init_struc->summa_paramSetup() != 0) {
    aout(self) << "ERROR -- Job_Actor: SummaInitStruc paramSetup failed\n";
    return {};
  }
  if (self->state.summa_init_struc->summa_readRestart()!= 0) {
    aout(self) << "ERROR -- Job_Actor: SummaInitStruc readRestart failed\n";
    return {};
  }
  self->state.summa_init_struc->getInitTolerance(self->state.hru_actor_settings);

  self->state.num_gru_info = NumGRUInfo(self->state.start_gru, 
                                        self->state.start_gru, 
                                        self->state.num_gru, 
                                        self->state.num_gru, 
                                        gru_struc->get_file_gru(), 
                                        false);

  self->state.file_access_actor = self->spawn(file_access_actor, 
                                              self->state.num_gru_info, 
                                              self->state.file_access_actor_settings, 
                                              self);
  self->request(self->state.file_access_actor, caf::infinite, 
                init_file_access_actor_v, gru_struc->get_file_gru(),
                gru_struc->getNumHrus())
      .await([=](int num_timesteps){
    
    if (num_timesteps < 0) {
      aout(self) << "ERROR: Job_Actor: File Access Actor Not Ready\n"
                 << "\t VALUE: " << num_timesteps << "\n";
      self->quit();
      return;
    }

    aout(self) << "Job_Actor: File Access Actor Ready\n";  
    self->state.job_timing.updateEndPoint("init_duration");
    aout(self) << "Job Actor Initialized \n";

    job_actor_settings.data_assimilation_mode ? 
        self->become(data_assimilation_mode(self)) : 
        self->become(async_mode(self));
    
    // Start the specific mode
    self->send(self, file_access_actor_ready_v, num_timesteps);
  });
      
  return {};

}

