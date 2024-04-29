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

  self->state.num_gru_info = NumGRUInfo(self->state.start_gru, 
                                        self->state.start_gru, 
                                        self->state.num_gru, 
                                        self->state.num_gru, 
                                        gru_struc->get_file_gru(), 
                                        false);

  self->state.file_access_actor = self->spawn(
      file_access_actor, self->state.num_gru_info, 
      self->state.file_access_actor_settings, self);
  self->request(self->state.file_access_actor, caf::infinite, 
                init_file_access_actor_v, gru_struc->get_file_gru())
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
      

  /**
   * TODO: This is where the error handling code can go
   * We can add a timeout to the receive and if we do not receive it in 
   * Time then we have to handle that error here
  */


  return {};
  


  return {

    [=](reinit_hru) {
      aout(self) << "Job_Actor: HRU Actor Re-initialized\n";
      self->send(self, update_hru_v);
    },

    [=](restart_failures) {
      aout(self) << "Job_Actor: Restarting GRUs that Failed\n";

      self->state.gru_container.num_gru_done = 0;
      self->state.gru_container.num_gru_in_run_domain = 
          self->state.gru_container.num_gru_failed;
      self->state.gru_container.num_gru_failed = 0;
      
      // notify file_access_actor
      self->send(self->state.file_access_actor, restart_failures_v); 

      // Set Sundials tolerance or decrease timestep length
      if (self->state.hru_actor_settings.rel_tol > 0 && 
          self->state.hru_actor_settings.abs_tol > 0) {
        self->state.hru_actor_settings.rel_tol /= 10;
        self->state.hru_actor_settings.abs_tol /= 10;
      } else {
        self->state.hru_actor_settings.dt_init_factor *= 2;
      }


      for(auto GRU : self->state.gru_container.gru_list) {
        if(GRU->isFailed()) {
          GRU->setRunning();
          GRU->decrementAttemptsLeft();
          auto gru_actor = self->spawn(hru_actor, GRU->getIndexNetcdf(), 
                                       GRU->getIndexJob(),
                                       self->state.hru_actor_settings,
                                       self->state.file_access_actor, self);
          self->state.gru_container.gru_list[GRU->getIndexJob()-1]->
              setActorRef(gru_actor);
        }
      }
    },

    // Handle Sundials Error
    [=](err_atom, caf::actor src, double rtol, double atol) {
      self->state.hru_actor_settings.rel_tol = rtol;
      self->state.hru_actor_settings.abs_tol = atol;
      handleGRUError(self, src);
    },

    // [=](const error& err, caf::actor src) {
      
    //   aout(self) << "\n\n ********** ERROR HANDLER \n";
      
    //   switch(err.category()) {
        
    //     case type_id_v<hru_error>:
    //       aout(self) << "HRU Error: " << to_string(err) << "\n";
    //       handleGRUError(self, src);

    //       break;
    //     case type_id_v<file_access_error>:
    //       if (err == file_access_error::mDecisions_error) {
    //         aout(self) << "Check mDecisions File For Correctness";
    //       } else {
    //         aout(self) << "File Access Error: " << to_string(err) << "No Handling Implemented\n";
    //       }
    //       for (auto GRU : self->state.gru_container.gru_list) {
    //         self->send_exit(GRU->getGRUActor(), exit_reason::user_shutdown);
    //       }
    //       self->quit();
    //       break;
    //     default:
    //       aout(self) << "Unknown Error: " << to_string(err) << "\n";
    //       break;
    //   }
    // },
  };
}






void handleGRUError(stateful_actor<job_state>* self, caf::actor src) {
  auto it = std::find_if(self->state.gru_container.gru_list.begin(), 
                          self->state.gru_container.gru_list.end(),
                          [src](auto& gru) {
                          return gru->getActorRef() == src;
                        });

  if (it != self->state.gru_container.gru_list.end()) {
    (*it)->setFailed();
    (*it)->decrementAttemptsLeft();

    self->state.gru_container.num_gru_done++;
    self->state.gru_container.num_gru_failed++;
    self->send(self->state.file_access_actor, run_failure_v, (*it)->getIndexJob());
  } else {
    aout(self) << "ERROR: Job_Actor: Could not find GRU in GRU_Container\n";
  }

  // Check if all GRUs are finished
  if (self->state.gru_container.num_gru_done >= self->state.gru_container.num_gru_in_run_domain) {
    // Check for failures
    if(self->state.gru_container.num_gru_failed == 0 || self->state.max_run_attempts == 1) {
      self->send(self, finalize_v); 
    } else {
      self->send(self, restart_failures_v);
    }
  }

}



