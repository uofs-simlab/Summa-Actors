#include "job_actor.hpp"

using json = nlohmann::json;
using chrono_time = std::chrono::time_point<std::chrono::system_clock>;

namespace caf {

// First Actor that is spawned that is not the Coordinator Actor.
behavior job_actor(stateful_actor<job_state>* self, 
    int start_gru, int num_gru, 
    File_Access_Actor_Settings file_access_actor_settings, 
    Job_Actor_Settings job_actor_settings, 
    HRU_Actor_Settings hru_actor_settings, caf::actor parent) {
    
  self->set_down_handler([=](const down_msg& dm) {
      aout(self) << "\n\n ********** DOWN HANDLER ********** \n"
                 << "Lost Connection With A Connected Actor\n"
                 << "Reason: " << to_string(dm.reason) << "\n";
  });

  self->set_exit_handler([=](const exit_msg& em) {
      aout(self) << "\n\n ********** EXIT HANDLER ********** \n"
                 << "Exit Reason: " << to_string(em.reason) << "\n";
  });

  // Timing Information
  self->state.job_timing = TimingInfo(); 
  self->state.job_timing.addTimePoint("total_duration");
  self->state.job_timing.updateStartPoint("total_duration");
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
  
  /* Calls: summa_SetTimesDirsAndFiles(), summa_defineGlobalData(),
         read_icond_nlayers(), Allocates time structures */
  self->state.job_timing.addTimePoint("init_duration");
  int file_gru = 0;
  int err = 0;
  job_init_fortran(self->state.job_actor_settings.file_manager_path.c_str(),
      &self->state.start_gru, &self->state.num_gru, &self->state.num_hru, 
      &file_gru, &err);
  if (err != 0) { 
    aout(self) << "\nERROR: Job_Actor - job_init_fortran\n"; 
    return {};
  }

  self->state.num_gru_info = NumGRUInfo(self->state.start_gru, 
      self->state.start_gru, self->state.num_gru, self->state.num_gru, 
      file_gru, false);

  // Spawn File Access Actor and await confirmation it is ready
  self->state.file_access_actor = self->spawn(file_access_actor, 
      self->state.num_gru_info, self->state.file_access_actor_settings, self);
  // self->send(self->state.file_access_actor, def_output_v, file_gru);
  self->request(self->state.file_access_actor, caf::infinite, 
      init_file_access_actor_v, file_gru).await(
        [=](int num_timesteps){
    
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
    /*** From file access actor after it spawns ***/
    // [=](init_file_access_actor, int num_timesteps) {
    //   self->state.num_steps = num_timesteps;
    //   aout(self) << "Num Steps: " << self->state.num_steps << "\n";
      
    //   // #####################################################
    //   // # Data Assimilation Mode
    //   // #####################################################
    //   if (self->state.job_actor_settings.data_assimilation_mode) {
    //     aout(self) << "Job_Actor: Data Assimilation Mode\n";

    //     auto& gru_container = self->state.gru_container;
        
    //     // Spawn HRUs in batches or individually
    //     if (self->state.job_actor_settings.batch_size > 1)
    //       spawnHRUBatches(self);
    //     else
    //       spawnHRUActors(self);
        

    //     aout(self) << "GRUs Initialized\n";
    //     self->send(self->state.file_access_actor, access_forcing_v, 
    //                self->state.iFile, self);
    //   } else {
    //     // #####################################################
    //     // # Normal Mode
    //     // #####################################################
    //     aout(self) << "Job_Actor: Normal Mode\n";
    //     spawnHRUActors(self);
    //   }
    // },

    // #####################################################
    // # Data Assimilation Mode Start
    // #####################################################
    // [=](new_forcing_file, int num_steps_in_iFile, int nextFile) {
    //   aout(self) << "Job_Actor: New Forcing File\n";
    //   self->state.iFile = nextFile;
    //   self->state.stepsInCurrentFFile = num_steps_in_iFile;
    //   self->state.forcingStep = 1;
    //   for(auto gru : self->state.gru_container.gru_list) {
    //     self->send(gru->getGRUActor(), update_timeZoneOffset_v, 
    //         self->state.iFile);
    //   }

    //   self->send(self, update_hru_v); // update HRUs
    // },

    // [=](update_hru){
    //   // aout(self) << "Job_Actor: Updating HRUs\n";
    //   for(auto gru : self->state.gru_container.gru_list) {
    //     self->send(gru->getGRUActor(), update_hru_v, 
    //                 self->state.timestep, self->state.forcingStep);
    //   }      
    // },





    [=](reinit_hru) {
      aout(self) << "Job_Actor: HRU Actor Re-initialized\n";
      self->send(self, update_hru_v);
    },

    // [=](std::vector<actor> hru_actors) {
    // },

    // #####################################################
    // # Data Assimilation Mode End
    // #####################################################




    // #####################################################
    // # Normal Mode Start
    // #####################################################
    // [=](done_hru, int local_gru_index) {
    //   auto& gru_container = self->state.gru_container;
    //   using namespace std::chrono;
      
    //   chrono_time end_point = high_resolution_clock::now();
    //   double total_duration = duration_cast<seconds>(end_point - 
    //       gru_container.gru_start_time).count();
    //   gru_container.num_gru_done++;

    //   aout(self) << "GRU Finished: " << gru_container.num_gru_done << "/" 
    //       << gru_container.num_gru_in_run_domain << " -- "
    //       << "GlobalGRU=" 
    //       << gru_container.gru_list[local_gru_index-1]->getGlobalGRUIndex()
    //       << " -- LocalGRU=" << local_gru_index << "\n";

    //   // Update Timing
    //   gru_container.gru_list[local_gru_index-1]->setRunTime(total_duration);
    //   gru_container.gru_list[local_gru_index-1]->setInitDuration(-1);
    //   gru_container.gru_list[local_gru_index-1]->setForcingDuration(-1);
    //   gru_container.gru_list[local_gru_index-1]->setRunPhysicsDuration(-1);
    //   gru_container.gru_list[local_gru_index-1]->setWriteOutputDuration(-1);

    //   gru_container.gru_list[local_gru_index-1]->setSuccess();


      
    //   // Check if all GRUs are finished
    //   if (gru_container.num_gru_done >= gru_container.num_gru_in_run_domain) {
    //     // Check for failures
    //     if(self->state.gru_container.num_gru_failed == 0 || 
    //         self->state.max_run_attempts == 1) {
    //       self->send(self, finalize_v); 
    //     } else {
    //       self->send(self, restart_failures_v);
    //     }
    //   }

    // },

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
          auto global_gru_index = GRU->getGlobalGRUIndex();
          auto local_gru_index = GRU->getLocalGRUIndex();
          auto gru_actor = self->spawn(hru_actor, 
                                        global_gru_index, 
                                        local_gru_index, 
                                        self->state.hru_actor_settings,
                                        self->state.file_access_actor, 
                                        self);
          self->state.gru_container.gru_list[local_gru_index-1]->
              setGRUActor(gru_actor);
        }
      }
    },

    // [=](finalize) {            
    //   std::vector<serializable_netcdf_gru_actor_info> 
    //       netcdf_gru_info = getGruNetcdfInfo(
    //         self->state.max_run_attempts,self->state.gru_container.gru_list);
        
            
    //   self->state.num_gru_failed = std::count_if(netcdf_gru_info.begin(), 
    //       netcdf_gru_info.end(), [](auto& gru_info) {
    //     return !gru_info.successful;
    //   });

    //   self->request(self->state.file_access_actor, infinite, finalize_v).await(
    //     [=](std::tuple<double, double> read_write_duration) {
    //       int err = 0;
    //       for (auto GRU : self->state.gru_container.gru_list) {
    //         delete GRU;
    //       }
    //       self->state.gru_container.gru_list.clear();

    //       self->state.job_timing.updateEndPoint("total_duration");

    //       aout(self) << "\n________________" 
    //                  << "PRINTING JOB_ACTOR TIMING INFO RESULTS"
    //                  << "________________\n"
    //                  << "Total Duration = "
    //                  << self->state.job_timing.getDuration("total_duration")
    //                      .value_or(-1.0) << " Seconds\n"
    //                  << "Total Duration = " 
    //                  << self->state.job_timing.getDuration("total_duration")
    //                      .value_or(-1.0) / 60 << " Minutes\n"
    //                  << "Total Duration = " 
    //                  << (self->state.job_timing.getDuration("total_duration")
    //                     .value_or(-1.0) / 60) / 60 << " Hours\n"
    //                  << "Job Init Duration = " 
    //                   << self->state.job_timing.getDuration("init_duration")
    //                       .value_or(-1.0) << " Seconds\n"
    //                  << "_________________________________" 
    //                  << "_______________________________________\n\n";

    //       deallocateJobActor(&err);

    //         // Tell Parent we are done
    //       self->send(self->state.parent, done_job_v, self->state.num_gru_failed, 
    //           self->state.job_timing.getDuration("total_duration")
    //               .value_or(-1.0),
    //           std::get<0>(read_write_duration), 
    //           std::get<1>(read_write_duration));
    //       self->quit();

    //   });
    // },

    // Handle Sundials Error
    [=](err_atom, caf::actor src, double rtol, double atol) {
      self->state.hru_actor_settings.rel_tol = rtol;
      self->state.hru_actor_settings.abs_tol = atol;
      handleGRUError(self, src);
    },

    [=](const error& err, caf::actor src) {
      
      aout(self) << "\n\n ********** ERROR HANDLER \n";
      
      switch(err.category()) {
        
        case type_id_v<hru_error>:
          aout(self) << "HRU Error: " << to_string(err) << "\n";
          handleGRUError(self, src);

          break;
        case type_id_v<file_access_error>:
          if (err == file_access_error::mDecisions_error) {
            aout(self) << "Check mDecisions File For Correctness";
          } else {
            aout(self) << "File Access Error: " << to_string(err) << "No Handling Implemented\n";
          }
          for (auto GRU : self->state.gru_container.gru_list) {
            self->send_exit(GRU->getGRUActor(), exit_reason::user_shutdown);
          }
          self->quit();
          break;
        default:
          aout(self) << "Unknown Error: " << to_string(err) << "\n";
          break;
      }
    },
  };
}







std::vector<serializable_netcdf_gru_actor_info> getGruNetcdfInfo(
    int max_run_attempts, std::vector<GRU*> &gru_list) {
  std::vector<serializable_netcdf_gru_actor_info> gru_netcdf_info;
  
  for(auto gru : gru_list) {
    serializable_netcdf_gru_actor_info gru_info;
    gru_info.run_time = gru->getRunTime();
    gru_info.init_duration = gru->getInitDuration();
    gru_info.forcing_duration = gru->getForcingDuration();
    gru_info.run_physics_duration = gru->getRunPhysicsDuration();
    gru_info.write_output_duration = gru->getWriteOutputDuration();
    
    gru_info.num_attempts = max_run_attempts - gru->getAttemptsLeft() + 1;
    gru_info.successful = is_success(gru->getStatus());
    gru_info.rel_tol = gru->getRelTol();
    gru_info.abs_tol = gru->getAbsTol();

    gru_netcdf_info.push_back(gru_info);
  }
  
  return gru_netcdf_info;
}




void handleGRUError(stateful_actor<job_state>* self, caf::actor src) {
  auto it = std::find_if(self->state.gru_container.gru_list.begin(), 
                          self->state.gru_container.gru_list.end(),
                          [src](auto& gru) {
                          return gru->getGRUActor() == src;
                        });

  if (it != self->state.gru_container.gru_list.end()) {
    (*it)->setFailed();
    (*it)->decrementAttemptsLeft();

    self->state.gru_container.num_gru_done++;
    self->state.gru_container.num_gru_failed++;
    self->send(self->state.file_access_actor, run_failure_v, (*it)->getLocalGRUIndex());
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

} // End Namespace caf


