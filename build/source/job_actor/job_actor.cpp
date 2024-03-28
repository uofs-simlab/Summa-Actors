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
      aout(self) << "\n\n ********** DOWN HANDLER ********** \n";
      aout(self) << "Lost Connection With A Connected Actor\n";
      aout(self) << "Reason: " << to_string(dm.reason) << "\n";
  });

  self->set_exit_handler([=](const exit_msg& em) {
      aout(self) << "\n\n ********** EXIT HANDLER ********** \n";
      aout(self) << "Exit Reason: " << to_string(em.reason) << "\n";
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
  
  // Initalize global variables calling Fortran Routines
  int err = 0;


  /*
  Calls: 
    - summa_SetTimesDirsAndFiles
    - summa_defineGlobalData
    - read_icond_nlayers
    - Allocates time structures
  */
  int file_gru = 0;
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

  // Spawn the file_access_actor.
  self->state.file_access_actor = self->spawn(file_access_actor, 
      self->state.num_gru_info, 
      self->state.file_access_actor_settings, self);
  self->send(self->state.file_access_actor, def_output_v, file_gru);


  aout(self) << "Job Actor Initialized \n";

  return {
    /*** From file access actor after it spawns ***/
    [=](init_file_access_actor, int num_timesteps) {
      self->state.num_steps = num_timesteps;
      aout(self) << "Num Steps: " << self->state.num_steps << "\n";
      
      // #####################################################
      // # Data Assimilation Mode
      // #####################################################
      if (self->state.job_actor_settings.data_assimilation_mode) {
        aout(self) << "Job_Actor: Data Assimilation Mode\n";

        auto& gru_container = self->state.gru_container;
        
        // Spawn HRUs in batches or individually
        if (self->state.job_actor_settings.batch_size > 1)
          spawnHRUBatches(self);
        else
          spawnHRUActors(self, false);
        

        aout(self) << "GRUs Initialized\n";
        self->send(self->state.file_access_actor, access_forcing_v, 
                   self->state.iFile, self);
      } else {
        // #####################################################
        // # Normal Mode
        // #####################################################
        aout(self) << "Job_Actor: Normal Mode\n";
        spawnHRUActors(self, true);
      }
    },

    // #####################################################
    // # Data Assimilation Mode Start
    // #####################################################
    [=](new_forcing_file, int num_steps_in_iFile, int nextFile) {
      aout(self) << "Job_Actor: New Forcing File\n";
      self->state.iFile = nextFile;
      self->state.stepsInCurrentFFile = num_steps_in_iFile;
      self->state.forcingStep = 1;
      for(auto gru : self->state.gru_container.gru_list) {
        self->send(gru->getGRUActor(), update_timeZoneOffset_v, 
            self->state.iFile);
      }

      self->send(self, update_hru_v); // update HRUs
    },

    [=](update_hru){
      // aout(self) << "Job_Actor: Updating HRUs\n";
      for(auto gru : self->state.gru_container.gru_list) {
        self->send(gru->getGRUActor(), update_hru_v, 
                    self->state.timestep, self->state.forcingStep);
      }      
    },

    [=](done_update){
      self->state.num_gru_done_timestep++;
      
      if (self->state.num_gru_done_timestep >= 
          self->state.gru_container.gru_list.size()) {
        aout(self) << "Job_Actor: Done Update for timestep:" 
                   << self->state.timestep << "\n";

        // write the output
        int steps_to_write = 1;
        int start_gru = 1;
        self->request(self->state.file_access_actor, caf::infinite,
          write_output_v, steps_to_write, start_gru, self->state.num_gru).await(
          [=](int err) {
            if (err != 0) {
              aout(self) << "Job_Actor: Error Writing Output\n";
              for (auto GRU : self->state.gru_container.gru_list)
                self->send(GRU->getGRUActor(), exit_msg_v);
              
              self->send_exit(self->state.file_access_actor, 
                              exit_reason::user_shutdown);
              self->quit();
            } 
          });

        self->state.timestep++;
        self->state.forcingStep++;

        // Check if we are done the simulation
        if (self->state.timestep > self->state.num_steps) {
          aout(self) << "Job_Actor: Done Job\n";
          for (auto GRU : self->state.gru_container.gru_list) {
            self->send_exit(GRU->getGRUActor(), exit_reason::user_shutdown);
            GRU->setSuccess();
          }
          self->send(self, finalize_v);
        } else if (self->state.forcingStep > self->state.stepsInCurrentFFile) {
        // Check if we need another forcing file
          aout(self) << "Job_Actor: Done Forcing Step\n";
          self->send(self->state.file_access_actor, access_forcing_v, 
              self->state.iFile+1, self);
        } else if (self->state.timestep == 5) {
          // Test serializing the HRU actor
          self->send(self->state.gru_container.gru_list[0]->getGRUActor(), 
              serialize_hru_v, 0);
        } else {
          // otherwise update all HRUs
          self->send(self, update_hru_v);
        }
        self->state.num_gru_done_timestep = 0;
      }
    },

    [=](serialize_hru, hru hru_data) {
      aout(self) << "Job_Actor: Recieved HRU Data\n";
      auto sender = actor_cast<actor>(self->current_sender());

      self->send(sender, reinit_hru_v, hru_data);
    },

    [=](reinit_hru) {
      aout(self) << "Job_Actor: HRU Actor Re-initialized\n";
      self->send(self, update_hru_v);
    },

    // #####################################################
    // # Data Assimilation Mode End
    // #####################################################




    // #####################################################
    // # Normal Mode Start
    // #####################################################
    [=](done_hru, int local_gru_index) {
      auto& gru_container = self->state.gru_container;
      using namespace std::chrono;
      
      chrono_time end_point = high_resolution_clock::now();
      double total_duration = duration_cast<seconds>(end_point - 
          gru_container.gru_start_time).count();
      gru_container.num_gru_done++;

      aout(self) << "GRU Finished: " << gru_container.num_gru_done << "/" 
          << gru_container.num_gru_in_run_domain << " -- "
          << "GlobalGRU=" 
          << gru_container.gru_list[local_gru_index-1]->getGlobalGRUIndex()
          << " -- LocalGRU=" << local_gru_index << "\n";

      // Update Timing
      gru_container.gru_list[local_gru_index-1]->setRunTime(total_duration);
      gru_container.gru_list[local_gru_index-1]->setInitDuration(-1);
      gru_container.gru_list[local_gru_index-1]->setForcingDuration(-1);
      gru_container.gru_list[local_gru_index-1]->setRunPhysicsDuration(-1);
      gru_container.gru_list[local_gru_index-1]->setWriteOutputDuration(-1);

      gru_container.gru_list[local_gru_index-1]->setSuccess();


      
      // Check if all GRUs are finished
      if (gru_container.num_gru_done >= gru_container.num_gru_in_run_domain) {
        // Check for failures
        if(self->state.gru_container.num_gru_failed == 0 || 
            self->state.max_run_attempts == 1) {
          self->send(self, finalize_v); 
        } else {
          self->send(self, restart_failures_v);
        }
      }

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

    [=](finalize) {            
      std::vector<serializable_netcdf_gru_actor_info> 
          netcdf_gru_info = getGruNetcdfInfo(
            self->state.max_run_attempts,self->state.gru_container.gru_list);
        
            
      self->state.num_gru_failed = std::count_if(netcdf_gru_info.begin(), 
          netcdf_gru_info.end(), [](auto& gru_info) {
        return !gru_info.successful;
      });

      self->request(self->state.file_access_actor, infinite, finalize_v).await(
        [=](std::tuple<double, double> read_write_duration) {
          int err = 0;
          for (auto GRU : self->state.gru_container.gru_list) {
            delete GRU;
          }
          self->state.gru_container.gru_list.clear();

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
                     << "_________________________________" 
                     << "_______________________________________\n\n";

          deallocateJobActor(&err);

            // Tell Parent we are done
          self->send(self->state.parent, done_job_v, self->state.num_gru_failed, 
              self->state.job_timing.getDuration("total_duration")
                  .value_or(-1.0),
              std::get<0>(read_write_duration), 
              std::get<1>(read_write_duration));
          self->quit();

      });
    },

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


void spawnHRUActors(stateful_actor<job_state>* self, bool normal_mode) {
  auto& gru_container = self->state.gru_container;
  gru_container.gru_start_time = std::chrono::high_resolution_clock::now();
  gru_container.run_attempts_left = self->state.max_run_attempts;
  gru_container.run_attempts_left--;

  for (int i = 0; i < gru_container.num_gru_in_run_domain; i++) {
    auto global_gru_index = gru_container.gru_list.size() 
                            + self->state.start_gru;
    auto local_gru_index = gru_container.gru_list.size() + 1;                                

    auto gru = self->spawn(hru_actor, global_gru_index, local_gru_index,               
                           self->state.hru_actor_settings,                                
                           self->state.file_access_actor, self);

    // Create the GRU object (Job uses this to keep track of GRU status)
    gru_container.gru_list.push_back(new GRU(global_gru_index, 
                                     local_gru_index, gru, 
                                     self->state.dt_init_start_factor, 
                                     self->state.hru_actor_settings.rel_tol,
                                     self->state.hru_actor_settings.abs_tol,
                                     self->state.max_run_attempts));  
    
    if (normal_mode) self->send(gru, update_hru_async_v);
  }                        
          
}

void spawnHRUBatches(stateful_actor<job_state>* self) {
  aout(self) << "Job_Actor: Spawning HRU Batches\n";
  int batch_size;

  auto& gru_container = self->state.gru_container;
  gru_container.gru_start_time = std::chrono::high_resolution_clock::now();
  gru_container.run_attempts_left = self->state.max_run_attempts;
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
  int start_hru_global = self->state.start_gru;
  int start_hru_local = 1;

  while (remaining_hru_to_batch > 0) {
    int current_batch_size = std::min(batch_size, remaining_hru_to_batch);
    auto gru_batch = self->spawn(hru_batch_actor, start_hru_local,
        start_hru_global, current_batch_size, self->state.hru_actor_settings,
        self->state.file_access_actor, self);

    gru_container.gru_list.push_back(new GRU(start_hru_global, 
        start_hru_local, gru_batch, self->state.dt_init_start_factor, 
        self->state.hru_actor_settings.rel_tol,
        self->state.hru_actor_settings.abs_tol, self->state.max_run_attempts));  

    remaining_hru_to_batch -= current_batch_size;
    start_hru_local += current_batch_size;
    start_hru_global += current_batch_size;
  }
  aout(self) << "Number of HRU_Batch_Actors: " 
             << gru_container.gru_list.size() << "\n";
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


