#include "job_actor.hpp"
#include "file_access_actor.hpp"
#include "json.hpp"
#include <chrono>
#include <thread>
#include "message_atoms.hpp"
#include "job_actor_subroutine_wrappers.hpp"
#include "hru_actor.hpp"
#include "gru_actor.hpp"

using json = nlohmann::json;
using chrono_time = std::chrono::time_point<std::chrono::system_clock>;


namespace caf {

// First Actor that is spawned that is not the Coordinator Actor.
behavior job_actor(stateful_actor<job_state>* self, 
                   int start_gru, int num_gru, 
                   File_Access_Actor_Settings file_access_actor_settings, 
                   Job_Actor_Settings job_actor_settings, 
                   HRU_Actor_Settings hru_actor_settings, 
                   caf::actor parent) {
    
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

    // hostname information that is useful for verifying that the job actor is running on the correct node
    char host[HOST_NAME_MAX];
    gethostname(host, HOST_NAME_MAX);
    self->state.hostname = host;
    
    // Initalize global variables calling Fortran Routines
    int err = 0;


    /*
    Calls: 
      - summa_SetTimesDirsAndFiles()
      - summa_defineGlobalData()
      - read_icond_nlayers()
      - Allocates time structures
    */
    job_init_fortran(self->state.job_actor_settings.file_manager_path.c_str(),
                     &self->state.start_gru,
                     &self->state.num_gru,
                     &self->state.num_hru,
                     &err);
    if (err != 0) { aout(self) << "\nERROR: Job_Actor - job_init_fortran\n"; return {}; }
    

    // Spawn the file_access_actor. This will return the number of forcing files we are working with
    self->state.file_access_actor = self->spawn(file_access_actor, 
                                                self->state.start_gru, 
                                                self->state.num_gru, 
                                                self->state.file_access_actor_settings, 
                                                self);


    aout(self) << "Job Actor Initialized \n";

    return {

        [=](init_gru) {
          auto& gru_container = self->state.gru_container;

          gru_container.gru_start_time = std::chrono::high_resolution_clock::now();
          gru_container.run_attempts_left = self->state.max_run_attempts;
          gru_container.run_attempts_left--;


          // Spawn the GRUs
          for(int i = 0; i < gru_container.num_gru_in_run_domain; i++) {
            auto global_gru_index = gru_container.gru_list.size() + self->state.start_gru;
            auto local_gru_index = gru_container.gru_list.size() + 1; // Fortran reference starts at 1

            auto gru = self->spawn(hru_actor, 
                                    global_gru_index,
                                    local_gru_index,               
                                    self->state.hru_actor_settings,                                
                                    self->state.file_access_actor, 
                                    self);

            // Create the GRU object (Job uses this to keep track of GRU status)
            gru_container.gru_list.push_back(new GRU(global_gru_index, 
                                                     local_gru_index, 
                                                     gru, 
                                                     self->state.dt_init_start_factor, 
                                                     self->state.hru_actor_settings.rel_tol,
                                                     self->state.hru_actor_settings.abs_tol,
                                                     self->state.max_run_attempts));    
          }
        }, // end init_gru

        [=](done_hru, int local_gru_index) {
          auto& gru_container = self->state.gru_container;
          using namespace std::chrono;
          
          chrono_time end_point = high_resolution_clock::now();
          double total_duration = duration_cast<seconds>(end_point - gru_container.gru_start_time).count();

          aout(self) << "\nJob_Actor: GRU Finished: \n" 
                     << "          global_gru_index = " 
                     << gru_container.gru_list[local_gru_index-1]->getGlobalGRUIndex() << "\n"
                     << "          local_gru_index = " << local_gru_index << "\n"
                     << "          total_duration = " << total_duration << "\n\n";
          // Update Timing
          gru_container.gru_list[local_gru_index-1]->setRunTime(total_duration);
          gru_container.gru_list[local_gru_index-1]->setInitDuration(-1);
          gru_container.gru_list[local_gru_index-1]->setForcingDuration(-1);
          gru_container.gru_list[local_gru_index-1]->setRunPhysicsDuration(-1);
          gru_container.gru_list[local_gru_index-1]->setWriteOutputDuration(-1);

          gru_container.gru_list[local_gru_index-1]->setSuccess();

          gru_container.num_gru_done++;

          
          // Check if all GRUs are finished
          if (gru_container.num_gru_done >= gru_container.num_gru_in_run_domain) {
            // Check for failures
            if(self->state.gru_container.num_gru_failed == 0 || self->state.max_run_attempts == 1) {
              self->send(self, finalize_v); 
            } else {
              self->send(self, restart_failures_v);
            }
          }

        },

        [=](restart_failures) {
          aout(self) << "Job_Actor: Restarting GRUs that Failed\n";

          self->state.gru_container.num_gru_done = 0;
          self->state.gru_container.num_gru_in_run_domain = self->state.gru_container.num_gru_failed;
          self->state.gru_container.num_gru_failed = 0;

          self->send(self->state.file_access_actor, restart_failures_v); // notify file_access_actor

          for(auto GRU : self->state.gru_container.gru_list) {
            if(GRU->isFailed()) {
              GRU->setRunning();
              GRU->decrementAttemptsLeft();
              self->state.hru_actor_settings.dt_init_factor *= 2;
              auto global_gru_index = GRU->getGlobalGRUIndex();
              auto local_gru_index = GRU->getLocalGRUIndex();
              auto gru_actor = self->spawn(hru_actor, 
                                           global_gru_index, 
                                           local_gru_index, 
                                           self->state.hru_actor_settings,
                                           self->state.file_access_actor, 
                                           self);
              self->state.gru_container.gru_list[local_gru_index-1]->setGRUActor(gru_actor);
            }
          }
        },

        [=](finalize) {
            
            std::vector<serializable_netcdf_gru_actor_info> 
                netcdf_gru_info = getGruNetcdfInfo(self->state.max_run_attempts,self->state.gru_container.gru_list);
              
              
            
            self->state.num_gru_failed = std::count_if(netcdf_gru_info.begin(), netcdf_gru_info.end(), [](auto& gru_info) {
                  return !gru_info.successful;
              });

            self->request(self->state.file_access_actor, 
                          infinite,
                          finalize_v, netcdf_gru_info)
              .await(
                [=](std::tuple<double, double> read_write_duration) {
                
                  int err = 0;
              
                  
                  for (auto GRU : self->state.gru_container.gru_list) {
                    delete GRU;
                  }
                  self->state.gru_container.gru_list.clear();
                  
                  self->state.job_timing.updateEndPoint("total_duration");

                  aout(self) << "\n________________PRINTING JOB_ACTOR TIMING INFO RESULTS________________\n"
                             << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) << " Seconds\n"
                             << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60 << " Minutes\n"
                             << "Total Duration = " << (self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60) / 60 << " Hours\n"
                             << "________________________________________________________________________\n\n";

                  deallocateJobActor(&err);

                    // Tell Parent we are done
                  self->send(self->state.parent, 
                              done_job_v, 
                              self->state.num_gru_failed, 
                              self->state.job_timing.getDuration("total_duration").value_or(-1.0),
                              std::get<0>(read_write_duration), 
                              std::get<1>(read_write_duration));
                  self->quit();

            });

        },

        [=](const error& err, caf::actor src) {
          
          aout(self) << "\n\n ********** ERROR HANDLER \n";
          
          switch(err.category()) {
            
            case type_id_v<hru_error>:
              aout(self) << "HRU Error: " << to_string(err) << "\n";
              handleGRUError(self, src);

              break;
            case type_id_v<file_access_error>:
              aout(self) << "File Access Error: " << to_string(err) << "No Handling Implemented\n";
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

std::vector<serializable_netcdf_gru_actor_info> getGruNetcdfInfo(int max_run_attempts, std::vector<GRU*> &gru_list) {

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


