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

namespace caf {

// First Actor that is spawned that is not the Coordinator Actor.
behavior job_actor(stateful_actor<job_state>* self, int start_gru, int num_gru, 
    File_Access_Actor_Settings file_access_actor_settings, Job_Actor_Settings job_actor_settings, 
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

    // Init the GRU Container
    self->state.gru_container.num_gru_in_run_domain = num_gru;

    // hostname information that is useful for verifying that the job actor is running on the correct node
    char host[HOST_NAME_MAX];
    gethostname(host, HOST_NAME_MAX);
    self->state.hostname = host;
    
    // Initalize global variables
    int err = 0;
    setTimesDirsAndFiles(self->state.job_actor_settings.file_manager_path.c_str(), &err);
    if (err != 0) {
        aout(self) << "ERROR: Job_Actor - setTimesDirsAndFiles\n";
        return {}; // Failure
    }
    defineGlobalData(&self->state.start_gru, &err);
    if (err != 0) {
        aout(self) << "ERROR: Job_Actor - defineGlobalData\n";
        return {}; // Failure
    }
    readDimension(&self->state.num_gru, &self->state.num_hru, &self->state.start_gru, &err);
    if (err != 0) {
        aout(self) << "ERROR: Job_Actor - readDimension\n";
        return {}; // Failure
    }
    readIcondNLayers(&self->state.num_gru, &err);
    if (err != 0) {
        aout(self) << "ERROR: Job_Actor - readIcondNLayers\n";
        return {}; // Failure
    }
    allocateTimeStructure(&err);
    if (err != 0) {
        aout(self) << "ERROR: Job_Actor - allocateTimeStructure\n";
        return {}; // Failure
    }

    // Spawn the file_access_actor. This will return the number of forcing files we are working with
    self->state.file_access_actor = self->spawn(file_access_actor, self->state.start_gru, self->state.num_gru, 
        self->state.file_access_actor_settings, self);


    aout(self) << "Job Actor Initalized \n";

    return {

        [=](init_gru) {
            initGRUs(self);
        },

        [=](done_hru, int local_gru_index, double total_duration, 
            double init_duration, double forcing_duration, 
            double run_physics_duration, double write_output_duration) {
            
          aout(self) << "\nJob_Actor: GRU Finished: \n" <<
                        "          global_gru_index = " << 
                        self->state.gru_container.gru_list[local_gru_index-1]->getGlobalGRUIndex() << "\n" <<
                        "          local_gru_index = " << local_gru_index << "\n" <<
                        "          total_duration = " << total_duration << "\n" <<
                        "          init_duration = " << init_duration << "\n" <<
                        "          forcing_duration = " << forcing_duration << "\n" <<
                        "          run_physics_duration = " << run_physics_duration << "\n" <<
                        "          write_output_duration = " << write_output_duration << "\n\n";
          // Update Timing
          self->state.gru_container.gru_list[local_gru_index-1]->setRunTime(total_duration);
          self->state.gru_container.gru_list[local_gru_index-1]->setInitDuration(init_duration);
          self->state.gru_container.gru_list[local_gru_index-1]->setForcingDuration(forcing_duration);
          self->state.gru_container.gru_list[local_gru_index-1]->setRunPhysicsDuration(run_physics_duration);
          self->state.gru_container.gru_list[local_gru_index-1]->setWriteOutputDuration(write_output_duration);

          self->state.gru_container.gru_list[local_gru_index-1]->setSuccess();

          self->state.gru_container.num_gru_done++;

          // Check if we have finished all active GRUs
          if (self->state.gru_container.num_gru_done >= self->state.gru_container.num_gru_in_run_domain) {
            
            // Check for failures
            if(self->state.gru_container.num_gru_failed == 0 || self->state.max_run_attempts == 1) {
              //TODO: RENAME DEALLOCATE_STURCTURES this is more of a finalize
              std::vector<serializable_netcdf_gru_actor_info> netcdf_gru_info = getGruNetcdfInfo(
                                                                                    self->state.max_run_attempts,
                                                                                    self->state.gru_container.gru_list);
              self->send(self->state.file_access_actor, deallocate_structures_v, netcdf_gru_info);
            
            } else {
              // TODO: Handle failures

            }
          }
        },

        [=](const error& err, caf::actor src) {
            aout(self) << "\n\n ********** ERROR HANDLER \n";
            switch(err.category()) {
                case type_id_v<hru_error>:
                    aout(self) << "HRU Error: " << to_string(err) << "\n";
                    handleGRUError(self, err, src);
                    break;
                case type_id_v<file_access_error>:
                    aout(self) << "File Access Error: " << to_string(err) << "\n";
                    break;
                default:
                    aout(self) << "Unknown Error: " << to_string(err) << "\n";
                    break;
            }
        },


        [=](file_access_actor_done, double read_duration, double write_duration) {
            int err = 0;
            // Delete GRUs
            for (auto GRU : self->state.gru_container.gru_list) {
                delete GRU;
            }
            self->state.gru_container.gru_list.clear();


            self->state.job_timing.updateEndPoint("total_duration");

            aout(self) << "\n________________PRINTING JOB_ACTOR TIMING INFO RESULTS________________\n";
            aout(self) << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) << " Seconds\n";
            aout(self) << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60 << " Minutes\n";
            aout(self) << "Total Duration = " << (self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60) / 60 << " Hours\n\n";

            deallocateJobActor(&err);
            // Tell Parent we are done
            self->send(self->state.parent, 
                    done_job_v, 
                    self->state.num_gru_failed, 
                    self->state.job_timing.getDuration("total_duration").value_or(-1.0),
                    read_duration, write_duration);
            self->quit();
        },

        // [=](file_access_actor_err, std::string function) {
        //     aout(self) << "Failure in File Access Actor in function: " << function << "\n";
        //     if (function == "def_output") {
        //         aout(self) << "Error with the output file, will try creating it agian\n";
        //         std::this_thread::sleep_for(std::chrono::seconds(5));
        //         self->state.file_access_actor = self->spawn(file_access_actor, self->state.start_gru, self->state.num_gru, 
        //            self->state.file_access_actor_settings, self);
        //     } else {
        //         aout(self) << "Letting Parent Know we are quitting\n";
        //         self->send(self->state.parent, err_v);
        //         self->quit();
        //     }
        // }
    };
}

void initGRUs(stateful_actor<job_state>* self) {
  for(int i = 0; i < self->state.gru_container.num_gru_in_run_domain; i++) {
      // Spawn the GRU Actor
      auto global_gru_index = self->state.gru_container.gru_list.size() + self->state.start_gru;
      auto local_gru_index = self->state.gru_container.gru_list.size() + 1; // Fortran reference starts at 1
      auto gru = self->spawn(hru_actor, 
                              global_gru_index, 
                              local_gru_index, 
                              self->state.hru_actor_settings,
                              self->state.file_access_actor, 
                              self);

      // Create the GRU object
      self->state.gru_container.gru_list.push_back(
              new GRU(global_gru_index, 
                      local_gru_index, 
                      gru, 
                      self->state.dt_init_start_factor, 
                      self->state.max_run_attempts));    
  }
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
        gru_info.successful = success(gru->getStatus());

        gru_netcdf_info.push_back(gru_info);

    }
    return gru_netcdf_info;
}

void handleGRUError(stateful_actor<job_state>* self, const error& err, caf::actor src) {

    // Find the GRU that failed
    for(auto GRU : self->state.gru_container.gru_list) {
        if (GRU->getGRUActor() == src) {
            GRU->setFailed();
            GRU->decrementAttemptsLeft();
            self->state.gru_container.num_gru_done++;
            self->state.gru_container.num_gru_failed++;
            self->send(self->state.file_access_actor, run_failure_v, GRU->getLocalGRUIndex());
            
            // Check if we have finished all active GRUs
            if (self->state.gru_container.num_gru_done >= self->state.gru_container.num_gru_in_run_domain) {
                // Check for failures
                if(self->state.gru_container.num_gru_failed == 0 || self->state.max_run_attempts == 1) {
                    //TODO: RENAME DEALLOCATE_STURCTURES this is more of a finalize
                    std::vector<serializable_netcdf_gru_actor_info> netcdf_gru_info = getGruNetcdfInfo(
                                                                                            self->state.max_run_attempts,
                                                                                            self->state.gru_container.gru_list);
                    self->send(self->state.file_access_actor, deallocate_structures_v, netcdf_gru_info);
                
                } else {
                // TODO: Handle failures

                }
            }
            break;
        }
    }



}

} // End Namespace caf


