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
    
    // Set the error handlers
    self->set_down_handler([=](const down_msg& dm) {
        aout(self) << "\n\n ********** DOWN HANDLER ********** \n";
        aout(self) << "Lost Connection With A Connected Actor\n";
        aout(self) << "Reason: " << to_string(dm.reason) << "\n";
    });

    self->set_error_handler([=](const error& err) {
        aout(self) << "\n\n ********** ERROR HANDLER ********** \n";
        
        switch(err.category()) {
            case type_id_v<hru_error>:
                aout(self) << "HRU Error: " << to_string(err) << "\n";
                break;
            case type_id_v<file_access_error>:
                aout(self) << "File Access Error: " << to_string(err) << "\n";
                break;
            default:
                aout(self) << "Unknown Error: " << to_string(err) << "\n";
                break;
        }
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

    // initCsvOutputFile(self);

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
            if(self->state.gru_container.num_gru_failed == 0) {
              //TODO: RENAME DEALLOCATE_STURCTURES this is more of a finalize
              std::vector<serializable_netcdf_gru_actor_info> netcdf_gru_info = getGruNetcdfInfo(
                                                                                    self->state.max_run_attempts,
                                                                                    self->state.gru_container.gru_list);
              self->send(self->state.file_access_actor, deallocate_structures_v, netcdf_gru_info);
            
            } else {
              // TODO: Handle failures
            }
          }

        }
        //     aout(self) << "\nDone - GRU:" << self->state.gru_list[indx_gru - 1]->getRefGRU()
        //         << " - IndexInJob = " << indx_gru << "\n";

        //     self->state.gru_list[indx_gru - 1]->doneRun(total_duration, init_duration, forcing_duration,
        //         run_physics_duration, write_output_duration);
            
        //     if (self->state.job_actor_settings.output_csv) {
        //         self->state.gru_list[indx_gru - 1]->writeSuccess(self->state.success_output_file, self->state.hostname);            
        //     }
            
        //     self->state.num_gru_done++;

        //     // Check if we are done
        //     if (self->state.num_gru_done >= self->state.num_gru) {
        //         self->state.num_gru_done = 0; // just in case there were failures

        //         if (self->state.num_gru_failed == 0) {
        //             self->send(self->state.file_access_actor, deallocate_structures_v);
        //         } else {
        //             restartFailures(self);
        //         }
        //     }
        // },

        // [=](run_failure, caf::actor actorRef, int indx_gru, int err) {
            
        //     aout(self) << "GRU:" << self->state.gru_list[indx_gru - 1]->getRefGRU()
        //         << "indx_gru = " << indx_gru << "Failed \n"
        //         << "Will have to wait until all GRUs are done before it can be re-tried\n";
            
        //     self->state.num_gru_failed++;
        //     self->state.gru_list[indx_gru - 1]->updateFailed();

        //     // Let the file_access_actor know this actor failed
        //     // self->send(self->state.file_access_actor, run_failure_v, indx_gru);

        //     // check if we are the last hru to complete
        //     if (self->state.num_gru_done + self->state.num_gru_failed >= self->state.num_gru) {
        //         // restartFailures(self);
        //         self->quit();
        //     }
        // },

        // [=](done_init_gru) {
        //     aout(self) << "GRU is Initialized\n";
        //     self->quit();
        //     return;
        // },

        // [=](file_access_actor_done, double read_duration, double write_duration) {
        //     int err = 0;
        //     // Delete GRUs
        //     for (auto GRU : self->state.gru_list) {
        //         delete GRU;
        //     }
        //     self->state.gru_list.clear();


        //     self->state.job_timing.updateEndPoint("total_duration");

        //     aout(self) << "\n________________PRINTING JOB_ACTOR TIMING INFO RESULTS________________\n";
        //     aout(self) << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) << " Seconds\n";
        //     aout(self) << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60 << " Minutes\n";
        //     aout(self) << "Total Duration = " << (self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60) / 60 << " Hours\n\n";

        //     deallocateJobActor(&err);
        //     // Tell Parent we are done
        //     self->send(self->state.parent, 
        //             done_job_v, 
        //             self->state.num_gru_failed, 
        //             self->state.job_timing.getDuration("total_duration").value_or(-1.0),
        //             read_duration, write_duration);
        //     self->quit();
        // },

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

// void initCsvOutputFile(stateful_actor<job_state>* self) {
//     std::string success = "/Success"; // allows us to build the string
//     if (self->state.job_actor_settings.output_csv) {
//         std::ofstream file;
//         self->state.success_output_file = self->state.job_actor_settings.csv_path += success += 
//             std::to_string(self->state.start_gru) += ".csv";
//         aout(self) << "Success Output File: " << self->state.success_output_file << "\n";
//         file.open(self->state.success_output_file, std::ios_base::out);
//         file << 
//             "hostname,"            <<
//             "GRU,"                 << 
//             "totalDuration,"       <<
//             "initDuration,"        << 
//             "forcingDuration,"     << 
//             "runPhysicsDuration,"  << 
//             "writeOutputDuration," << 
//             "dt_init,"             << 
//             "numAttemtps\n";
//         file.close();
//     }
// }

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





    // for(int i = 0; i < self->state.num_gru; i++) {
    //     int start_gru = self->state.gru_list.size() + self->state.start_gru;
    //     int index_gru = self->state.gru_list.size() + 1; // Fortran reference starts at 1
    //     auto gru = self->spawn(hru_actor, 
    //                            start_gru, 
    //                            index_gru, 
    //                            self->state.hru_actor_settings,
    //                            self->state.file_access_actor, 
    //                            self);
    //     self->state.gru_list.push_back(new GRUinfo(start_gru, index_gru, gru, 
    //         self->state.dt_init_start_factor, self->state.max_run_attempts));
    // }
    
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



// void runGRUs(stateful_actor<job_state>* self) {
//     for(auto gru : self->state.gru_list) {
//         if(!gru->isCompleted() && !gru->isFailed()) {
//             self->send(gru->getActor(), start_hru_v);
//         }
//     }
// }

// void restartFailures(stateful_actor<job_state>* self) {
//     // Need to let the file_access_actor know so it can set up the new output Manager
//     self->send(self->state.file_access_actor, restart_failures_v);

//     self->state.num_gru = self->state.num_gru_failed;
//     self->state.num_gru_failed = 0;
//     self->state.num_gru_done = 0;


//     for(auto gru : self->state.gru_list) {
//         if (gru->isFailed() && !gru->isMaxAttemptsReached()) {
//             gru->updateFailed();
//             gru->updateDt_init();
//             auto newGRU = self->spawn(hru_actor, gru->getRefGRU(), gru->getIndxGRU(), 
//                 self->state.hru_actor_settings, self->state.file_access_actor, 
//                 self);
//             gru->updateGRU(newGRU);
//             gru->updateCurrentAttempt();
//             self->send(gru->getActor(), dt_init_factor_v, gru->getDt_init());
//         } else {
//             // Max attempts reached, so we are done with this GRU
//             self->state.gru_list[gru->getIndxGRU() - 1]->doneRun(-1, -1, -1, -1, -1);
//             if (self->state.job_actor_settings.output_csv) {
//                 self->state.gru_list[gru->getIndxGRU() - 1]->writeSuccess(self->state.success_output_file, self->state.hostname);            
//             }
//             self->state.num_gru_done++;
//         }
//     }
// }

} // End Namespace caf


