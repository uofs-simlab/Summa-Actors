#include "job_actor.hpp"
#include "file_access_actor.hpp"
#include "json.hpp"
#include <chrono>
#include <thread>
#include "message_atoms.hpp"
#include "global.hpp"
#include "job_actor_subroutine_wrappers.hpp"
#include "hru_actor.hpp"
#include "gru_actor.hpp"

using json = nlohmann::json;

namespace caf {

/**
 * @brief First Actor that is spawned that is not the Coordinator Actor.
 * 
 * @param self 
 * @return behavior 
 */
behavior job_actor(stateful_actor<job_state>* self, int start_gru, int num_gru, 
    std::string config_path, int output_struct_size, caf::actor parent) {
    // Timinig Information
    self->state.job_timing = TimingInfo();
    self->state.job_timing.addTimePoint("total_duration");
    self->state.job_timing.updateStartPoint("total_duration");


    // Set Job Variables
    self->state.start_gru = start_gru;
    self->state.num_gru = num_gru;
    self->state.config_path = config_path;
    self->state.parent = parent;
    self->state.output_struct_size = output_struct_size;

    // Get All Settings
    self->state.file_manager = getSettings(self->state.config_path, "JobActor", "FileManagerPath", 
        self->state.file_manager).value_or("");
    if(self->state.file_manager == "") {
        aout(self) << "ERROR: Job_Actor - getSettings() - file_manager_path\n";
        self->quit();
        return {}; // Failure
    }

    self->state.output_csv = getSettings(self->state.config_path, "JobActor", "outputCSV",
        self->state.output_csv).value_or(false);
    if (self->state.output_csv) {
        self->state.csv_path = getSettings(self->state.config_path, "JobActor", "csvPath",
        self->state.csv_path).value_or("");
        if (self->state.csv_path == ""){ // check if we found the value if not set output_csv to false
            self->state.output_csv = false;
        }
    }
    
    // Print Settings
    aout(self) << "\nSETTINGS FOR JOB_ACTOR\n" << 
        "File Manager Path = " << self->state.file_manager << "\n" <<
        "output_csv = " << self->state.output_csv << "\n";
    if (self->state.output_csv) {
        aout(self) << "csv_path = " << self->state.csv_path << "\n";
    }

    // Initalize global variables
    int err = 0;
    setTimesDirsAndFiles(self->state.file_manager.c_str(), &err);
    if (err != 0) {
        aout(self) << "ERROR: Job_Actor - setTimesDirsAndFiles\n";
    }
    defineGlobalData(&self->state.start_gru, &err);
    if (err != 0) {
        aout(self) << "ERROR: Job_Actor - defineGlobalData\n";
    }
    readDimension(&self->state.num_gru, &self->state.num_hru, &self->state.start_gru, &err);
    if (err != 0) {
        aout(self) << "ERROR: Job_Actor - readDimension\n";
    }
    initJob(self);

    // Spawn the file_access_actor. This will return the number of forcing files we are working with
    self->state.file_access_actor = self->spawn(file_access_actor, self->state.start_gru, self->state.num_gru, 
        self->state.output_struct_size, self->state.config_path, self);


    aout(self) << "Job Actor Initalized \n";

    return {

        [=](init_hru) {
            initalizeGRU(self);
        },

        [=](done_init_hru) {
            if (debug) {
                aout(self) << "Done Init\n";
            }
            // aout(self) << "Done init\n";
            self->state.gru_init++;
            if (self->state.gru_init < self->state.num_gru) {
                self->send(self, init_hru_v);
            } else {
                aout(self) << "All GRUs are initalized\n";
                self->state.gru_init = 0; // reset counter in case we have failures
                runGRUs(self);
            }
        },

        [=](done_hru, int indx_gru, double total_duration, double init_duration, 
            double forcing_duration, double run_physics_duration, double write_output_duration) {
            aout(self) << "\nDone - GRU:" << self->state.gru_list[indx_gru - 1]->getRefGRU()
                << " - IndexInJob = " << indx_gru << "\n";

            self->state.gru_list[indx_gru - 1]->doneRun(total_duration, init_duration, forcing_duration,
                run_physics_duration, write_output_duration);
            
            if (self->state.output_csv) {
                self->state.gru_list[indx_gru - 1]->writeSuccess(self->state.success_output_file);            
            }
            
            self->state.num_gru_done++;

            // Check if we are done
            if (self->state.num_gru_done >= self->state.num_gru) {
                self->state.num_gru_done = 0; // just in case there were failures

                if (self->state.num_gru_failed == 0) {
                    self->send(self->state.file_access_actor, deallocate_structures_v);
                } else {
                    restartFailures(self);
                }
            }
        },

        [=](run_failure, caf::actor actorRef, int indx_gru, int err) {
            aout(self) << "GRU:" << self->state.gru_list[indx_gru - 1]->getRefGRU()
                << "indx_gru = " << indx_gru << "Failed \n"
                << "Will have to wait until all GRUs are done before it can be re-tried\n";
            
            self->state.num_gru_failed++;
            self->state.num_gru_done++;
            self->state.gru_list[indx_gru - 1]->updateFailed();

            // Let the file_access_actor know this actor failed
            self->send(self->state.file_access_actor, run_failure_v, indx_gru);

            // check if we are the last hru to complete
            if (self->state.num_gru_done >= self->state.num_gru) {
                restartFailures(self);
            }
        },

        [=](done_file_access_actor_init) {
            // Init GRU Actors and the Output Structure
            self->send(self, init_hru_v);
            // auto gru = self->spawn(gru_actor, 1, 1, 
            //     self->state.config_path,
            //     self->state.output_struct_size, self);
            // self->send(gru, init_gru_v);
        },

        [=](file_access_actor_done, double read_duration, double write_duration) {
            int err = 0;
            if (debug) {
                aout(self) << "\n********************************\n";
                aout(self) << "Outputing Timing Info for HRUs\n";
                
                for(auto gru : self->state.gru_list) {
                    gru->printOutput();
                }
                aout(self) << "********************************\n";
            }
            // Delete GRUs
            for (auto GRU : self->state.gru_list) {
                delete GRU;
            }
            self->state.gru_list.clear();


            self->state.job_timing.updateEndPoint("total_duration");

            aout(self) << "\n________________PRINTING JOB_ACTOR TIMING INFO RESULTS________________\n";
            aout(self) << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) << " Seconds\n";
            aout(self) << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60 << " Minutes\n";
            aout(self) << "Total Duration = " << (self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60) / 60 << " Hours\n\n";

            cleanUpJobActor(&err);
            // Tell Parent we are done
            self->send(self->state.parent, 
                    done_job_v, 
                    self->state.num_gru_failed, 
                    self->state.job_timing.getDuration("total_duration").value_or(-1.0),
                    read_duration, write_duration);
            self->quit();
        },

        [=](file_access_actor_err, std::string function) {
            aout(self) << "Failure in File Access Actor in function: " << function << "\n";
            if (function == "def_output") {
                aout(self) << "Error with the output file, will try creating it agian\n";
                std::this_thread::sleep_for(std::chrono::seconds(5));
                self->state.file_access_actor = self->spawn(file_access_actor, self->state.start_gru, self->state.num_gru, 
                    self->state.output_struct_size, self->state.config_path, self);
            } else {
                aout(self) << "Letting Parent Know we are quitting\n";
                self->send(self->state.parent, err_v);
                self->quit();
            }


        }
    // *******************************************************************************************
    // ************************** END INTERFACE WITH FileAccessActor *****************************
    // *******************************************************************************************

    };
}

void initJob(stateful_actor<job_state>* self) {
    std::string success = "Success"; // allows us to build the string
    if (self->state.output_csv) {
        std::ofstream file;
        self->state.success_output_file = self->state.csv_path += success += 
            std::to_string(self->state.start_gru) += ".csv";
        file.open(self->state.success_output_file, std::ios_base::out);
        file << 
            "GRU,"                 << 
            "totalDuration,"       <<
            "initDuration,"        << 
            "forcingDuration,"     << 
            "runPhysicsDuration,"  << 
            "writeOutputDuration," << 
            "dt_init,"             << 
            "numAttemtps\n";
        file.close();
    }

    int totalGRUs           = 0;
    int totalHRUs           = 0;
    int numHRUs             = 0;
    int err                 = 0;

    // aout(self) << "Initalizing Globals \n";
    initGlobals(self->state.file_manager.c_str(), 
        &totalGRUs,    
        &totalHRUs, 
        &self->state.num_gru, 
        &numHRUs,
        &self->state.start_gru, 
        &err);
    if (err != 0) {
        aout(self) << "Error: initGlobals" << std::endl;
        self->quit();
    }
}

void initalizeGRU(stateful_actor<job_state>* self) {
    int start_gru = self->state.gru_list.size() + self->state.start_gru;
    int index_gru = self->state.gru_list.size() + 1; // Fortran reference starts at 1
    auto gru = self->spawn(hru_actor, start_gru, index_gru, 
        self->state.config_path, self->state.file_access_actor, 
        self->state.output_struct_size, self);
    self->state.gru_list.push_back(new GRUinfo(start_gru, index_gru, gru, 
        self->state.dt_init_start_factor, self->state.max_run_attempts));
}

void runGRUs(stateful_actor<job_state>* self) {
    for(auto gru : self->state.gru_list) {
        if(!gru->isCompleted() && !gru->isFailed()) {
            self->send(gru->getActor(), start_hru_v);
        }
    }
}

void restartFailures(stateful_actor<job_state>* self) {
    // Need to let the file_access_actor know so it can set up the new output Manager
    self->send(self->state.file_access_actor, restart_failures_v);

    self->state.num_gru = self->state.num_gru_failed;
    self->state.num_gru_failed = 0;
    self->state.num_gru_done = 0;
    for(auto gru : self->state.gru_list) {
        if (gru->isFailed() && !gru->isMaxAttemptsReached()) {
            gru->updateFailed();
            self->send(self->state.file_access_actor, reset_outputCounter_v, gru->getIndxGRU());
            gru->updateDt_init();
            auto newGRU = self->spawn(hru_actor, gru->getRefGRU(), gru->getIndxGRU(), 
                self->state.config_path,self->state.file_access_actor, 
                self->state.output_struct_size, self);
            gru->updateGRU(newGRU);
            gru->updateCurrentAttempt();
            self->send(gru->getActor(), dt_init_factor_v, gru->getDt_init());
        }
    }
}

} // End Namespace caf


