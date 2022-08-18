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
behavior job_actor(stateful_actor<job_state>* self, int startGRU, int numGRU, 
    std::string configPath, int outputStrucSize, caf::actor parent) {
    // Timinig Information
    self->state.job_timing = TimingInfo();
    self->state.job_timing.addTimePoint("total_duration");
    self->state.job_timing.updateStartPoint("total_duration");


    // Set Job Variables
    self->state.startGRU = startGRU;
    self->state.numGRU = numGRU;
    self->state.configPath = configPath;
    self->state.parent = parent;
    self->state.outputStrucSize = outputStrucSize;

    // Get All Settings
    self->state.fileManager = getSettings(self->state.configPath, "JobActor", "FileManagerPath", 
        self->state.fileManager).value_or("");
    self->state.outputCSV = getSettings(self->state.configPath, "JobActor", "outputCSV",
        self->state.outputCSV).value_or(false);
    if (self->state.outputCSV) {
        self->state.csvPath = getSettings(self->state.configPath, "JobActor", "csvPath",
        self->state.csvPath).value_or("");
        if (self->state.csvPath == ""){ // check if we found the value if not set outputCSV to false
            self->state.outputCSV = false;
        }
    }
    
    // Print Settings
    aout(self) << "\nSETTINGS FOR JOB_ACTOR\n" << 
        "File Manager Path = " << self->state.fileManager << "\n" <<
        "outputCSV = " << self->state.outputCSV << "\n";
    if (self->state.outputCSV) {
        aout(self) << "csvPath = " << self->state.csvPath << "\n";
    }

    // Initalize global variables
    initJob(self);

    // Spawn the file_access_actor. This will return the number of forcing files we are working with
    self->state.file_access_actor = self->spawn(file_access_actor, self->state.startGRU, self->state.numGRU, 
        self->state.outputStrucSize, self->state.configPath, self);


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
            self->state.GRUInit++;
            if (self->state.GRUInit < self->state.numGRU) {
                self->send(self, init_hru_v);
            } else {
                aout(self) << "All GRUs are initalized\n";
                self->state.GRUInit = 0; // reset counter in case we have failures
                runGRUs(self);
            }
        },

        [=](done_hru, int indx_gru, double total_duration, double init_duration, 
            double forcing_duration, double run_physics_duration, double write_output_duration) {
            aout(self) << "\nDone - GRU:" << self->state.GRUList[indx_gru - 1]->getRefGRU()
                << " - IndexInJob = " << indx_gru << "\n";

            self->state.GRUList[indx_gru - 1]->doneRun(total_duration, init_duration, forcing_duration,
                run_physics_duration, write_output_duration);
            
            if (self->state.outputCSV) {
                self->state.GRUList[indx_gru - 1]->writeSuccess(self->state.successOutputFile);            
            }
            
            self->state.numGRUDone++;

            // Check if we are done
            if (self->state.numGRUDone >= self->state.numGRU) {
                self->state.numGRUDone = 0; // just in case there were failures

                if (self->state.numGRUFailed == 0) {
                    self->send(self->state.file_access_actor, deallocate_structures_v);
                } else {
                    restartFailures(self);
                }
            }
        },

        [=](run_failure, caf::actor actorRef, int indxGRU, int err) {
            aout(self) << "GRU:" << self->state.GRUList[indxGRU - 1]->getRefGRU()
                << "indxGRU = " << indxGRU << "Failed \n"
                << "Will have to wait until all GRUs are done before it can be re-tried\n";
            
            self->state.numGRUFailed++;
            self->state.numGRUDone++;
            self->state.GRUList[indxGRU - 1]->updateFailed();

            // Let the file_access_actor know this actor failed
            self->send(self->state.file_access_actor, run_failure_v, indxGRU);

            // check if we are the last hru to complete
            if (self->state.numGRUDone >= self->state.numGRU) {
                restartFailures(self);
            }
        },

        [=](done_file_access_actor_init) {
            // Init GRU Actors and the Output Structure
            // self->send(self, init_hru_v);s
            auto gru = self->spawn(gru_actor, 1, 1, 
                self->state.configPath,
                self->state.outputStrucSize, self);
            self->send(gru, init_gru_v);
        },

        [=](file_access_actor_done, double read_duration, double write_duration) {
            int err = 0;
            if (debug) {
                aout(self) << "\n********************************\n";
                aout(self) << "Outputing Timing Info for HRUs\n";
                
                for(auto gru : self->state.GRUList) {
                    gru->printOutput();
                }
                aout(self) << "********************************\n";
            }
            // Delete GRUs
            for (auto GRU : self->state.GRUList) {
                delete GRU;
            }
            self->state.GRUList.clear();


            self->state.job_timing.updateEndPoint("total_duration");

            aout(self) << "\n________________PRINTING JOB_ACTOR TIMING INFO RESULTS________________\n";
            aout(self) << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) << " Seconds\n";
            aout(self) << "Total Duration = " << self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60 << " Minutes\n";
            aout(self) << "Total Duration = " << (self->state.job_timing.getDuration("total_duration").value_or(-1.0) / 60) / 60 << " Hours\n\n";

            cleanUpJobActor(&err);
            // Tell Parent we are done
            self->send(self->state.parent, 
                    done_job_v, 
                    self->state.numGRUFailed, 
                    self->state.job_timing.getDuration("total_duration").value_or(-1.0),
                    read_duration, write_duration);
            self->quit();
        },

        [=](file_access_actor_err, std::string function) {
            aout(self) << "Failure in File Access Actor in function: " << function << "\n";
            if (function == "def_output") {
                aout(self) << "Error with the output file, will try creating it agian\n";
                std::this_thread::sleep_for(std::chrono::seconds(5));
                self->state.file_access_actor = self->spawn(file_access_actor, self->state.startGRU, self->state.numGRU, 
                    self->state.outputStrucSize, self->state.configPath, self);
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
    if (self->state.outputCSV) {
        std::ofstream file;
        self->state.successOutputFile = self->state.csvPath += success += 
            std::to_string(self->state.startGRU) += ".csv";
        file.open(self->state.successOutputFile, std::ios_base::out);
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
    initGlobals(self->state.fileManager.c_str(), 
        &totalGRUs,    
        &totalHRUs, 
        &self->state.numGRU, 
        &numHRUs,
        &self->state.startGRU, 
        &err);
    if (err != 0) {
        aout(self) << "Error: initGlobals" << std::endl;
        self->quit();
    }
}

void initalizeGRU(stateful_actor<job_state>* self) {
    int startGRU = self->state.GRUList.size() + self->state.startGRU;
    int indexGRU = self->state.GRUList.size() + 1; // Fortran reference starts at 1
    auto gru = self->spawn(hru_actor, startGRU, indexGRU, 
        self->state.configPath, self->state.file_access_actor, 
        self->state.outputStrucSize, self);
    self->state.GRUList.push_back(new GRUinfo(startGRU, indexGRU, gru, 
        self->state.dt_init_start_factor, self->state.maxRunAttempts));
}

void runGRUs(stateful_actor<job_state>* self) {
    for(auto gru : self->state.GRUList) {
        if(!gru->isCompleted() && !gru->isFailed()) {
            self->send(gru->getActor(), start_hru_v);
        }
    }
}

void restartFailures(stateful_actor<job_state>* self) {
    // Need to let the file_access_actor know so it can set up the new output Manager
    self->send(self->state.file_access_actor, restart_failures_v);

    self->state.numGRU = self->state.numGRUFailed;
    self->state.numGRUFailed = 0;
    self->state.numGRUDone = 0;
    for(auto gru : self->state.GRUList) {
        if (gru->isFailed() && !gru->isMaxAttemptsReached()) {
            gru->updateFailed();
            self->send(self->state.file_access_actor, reset_outputCounter_v, gru->getIndxGRU());
            gru->updateDt_init();
            auto newGRU = self->spawn(hru_actor, gru->getRefGRU(), gru->getIndxGRU(), 
                self->state.configPath,self->state.file_access_actor, 
                self->state.outputStrucSize, self);
            gru->updateGRU(newGRU);
            gru->updateCurrentAttempt();
            self->send(gru->getActor(), dt_init_factor_v, gru->getDt_init());
        }
    }
}

} // End Namespace caf


