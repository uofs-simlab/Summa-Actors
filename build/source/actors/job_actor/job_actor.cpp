#include "job_actor.hpp"
#include "file_access_actor.hpp"
#include "json.hpp"
#include "message_atoms.hpp"
#include "global.hpp"
#include "job_actor_subroutine_wrappers.hpp"
#include "hru_actor.hpp"

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
    self->state.start = std::chrono::high_resolution_clock::now();
    // Set Job Variables
    self->state.startGRU = startGRU;
    self->state.numGRU = numGRU;
    self->state.configPath = configPath;
    self->state.parent = parent;
    self->state.outputStrucSize = outputStrucSize;

    if (parseSettings(self, configPath) == -1) {
        aout(self) << "ERROR WITH JSON SETTINGS FILE!!!\n";
        self->quit();
    } else {
        aout(self) << "\nSETTINGS FOR JOB_ACTOR\n" << 
        "File Manager Path = " << self->state.fileManager << "\n" <<
        "outputCSV = " << self->state.outputCSV << "\n";
        if (self->state.outputCSV) {
            aout(self) << "csvPath = " << self->state.csvPath << "\n";
        }
    }

    // Initalize global variables
    initJob(self);

    // Spawn the file_access_actor. This will return the number of forcing files we are working with
    self->state.file_access_actor = self->spawn(file_access_actor, self->state.startGRU, self->state.numGRU, 
        self->state.outputStrucSize, self->state.configPath, self);


    aout(self) << "Job Actor Initalized \n";

    return {
    // *******************************************************************************************
    // *********************************** INTERFACE WITH HRU ************************************
    // *******************************************************************************************
        
        /**
         * 
         */
        [=](init_hru) {
            initalizeGRU(self);
        },

        /**
         * 
         */
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


        [=](done_hru, int indxGRU, double totalDuration, double initDuration, 
            double forcingDuration, double runPhysicsDuration, double writeOutputDuration) {
            aout(self) << "GRU:" << self->state.GRUList[indxGRU - 1]->getRefGRU()
                << "indxGRU = " << indxGRU << "Done \n";

            self->state.GRUList[indxGRU - 1]->doneRun(totalDuration, initDuration, forcingDuration,
                runPhysicsDuration, writeOutputDuration);
            
            if (self->state.outputCSV) {
                self->state.GRUList[indxGRU - 1]->writeSuccess(self->state.successOutputFile);            
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

    // *******************************************************************************************
    // ******************************* END INTERFACE WITH HRU ************************************
    // *******************************************************************************************

    // *******************************************************************************************
    // ****************************** INTERFACE WITH FileAccessActor *****************************
    // *******************************************************************************************
        /**
         * 
         */
        [=](done_file_access_actor_init) {
            // Init GRU Actors and the Output Structure
            // self->send(self->state.file_access_actor, initalize_outputStructure_v);
            self->send(self, init_hru_v);
        },



        [=](file_access_actor_done, double readDuration, double writeDuration) {
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


            self->state.end = std::chrono::high_resolution_clock::now();
            self->state.duration = calculateTime(self->state.start, self->state.end);

            self->state.duration = self->state.duration / 1000; // Convert to milliseconds

            aout(self) << "\nTotal Job Duration:\n";
            aout(self) << "     " << self->state.duration / 1000  << " Seconds\n";
            aout(self) << "     " << (self->state.duration / 1000) / 60  << " Minutes\n";
            aout(self) << "     " << ((self->state.duration / 1000) / 60) / 60 << " Hours\n";
            aout(self) << "\nReading Duration:\n";
            aout(self) << "     " << readDuration  << " Seconds\n";
            aout(self) << "\nWriting Duration:\n";
            aout(self) << "     " << writeDuration << " Seconds\n\n";

            cleanUpJobActor(&err);
            // Tell Parent we are done
            self->send(self->state.parent, done_job_v, self->state.numGRUFailed);
            self->quit();
        },

        [=](file_access_actor_err, std::string function) {
            aout(self) << "Failure in File Access Actor in function" << function << "\n";
            aout(self) << "Letting Parent Know we are quitting\n";
            self->send(self->state.parent, err_v);
            self->quit();
        }
    // *******************************************************************************************
    // ************************** END INTERFACE WITH FileAccessActor *****************************
    // *******************************************************************************************

    };
}


int parseSettings(stateful_actor<job_state>* self, std::string configPath) {
    json settings;
    std::string SummaActorsSettings = "/Summa_Actors_Settings.json";
	std::ifstream settings_file(configPath + SummaActorsSettings);
	settings_file >> settings;
	settings_file.close();
    
    if (settings.find("JobActor") != settings.end()) {
        json JobActorConfig = settings["JobActor"];
        // Find the File Manager Path
        if (JobActorConfig.find("FileManagerPath") !=  JobActorConfig.end()) {
            self->state.fileManager = JobActorConfig["FileManagerPath"];
        } else {
            aout(self) << "Error Finding FileManagerPath - Exiting as this is needed\n";
            return -1;
        }

        // Find if we want to outputCSV
        if (JobActorConfig.find("outputCSV") !=  JobActorConfig.end()) {
            self->state.outputCSV = JobActorConfig["outputCSV"];
        } else {
            aout(self) << "Error Finding outputCSV in JSON file - Reverting to Default Value\n";
            self->state.outputCSV = false;
        }

        // Output Path of CSV
        if (self->state.outputCSV) {
            if (JobActorConfig.find("csvPath") !=  JobActorConfig.end()) {
                self->state.csvPath = JobActorConfig["csvPath"];
            } else {
                aout(self) << "Error Finding csvPath in JSON file = Reverting to Default Value \n";
                self->state.outputCSV = false; // we just choose not to output a csv
            }
        }

        return 0;
    } else {
        aout(self) << "Error Finding JobActor in JSON file - Exiting as there is no path for the fileManger\n";
        return -1;
    }
}

void initJob(stateful_actor<job_state>* self) {
    std::string success = "Success"; // allows us to build the string
    if (self->state.outputCSV) {
        std::ofstream file;
        self->state.successOutputFile = self->state.csvPath += success += 
            std::to_string(self->state.startGRU) += ".csv";
        file.open(self->state.successOutputFile, std::ios_base::out);
        file << "GRU" << "," << "totalDuration" << "," << "initDuration" << "," << 
                    "forcingDuration" << "," << "runPhysicsDuration" << "," << "writeOutputDuration" << 
                    "," << "dt_init" << "," << "numAttemtps" << "\n";
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


