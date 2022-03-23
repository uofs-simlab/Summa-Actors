#ifndef SUMMACLIENTACTOR_H_
#define SUMMACLIENTACTOR_H_

#include "Job.h"

using namespace caf;


/**
 * @brief First Actor that is spawned that is not the Coordinator Actor.
 * 
 * @param self 
 * @return behavior 
 */
behavior job_actor(stateful_actor<job_state>* self, int startGRU, int numGRU, 
    std::string fileManager, int outputStrucSize, std::string csvOut, caf::actor parent) {
    self->state.start = std::chrono::high_resolution_clock::now();
    // Set Job Variables
    self->state.startGRU = startGRU;
    self->state.numGRU = numGRU;
    self->state.fileManager = fileManager;
    self->state.parent = parent;
    self->state.outputStrucSize = outputStrucSize;
    self->state.csvOut = csvOut;

    // Initalize global variables
    initJob(self);

    // Spawn the file_access_actor. This will return the number of forcing files we are working with
    self->state.file_access_actor = self->spawn(file_access_actor, self->state.startGRU, self->state.numGRU, 
        self->state.outputStrucSize, self);
    aout(self) << "Job Actor Initalized \n";

    return {
        [=](done_file_access_actor_init) {
            // Init GRU Actors and the Output Structure
            self->send(self->state.file_access_actor, initalize_outputStrucure_v);
            self->send(self, init_hru_v);
        },

        [=](init_hru) {
            initalizeGRU(self);
        },

        [=](done_init_hru) {
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

        /**
         * Message from HRUActor, HRU is done the current forcing file but is not
         * done its simulation and needs the next file
         * indxGRU - Index into the actor array so we know which HRU this is.
         * NOTE: Naming of GRU and HRU is confusing as the plan is to further seperate
         * NOTE: For NA_Domain GRU is used as that is how we index the forcing file
         */ 

        [=](done_hru, int indxGRU, double totalDuration, double initDuration, 
            double forcingDuration, double runPhysicsDuration, double writeOutputDuration) {
            aout(self) << "GRU " << indxGRU << " Done\n";
            self->state.GRUList[indxGRU - 1]->doneRun(totalDuration, initDuration, forcingDuration,
                runPhysicsDuration, writeOutputDuration);
            self->state.GRUList[indxGRU - 1]->writeSuccess(self->state.successOutputFile);            
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

        [=](deallocate_structures) {
            // Calculate Run-Time
            self->state.end = std::chrono::high_resolution_clock::now();
            self->state.duration = std::chrono::duration_cast<std::chrono::seconds>
                (self->state.end - self->state.start).count();
            aout(self) << "Total Job Duration:";
            aout(self) << "     " << self->state.duration             << " Seconds\n";
            aout(self) << "     " << self->state.duration / 60        << " Minutes\n";
            aout(self) << "     " << (self->state.duration / 60) / 60 << " Hours\n";

            // Delete GRUs
            for (auto GRU : self->state.GRUList) {
                delete GRU;
            }
            self->state.GRUList.clear();

            // Tell Parent we are done
            self->send(self->state.parent, done_job_v, self->state.numGRUFailed);
            self->quit();
        },

        [=](run_failure, int indxGRU, int err) {
            aout(self) << "GRU:" << indxGRU << "Failed \n" <<
                "Will have to wait until all GRUs are done before it can be re-tried\n";
            self->state.numGRUFailed++;
            self->state.numGRUDone++;
            self->state.GRUList[indxGRU - 1]->updateFailed();

            // check if we are the last hru to complete
            if (self->state.numGRUDone >= self->state.numGRU) {
                restartFailures(self);
            }
        },

    };
}

void initJob(stateful_actor<job_state>* self) {
    std::ofstream file;
    self->state.successOutputFile += std::to_string(self->state.startGRU) += self->state.csvOut +=".csv";
    file.open(self->state.successOutputFile, std::ios_base::out);
    file << "GRU" << "," << "totalDuration" << "," << "initDuration" << "," << 
                "forcingDuration" << "," << "runPhysicsDuration" << "," << "writeOutputDuration" << 
                "," << "dt_init" << "," << "numAttemtps" << "\n";
    file.close();

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
    auto gru = self->spawn(hru_actor, startGRU, indexGRU, self->state.file_access_actor, 
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
    self->state.numGRU = self->state.numGRUFailed;
    self->state.numGRUFailed = 0;
    self->state.numGRUDone = 0;
    for(auto gru : self->state.GRUList) {
        if (gru->isFailed() && !gru->isMaxAttemptsReached()) {
            gru->updateFailed();
            self->send(self->state.file_access_actor, reset_outputCounter_v, gru->getIndxGRU());
            gru->updateDt_init();
            auto newGRU = self->spawn(hru_actor, gru->getRefGRU(), gru->getIndxGRU(), self->state.file_access_actor, 
                self->state.outputStrucSize, self);
            gru->updateGRU(newGRU);
            gru->updateCurrentAttempt();
            self->send(gru->getActor(), dt_init_factor_v, gru->getDt_init());
        } else {
            aout(self) << "We are done \n";
        }
    }
}



#endif
