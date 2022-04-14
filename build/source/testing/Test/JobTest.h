#ifndef JOBTEST_H_
#define JOBTEST_H_

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "string.h"
#include <unistd.h>
#include <vector>
#include "/home/k13nk/SummaProject/Summa-Actors/build/source/interface/job_actor/job_subroutine_wrappers.h"
#include "HRUTest.h"
#include <chrono>
#include "messageAtoms.h"
#include <iostream>
#include <fstream>
#include <sys/stat.h>
#include "/home/k13nk/SummaProject/Summa-Actors/build/source/actors/global.h"

struct job_state {
    // Actor References
    caf::actor parent;            // actor reference to the top-level SummaActor

    // Job Parameters
    int startGRU;                 // Starting GRU for this job
    int numGRU;                   // Number of GRUs for this job
    std::string configPath;

    std::string fileManager;      // Path of the fileManager.txt file

    // Variables for GRU monitoring
    int dt_init_start_factor = 1;   // Initial Factor for dt_init (coupled_em)
    int maxRunAttempts = 3;         // Max number of attemtps to solve a GRU
    int numGRUDone = 0;             // The number of GRUs that have completed
    int GRUInit = 0;                // Number of GRUs initalized 
    int err = 0;                    // Error Code
    int numGRUFailed = 0;           // Number of GRUs that have failed
    int outputStrucSize;

    // Timing Variables
    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;
    double duration;
    
    // Output File Names for Timings
    bool outputCSV;
    std::string csvOut;
    std::string csvPath;
    std::string successOutputFile;
    std::string failedOutputFile = "failedHRU";
    std::string fileAccessActorStats = "fileAccessActor.csv";

};

int parseSettings(stateful_actor<job_state>* self, std::string configPath);

void initJob(stateful_actor<job_state>* self);

void initalizeGRU(stateful_actor<job_state>* self);

void runGRUs(stateful_actor<job_state>* self);

void restartFailures(stateful_actor<job_state>* self);

behavior job_actor(stateful_actor<job_state>* self) {
    self->state.start = std::chrono::high_resolution_clock::now();
    // Set Job Variables
    self->state.startGRU = 1;
    self->state.numGRU = 500;
    self->state.configPath = "/home/k13nk/SummaProject/Summa-Actors/config";
    self->state.outputStrucSize = 100;
    self->state.fileManager = "/home/k13nk/SummaProject/SummaActorsSettings/fileManager.txt";

    // Initalize global variables
    initJob(self);

    // // spawn HRU test actor
    auto gru = self->spawn(hru_actor, 1, 1, 
        self->state.configPath, 
        self->state.outputStrucSize, self);


    return {

        [=](done_init_hru) {
            // Dealocate the structure
            aout(self) << "Received done from HRU\n";
            int err = 0;
            cleanUpJobActor(&err);
        }
    };

}

void initJob(stateful_actor<job_state>* self) {
    std::string success = "Success"; // allows us to build the string

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



#endif