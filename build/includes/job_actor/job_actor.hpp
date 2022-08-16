#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "GRUinfo.hpp"
#include "timing_info.hpp"

namespace caf {
struct job_state {
    // Actor References
    caf::actor file_access_actor; // actor reference for the file_access_actor
    caf::actor parent;            // actor reference to the top-level SummaActor

    // Job Parameters
    int startGRU;                 // Starting GRU for this job
    int numGRU;                   // Number of GRUs for this job
    std::string configPath;

    std::string fileManager;      // Path of the fileManager.txt file

    // Variables for GRU monitoring
    int dt_init_start_factor = 1;   // Initial Factor for dt_init (coupled_em)
    int maxRunAttempts = 3;         // Max number of attemtps to solve a GRU
    std::vector<GRUinfo*> GRUList;  // List of all GRUs under this job actor
    int numGRUDone = 0;             // The number of GRUs that have completed
    int GRUInit = 0;                // Number of GRUs initalized 
    int err = 0;                    // Error Code
    int numGRUFailed = 0;           // Number of GRUs that have failed
    int outputStrucSize;

    // Timing Variables
    TimingInfo job_timing;

    
    // Output File Names for Timings
    bool outputCSV;
    std::string csvOut;
    std::string csvPath;
    std::string successOutputFile;
    std::string failedOutputFile = "failedHRU";
    std::string fileAccessActorStats = "fileAccessActor.csv";

};

behavior job_actor(stateful_actor<job_state>* self, int startGRU, int numGRU, 
    std::string configPath, int outputStrucSize, actor parent);

int parseSettings(stateful_actor<job_state>* self, std::string configPath);

void initJob(stateful_actor<job_state>* self);

void initalizeGRU(stateful_actor<job_state>* self);

void runGRUs(stateful_actor<job_state>* self);

void restartFailures(stateful_actor<job_state>* self);

} // end namespace