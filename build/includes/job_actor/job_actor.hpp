#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "GRUinfo.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include <unistd.h>
#include <limits.h>

namespace caf {
struct job_state {
    // Actor References
    caf::actor file_access_actor; // actor reference for the file_access_actor
    caf::actor parent;            // actor reference to the top-level SummaActor

    // Job Parameters
    int start_gru;                 // Starting GRU for this job
    int num_gru;                   // Number of GRUs for this job
    int num_hru;

    // Variables for GRU monitoring
    int dt_init_start_factor = 1;   // Initial Factor for dt_init (coupled_em)
    int max_run_attempts = 3;         // Max number of attemtps to solve a GRU
    std::vector<GRUinfo*> gru_list;  // List of all GRUs under this job actor
    int num_gru_done = 0;             // The number of GRUs that have completed
    int gru_init = 0;                // Number of GRUs initalized 
    int err = 0;                    // Error Code
    int num_gru_failed = 0;           // Number of GRUs that have failed

    // Timing Variables
    TimingInfo job_timing;

    std::string hostname;

    
    // Output File Names for Timings
    std::string success_output_file;
    std::string failed_output_file = "failedHRU";
    std::string file_access_actor_stats = "fileAccessActor.csv";

    // settings for all child actors (save in case we need to recover)
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings; 
    HRU_Actor_Settings hru_actor_settings;

};

behavior job_actor(stateful_actor<job_state>* self, int start_gru, int num_gru, 
    File_Access_Actor_Settings file_access_actor_settings, Job_Actor_Settings job_actor_settings, 
    HRU_Actor_Settings hru_actor_settings, actor parent);

void initCsvOutputFile(stateful_actor<job_state>* self);

void initalizeGRU(stateful_actor<job_state>* self);

void runGRUs(stateful_actor<job_state>* self);

void restartFailures(stateful_actor<job_state>* self);

} // end namespace