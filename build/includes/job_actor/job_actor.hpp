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
    int start_gru;                 // Starting GRU for this job
    int num_gru;                   // Number of GRUs for this job
    int num_hru;
    std::string config_path;

    std::string file_manager;      // Path of the fileManager.txt file

    // Variables for GRU monitoring
    int dt_init_start_factor = 1;   // Initial Factor for dt_init (coupled_em)
    int max_run_attempts = 3;         // Max number of attemtps to solve a GRU
    std::vector<GRUinfo*> gru_list;  // List of all GRUs under this job actor
    int num_gru_done = 0;             // The number of GRUs that have completed
    int gru_init = 0;                // Number of GRUs initalized 
    int err = 0;                    // Error Code
    int num_gru_failed = 0;           // Number of GRUs that have failed
    int output_struct_size;

    // Timing Variables
    TimingInfo job_timing;

    
    // Output File Names for Timings
    bool output_csv;
    std::string csv_out;
    std::string csv_path;
    std::string success_output_file;
    std::string failed_output_file = "failedHRU";
    std::string file_access_actor_stats = "fileAccessActor.csv";

};

behavior job_actor(stateful_actor<job_state>* self, int start_gru, int num_gru, 
    std::string config_path, int output_struct_size, actor parent);

void initCsvOutputFile(stateful_actor<job_state>* self);

void initalizeGRU(stateful_actor<job_state>* self);

void runGRUs(stateful_actor<job_state>* self);

void restartFailures(stateful_actor<job_state>* self);

} // end namespace