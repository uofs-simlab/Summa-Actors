#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "GRUinfo.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include <unistd.h>
#include <limits.h>
#include "global.hpp"

namespace caf {
using chrono_time = std::chrono::time_point<std::chrono::system_clock>;

struct GRU_Container {
    std::vector<GRU*> gru_list;
    chrono_time gru_start_time; // Vector of start times for each GRU
    int num_gru_done = 0; 
    int num_gru_failed = 0; // number of grus that are waiting to be restarted
    int num_gru_in_run_domain = 0; // number of grus we are currently solving for
};

struct job_state {
    // Actor References
    caf::actor file_access_actor; // actor reference for the file_access_actor
    caf::actor parent;            // actor reference to the top-level SummaActor

    // Job Parameters
    int start_gru;                 // Starting GRU for this job
    int num_gru;                   // Number of GRUs for this job
    int num_hru;
    int max_run_attempts = 1;         // Max number of attemtps to solve a GRU



    GRU_Container gru_container;



    // Variables for GRU monitoring
    int dt_init_start_factor = 1;   // Initial Factor for dt_init (coupled_em)
    // std::vector<GRUinfo*> gru_list;  // List of all GRUs under this job actor
    int num_gru_done = 0;             // The number of GRUs that have completed
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

/*
 * Start all of the GRU actors and set up their container class
*/
void initGRUs(stateful_actor<job_state>* self);

/**
 * Get the information for the GRUs that will be written to the netcdf file
*/
std::vector<serializable_netcdf_gru_actor_info> getGruNetcdfInfo(int max_run_attempts, std::vector<GRU*> &gru_list);

void handleGRUError(stateful_actor<job_state>* self, const error& err, caf::actor src);

} // end namespace