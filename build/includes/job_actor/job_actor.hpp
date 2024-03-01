#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "GRU.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include "global.hpp"
#include "json.hpp"
#include "hru_actor.hpp"
#include "hru_batch_actor.hpp"
#include "message_atoms.hpp"
#include "file_access_actor.hpp"
#include <unistd.h>
#include <limits.h>


/*********************************************
 * Job Actor Fortran Functions
 *********************************************/
extern "C" {
    void job_init_fortran(char const* file_manager, int* start_gru_index, int* num_gru, int* num_hru, int* err);

    void deallocateJobActor(int* err);
}


/*********************************************
 * Job Actor state variables
 *********************************************/
namespace caf {
using chrono_time = std::chrono::time_point<std::chrono::system_clock>;

// Holds information about the GRUs
struct GRU_Container {
    std::vector<GRU*> gru_list;
    chrono_time gru_start_time; // Vector of start times for each GRU
    int num_gru_done = 0; 
    int num_gru_failed = 0; // number of grus that are waiting to be restarted
    int num_gru_in_run_domain = 0; // number of grus we are currently solving for
    int run_attempts_left = 1; // current run attempt for all grus
};

struct job_state {
    // Actor References
    caf::actor file_access_actor; // actor reference for the file_access_actor
    caf::actor parent;            // actor reference to the top-level SummaActor

    // Job Parameters
    int start_gru;                // Starting GRU for this job
    int num_gru;                  // Number of GRUs for this job
    int num_hru;
    int max_run_attempts = 1;     // Max number of attempts to solve a GRU

    GRU_Container gru_container;

    // Variables for GRU monitoring
    int dt_init_start_factor = 1; // Initial Factor for dt_init (coupled_em)
    int num_gru_done = 0;         // The number of GRUs that have completed
    int num_gru_failed = 0;       // Number of GRUs that have failed

    // Timing Variables
    TimingInfo job_timing;
    
    std::string hostname;

    // settings for all child actors (save in case we need to recover)
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings; 
    HRU_Actor_Settings hru_actor_settings;

    // Forcing information
    int iFile = 1; // index of current forcing file from forcing file list
    int stepsInCurrentFFile;
    int forcingStep = 1;
    int timestep = 1;
    int num_gru_done_timestep = 0;
    int num_steps = 0;
};


/** The Job Actor */
behavior job_actor(stateful_actor<job_state>* self, 
                   int start_gru, int num_gru, 
                   File_Access_Actor_Settings file_access_actor_settings, 
                   Job_Actor_Settings job_actor_settings, 
                   HRU_Actor_Settings hru_actor_settings, 
                   actor parent);


/*********************************************
 * Functions for the Job Actor
 *********************************************/
// Spawn HRU Actors Individually
void spawnHRUActors(stateful_actor<job_state>* self, bool normal_mode);
// Spawn HRU Batch Actors
void spawnHRUBatches(stateful_actor<job_state>* self);


/** Get the information for the GRUs that will be written to the netcdf file */
std::vector<serializable_netcdf_gru_actor_info> getGruNetcdfInfo(int max_run_attempts, 
                                                                 std::vector<GRU*> &gru_list);

void handleGRUError(stateful_actor<job_state>* self, caf::actor src);
} // end namespace