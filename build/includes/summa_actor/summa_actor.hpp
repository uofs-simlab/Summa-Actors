#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include "batch_container.hpp"
#include <chrono>
#include <string>
#include <vector>

namespace caf {


struct job_timing_info {
    std::vector<double> job_duration;
    std::vector<double> job_read_duration;
    std::vector<double> job_write_duration;
};

struct summa_actor_state {
    // Timing Information For Summa-Actor
    TimingInfo summa_actor_timing;
    struct job_timing_info timing_info_for_jobs;

    // Program Parameters
    int startGRU;           // starting GRU for the simulation
    int numGRU;             // number of GRUs to compute
    int fileGRU;            // number of GRUs in the file
    std::string configPath; // path to the fileManager.txt file
    int numFailed = 0;      // Number of jobs that have failed
    caf::actor currentJob;  // Reference to the current job actor
    caf::actor parent;

    // Batches
    Batch_Container batch_container;
    int current_batch_id;


    // settings for all child actors (save in case we need to recover)
    Summa_Actor_Settings summa_actor_settings;
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings; 
    HRU_Actor_Settings hru_actor_settings;
};

behavior summa_actor(stateful_actor<summa_actor_state>* self, 
    int startGRU, int numGRU, 
    Summa_Actor_Settings summa_actor_settings, 
    File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, 
    HRU_Actor_Settings hru_actor_settings, actor parent);

void spawnJob(stateful_actor<summa_actor_state>* self);




} // namespace caf