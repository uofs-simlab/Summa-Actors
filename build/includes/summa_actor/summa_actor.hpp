#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include "batch_container.hpp"
#include "fileManager.hpp"
#include "gru_struc.hpp"
#include <chrono>
#include <string>
#include <vector>
#include "message_atoms.hpp"
#include "summa_global_data.hpp"


namespace caf {
struct summa_actor_state {
  TimingInfo summa_actor_timing;

  int start_gru;           // starting GRU for the simulation
  int num_gru;             // number of GRUs to compute
  int file_gru;            // number of GRUs in the file
  int num_gru_failed = 0;  // Number of GRUs that have failed
  caf::actor current_job;  // Reference to the current job actor
  caf::actor parent;

  Batch_Container batch_container;
  int current_batch_id;

  std::unique_ptr<fileManager> file_manager;
  std::unique_ptr<summaGlobalData> global_fortran_state;

    // Batches
    Batch_Container batch_container;
    int current_batch_id;


  // settings for all child actors (save in case we need to recover)
  Summa_Actor_Settings summa_actor_settings;
  File_Access_Actor_Settings file_access_actor_settings;
  Job_Actor_Settings job_actor_settings; 
  HRU_Actor_Settings hru_actor_settings;
};

/**
 * This actor is for local computation and computes a num_gru from the 
 * start_gru index. This actor can split the computation into multiple
 * jobs if needed.
*/
behavior summa_actor(stateful_actor<summa_actor_state>* self, 
                     int startGRU, int numGRU, 
                     Summa_Actor_Settings summa_actor_settings, 
                     File_Access_Actor_Settings file_access_actor_settings,
                     Job_Actor_Settings job_actor_settings, 
                     HRU_Actor_Settings hru_actor_settings, actor parent);

void spawnJob(stateful_actor<summa_actor_state>* self);
} // namespace caf

// Gets the number of GRUs from the attribute file
int getNumGRUInFile(const std::string &settingsPath, 
                    const std::string &attributeFile);



