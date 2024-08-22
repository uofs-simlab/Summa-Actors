#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include "batch_container.hpp"
#include "fileManager.hpp"
#include "openwq_actor.hpp"
#include "gru_struc.hpp"
#include <chrono>
#include <string>
#include <vector>
#include "message_atoms.hpp"
#include "summa_global_data.hpp"


struct summa_actor_state {
  TimingInfo summa_actor_timing;

  int start_gru;           // starting GRU for the simulation
  int num_gru;             // number of GRUs to compute
  int file_gru;            // number of GRUs in the file
  int num_gru_failed = 0;  // Number of GRUs that have failed
  caf::actor current_job;  // Reference to the current job actor
  caf::actor parent;

  caf::actor openwq_actor;

  std::unique_ptr<Batch_Container> batch_container;
  std::shared_ptr<const Batch> current_batch;

  std::unique_ptr<fileManager> file_manager;
  std::unique_ptr<summaGlobalData> global_fortran_state;

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
caf::behavior summa_actor(caf::stateful_actor<summa_actor_state>* self, 
                          int startGRU, int numGRU, 
                          Summa_Actor_Settings summa_actor_settings, 
                          File_Access_Actor_Settings file_access_actor_settings,
                          Job_Actor_Settings job_actor_settings, 
                          HRU_Actor_Settings hru_actor_settings, 
                          caf::actor parent);

int spawnJob(caf::stateful_actor<summa_actor_state>* self, caf::actor openwq);
void finalizeSumma(caf::stateful_actor<summa_actor_state>* self, caf::actor openwq);

// Gets the number of GRUs from the attribute file
int getNumGRUInFile(const std::string &settingsPath, 
                    const std::string &attributeFile);



