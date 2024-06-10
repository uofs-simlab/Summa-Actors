#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include <chrono>
#include <string>
#include <vector>
#include "job_actor.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include "batch_container.hpp"
#include "fileManager.hpp"
#include "gru_struc.hpp"
#include "message_atoms.hpp"
#include "summa_global_data.hpp"


class SummaActor {
  caf::event_based_actor* self_;

  TimingInfo timing_info_;
  int start_gru_;           // starting GRU for the simulation
  int num_gru_;             // number of GRUs to compute
  int file_gru_;            // number of GRUs in the file
  int num_gru_failed_ = 0;  // Number of GRUs that have failed
  caf::actor current_job_;  // Reference to the current job actor
  caf::actor parent_;
  std::string log_folder_;

  std::unique_ptr<BatchContainer> batch_container_;
  std::shared_ptr<const Batch> current_batch_;

  std::unique_ptr<FileManager> file_manager_;
  std::unique_ptr<SummaGlobalData> global_fortran_state_;

  Settings settings_;

  public:
    SummaActor(caf::event_based_actor* self, int start_gru, int num_gru, 
               Settings settings, caf::actor parent) 
               : self_(self), start_gru_(start_gru), num_gru_(num_gru), 
               settings_(settings), parent_(parent) {};
  
    caf::behavior make_behavior();

    // Get number of GRUs from the attributes file
    int getFileGRU(const std::string &settingsPath, 
                   const std::string &attributeFile);

    int spawnJob();

    int createLogDirectory();

    void finalize();
};






// struct summa_actor_state {
//   TimingInfo summa_actor_timing;

//   int start_gru;           // starting GRU for the simulation
//   int num_gru;             // number of GRUs to compute
//   int file_gru;            // number of GRUs in the file
//   int num_gru_failed = 0;  // Number of GRUs that have failed
//   caf::actor current_job;  // Reference to the current job actor
//   caf::actor parent;

//   std::unique_ptr<Batch_Container> batch_container;
//   std::shared_ptr<const Batch> current_batch;

//   std::unique_ptr<fileManager> file_manager;
//   std::unique_ptr<summaGlobalData> global_fortran_state;

//   // settings for all child actors (save in case we need to recover)
//   Summa_Actor_Settings summa_actor_settings;
//   File_Access_Actor_Settings file_access_actor_settings;
//   Job_Actor_Settings job_actor_settings; 
//   HRU_Actor_Settings hru_actor_settings;
// };

// /**
//  * This actor is for local computation and computes a num_gru from the 
//  * start_gru index. This actor can split the computation into multiple
//  * jobs if needed.
// */
// caf::behavior summa_actor(caf::stateful_actor<summa_actor_state>* self, 
//                           int startGRU, int numGRU, 
//                           Summa_Actor_Settings summa_actor_settings, 
//                           File_Access_Actor_Settings file_access_actor_settings,
//                           Job_Actor_Settings job_actor_settings, 
//                           HRU_Actor_Settings hru_actor_settings, 
//                           caf::actor parent);

// int spawnJob(caf::stateful_actor<summa_actor_state>* self);
// void finalizeSumma(caf::stateful_actor<summa_actor_state>* self);

// // Gets the number of GRUs from the attribute file
// int getNumGRUInFile(const std::string &settingsPath, 
//                     const std::string &attributeFile);



