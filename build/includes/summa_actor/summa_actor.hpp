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
#include "file_manager.hpp"
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
  std::string restart_;

  public:
    SummaActor(caf::event_based_actor* self, int start_gru, int num_gru, 
               Settings settings, caf::actor parent, std::string restart) 
               : self_(self), start_gru_(start_gru), num_gru_(num_gru), 
               settings_(settings), parent_(parent), restart_(restart) {};
  
    caf::behavior make_behavior();

    int spawnJob();

    int createLogDirectory();

    void finalize();
};



