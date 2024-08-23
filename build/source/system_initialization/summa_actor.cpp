#include "summa_actor.hpp"
// #include "job_actor.hpp"
#include "json.hpp"
#include <iostream>
#include <fstream>
// #include <filesystem>
#include <chrono>
#include <sstream>
#include <iomanip>

// Create directories
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <cstring>
#include <iostream>

using json = nlohmann::json;
using namespace caf;

behavior SummaActor::make_behavior() {
  self_->println("Starting SUMMA Actor, start_gru {}, num_gru {}", start_gru_, 
                 num_gru_);
  timing_info_ = TimingInfo();
  timing_info_.addTimePoint("total_duration");
  timing_info_.updateStartPoint("total_duration");

  file_manager_ = std::make_unique<FileManager>(
      settings_.job_actor_settings_.file_manager_path_);
  auto err_msg = file_manager_->setTimesDirsAndFiles();
  if (!err_msg.empty()) {
    self_->println("ERROR--File Manager: {}", err_msg);
    self_->quit();
    return {};
  }
  file_gru_ = file_manager_->getFileGru();
  if (file_gru_ < 0) {
    self_->println("ERROR--File Manager: Unable To Verify Number Of GRUs");
    self_->quit();
    return {};
  } else if (file_gru_ < num_gru_ + start_gru_ - 1) {
    self_->println("ERROR--File Manager: Number Of GRUs Exceeds File GRUs");
    self_->quit();
    return {};
  }
  
  global_fortran_state_ = std::make_unique<SummaGlobalData>();
  auto err = global_fortran_state_->defineGlobalData();
  if (err != 0) {
    self_->println("ERROR--Global State: Unable To Define Global Data");
    self_->quit();
    return {};
  }

  err = createLogDirectory();
  if (err != 0) {
    self_->println("ERROR--Summa_Actor: Unable To Create Log Directory");
    self_->quit();
    return {};
  }

  batch_container_ = std::make_unique<BatchContainer>(start_gru_, num_gru_, 
      settings_.summa_actor_settings_.max_gru_per_job_, log_folder_);
  self_->println("\n\nStarting SUMMA With {} Batches\n\n", 
                 batch_container_->getBatchesRemaining());

     
  if (spawnJob() != 0) {
    self_->println("ERROR--Summa_Actor: Unable To Spawn Job\n");
    self_->quit();
    return {};
  }

  return {
    [this](done_job, int num_gru_failed, double job_duration, 
           double read_duration, double write_duration) {
      int num_success = current_batch_->getNumHRU() - num_gru_failed;

      batch_container_->updateBatchStats(current_batch_->getBatchID(), 
                                         job_duration, read_duration,
                                         write_duration, num_success, 
                                         num_gru_failed);
      num_gru_failed_ += num_gru_failed;
      if (!batch_container_->hasUnsolvedBatches()) {
        finalize();
        return;
      }

      if (spawnJob() != 0) {
        self_->println("ERROR--Summa_Actor: Unable To Spawn Job");
        self_->quit();
        return;
      }
    },

    [this](err_atom, int err_code, std::string err_msg) {
      if (err_code == -2) { // Unrecoverable Error
        self_->println("Summa-Actor: Unrecoverable Error from job_actor\n" 
                       "\t Error Message = {}", err_msg);
        self_->quit();
        return; // TODO: Need to handle this in the main actor?
      } else {
        self_->println("Summa-Actor: Recoverable Error from job_actor\n" 
                       "\t Error Message = {}\n" "\t Error Code = {}\n"
                       "IMPLEMENTATION NEEDED\n", err_msg, err_code);
        self_->quit();
        return;
      }
    },
    [this](const down_msg& dm) {
      self_->println("Lost Connection With A Connected Actor\nReason: {}",
                   to_string(dm.reason));
    }
  };
}

int SummaActor::spawnJob() {
  std::optional<Batch> batch = batch_container_->getUnsolvedBatch();
  if (!batch.has_value()) {
    self_->println("ERROR--Summa_Actor: No Batches To Solve");
    self_->quit(); 
    return -1;
  }
  current_batch_ = std::make_shared<Batch>(batch.value());
  current_job_ = self_->spawn(actor_from_state<JobActor>, batch.value(),
                              settings_.summa_actor_settings_.enable_logging_,
                              settings_.job_actor_settings_, 
                              settings_.fa_actor_settings_,
                              settings_.hru_actor_settings_, self_);
  return 0;
}


// TODO: This is meant to be a temporary solution for users that don't have
// TODO: a compiler with std::filesystem support
bool create_directories(const std::string& path) {
    struct stat info;

    if (stat(path.c_str(), &info) != 0) {
        // Directory does not exist
        if (errno == ENOENT) {
            if (mkdir(path.c_str(), 0755) != 0) {
                std::cerr << "Error creating directory: " << strerror(errno) << std::endl;
                return false;
            }
        } else {
            std::cerr << "Error checking directory: " << strerror(errno) << std::endl;
            return false;
        }
    } else if (!(info.st_mode & S_IFDIR)) {
        std::cerr << "Path exists but is not a directory" << std::endl;
        return false;
    }
    return true;
}

int SummaActor::createLogDirectory() {
  if (settings_.summa_actor_settings_.enable_logging_) {
    auto now = std::chrono::system_clock::now();
    auto now_c = std::chrono::system_clock::to_time_t(now);
    std::tm* now_tm = std::localtime(&now_c);
    std::stringstream ss;
    ss << std::put_time(now_tm, "%m_%d_%H:%M");
    log_folder_ = "startgru-" + std::to_string(start_gru_) + "_endgru-" + 
        std::to_string(start_gru_ + num_gru_ - 1) + "_" + ss.str();
    if (!settings_.summa_actor_settings_.log_dir_.empty())
        log_folder_ = settings_.summa_actor_settings_.log_dir_ + "/" + log_folder_;
      
    return (create_directories(log_folder_)) ? 0 : -1;
  } else {
    log_folder_ = ""; // Empty log to signal no logging
    return 0;
  }
}

void SummaActor::finalize() {
  self_->println("All Batches Finished\n{}", 
      batch_container_->getAllBatchInfoString());
  
  timing_info_.updateEndPoint("total_duration");

  double total_dur_sec = timing_info_.getDuration(
      "total_duration").value_or(-1.0);
  double total_dur_min = total_dur_sec / 60;
  double total_dur_hr = total_dur_min / 60;
  double read_dur_sec = batch_container_->getTotalReadTime();
  double write_dur_sec = batch_container_->getTotalWriteTime();

  self_->println("\n________________SUMMA INFO________________\n"
                 "Total Duration = {} Seconds\n"
                 "Total Duration = {} Minutes\n"
                 "Total Duration = {} Hours\n"
                 "Total Read Duration = {} Seconds\n"
                 "Total Write Duration = {} Seconds\n"
                 "Num Failed = {}\n"
                 "___________________Program Finished__________________\n",
                 total_dur_sec, total_dur_min, total_dur_hr, read_dur_sec, 
                 write_dur_sec, num_gru_failed_);

}



