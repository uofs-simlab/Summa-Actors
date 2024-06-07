#include "summa_actor.hpp"
// #include "job_actor.hpp"
#include "json.hpp"
#include <iostream>
#include <fstream>
#include <netcdf.h>
#include <filesystem>
#include <chrono>
#include <sstream>
#include <iomanip>

using json = nlohmann::json;
using namespace caf;

behavior SummaActor::make_behavior() {
  self_->println("Starting SUMMA Actor, start_gru {}, num_gru {}", 
                 start_gru_, num_gru_);
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
  
  global_fortran_state_ = std::make_unique<SummaGlobalData>();
  auto err = global_fortran_state_->defineGlobalData();
  if (err != 0) {
    self_->println("ERROR--Global State: Unable To Define Global Data");
    self_->quit();
    return {};
  }

  // TODO: This can all be combined now inside the method
  file_gru_ = getNumGRUInFile(file_manager_->settings_path_, 
                              file_manager_->local_attributes_);
  if (file_gru_ == -1) {
    self_->println("ERROR--File Manager: Unable To Verify Number Of GRUs");
    self_->quit();
    return {};
  }

  if (file_gru_ > 0) {
    // Adjust num_gru if it exceeds the number of GRUs in the file
    if (start_gru_ + num_gru_ > file_gru_) {
      num_gru_ = file_gru_ - start_gru_ + 1;
    }
  }

  // Create the log directory
  auto now = std::chrono::system_clock::now();
  auto now_c = std::chrono::system_clock::to_time_t(now);
  std::tm* now_tm = std::localtime(&now_c);

  std::stringstream ss;
  ss << std::put_time(now_tm, "%m_%d_%H:%M");

  std::string folder_name = "startgru-" + 
      std::to_string(start_gru_) + "_endgru-" + 
      std::to_string(start_gru_ + num_gru_ - 1)
      + "_" + ss.str();
  
  if (!settings_.summa_actor_settings_.log_dir_.empty())
    folder_name = settings_.summa_actor_settings_.log_dir_ + "/" + folder_name;
  std::filesystem::create_directories(folder_name);

  // Create the batch container
  batch_container_ = std::make_unique<BatchContainer>(start_gru_, num_gru_, 
      settings_.summa_actor_settings_.max_gru_per_job_, folder_name);

  self_->println("\n\nStarting SUMMA With {} Batches\n\n", 
                 batch_container_->getBatchesRemaining());
  return {

  };
}



// behavior summa_actor(stateful_actor<summa_actor_state>* self, int start_gru, 
//                      int num_gru, Summa_Actor_Settings summa_actor_settings, 
//                      File_Access_Actor_Settings file_access_actor_settings,
//                      Job_Actor_Settings job_actor_settings, 
//                      HRU_Actor_Settings hru_actor_settings, actor parent) {




   
//   if (spawnJob(self) != 0) {
//     aout(self) << "ERROR--Summa_Actor: Unable To Spawn Job\n";
//     self->quit();
//     exit(EXIT_FAILURE);
//   }

//   return {
//     [&batch_container, self](done_job, int num_gru_failed, double job_duration, 
//                              double read_duration, double write_duration) {
//       int num_success = self->state.current_batch->getNumHRU() - num_gru_failed;
//       batch_container->updateBatchStats(self->state.current_batch->getBatchID(), 
//                                         job_duration, read_duration,
//                                         write_duration, num_success, 
//                                         num_gru_failed);
      
//       self->state.num_gru_failed += num_gru_failed;
      
//       if (!batch_container->hasUnsolvedBatches()) {
//         finalizeSumma(self);
//         return;
//       }

//       // Find another batch to solve
//       if (spawnJob(self) != 0) {
//         aout(self) << "ERROR--Summa_Actor: Unable To Spawn Job\n";
//         self->quit();
//         exit(EXIT_FAILURE);
//       }
//     },

//     [=](err_atom, int err_code, std::string err_msg) {
//       if (err_code == -2) { // Unrecoverable Error
//         aout(self) << "Summa-Actor: Unrecoverable Error from job_actor\n" 
//                    << "\t Error Message = " << err_msg << "\n";
//         self->quit();
//         exit(EXIT_FAILURE);
//       } else {
//         aout(self) << "Summa-Actor: Recoverable Error from job_actor\n" 
//                    << "\t Error Message = " << err_msg << "\n"
//                    << "\t Error Code = " << err_code << "\n"
//                    << "IMPLEMENTATION NEEDED\n";
//         self->quit();
//         return;
//       }
//     }
//   };
// }


int SummaActor::spawnJob() {
  std::optional<Batch> batch = batch_container_->getUnsolvedBatch();
  if (!batch.has_value()) {
    self_->println("ERROR--Summa_Actor: No Batches To Solve");
    self_->quit(); 
    return -1;
  }
  // TODO: Spawn the job actor
  current_batch_ = std::make_shared<Batch>(batch.value());
  // current_job_ = self->spawn(job_actor, batch.value(),
  //     self->state.file_access_actor_settings, 
  //     self->state.job_actor_settings, self->state.hru_actor_settings, self);
  return 0;
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

// void finalizeSumma(stateful_actor<summa_actor_state>* self) {
//   auto& batch_container = self->state.batch_container;
//   aout(self) << "All Batches Finished\n"
//               << batch_container->getAllBatchInfoString();
//   self->state.summa_actor_timing.updateEndPoint("total_duration");

//   double total_dur_sec = self->state.summa_actor_timing.getDuration(
//       "total_duration").value_or(-1.0);
//   double total_dur_min = total_dur_sec / 60;
//   double total_dur_hr = total_dur_min / 60;
//   double read_dur_sec = batch_container->getTotalReadTime();
//   double write_dur_sec = batch_container->getTotalWriteTime();
    
//   aout(self) << "\n________________SUMMA INFO________________\n"
//              << "Total Duration = " << total_dur_sec << " Seconds\n"
//              << "Total Duration = " << total_dur_min << " Minutes\n"
//              << "Total Duration = " << total_dur_hr << " Hours\n"
//              << "Total Read Duration = " << read_dur_sec << "Seconds\n"
//              << "Total Write Duration = " << write_dur_sec << "Seconds\n"
//              << "Num Failed = " << self->state.num_gru_failed << "\n"
//              << "___________________Program Finished__________________\n";
  
//   self->send(self->state.parent, done_batch_v, total_dur_sec, 
//               read_dur_sec, write_dur_sec);
//   self->quit();
//   return;
// }



int SummaActor::getNumGRUInFile(const std::string &settingsPath, 
                                const std::string &attributeFile) {
  size_t fileGRU = -1;
  int ncid, gru_dim;

  if (attributeFile.empty() || settingsPath.empty())
    return fileGRU;
  
  std::string combined = settingsPath + attributeFile;

  if (NC_NOERR != nc_open(combined.c_str(), NC_NOWRITE, &ncid))
    return fileGRU;
  if (NC_NOERR != nc_inq_dimid(ncid, "gru", &gru_dim)) {
    nc_close(ncid);
    return -1;
  }
  if (NC_NOERR != nc_inq_dimlen(ncid, gru_dim, &fileGRU)) {
    nc_close(ncid);
    return -1;
  }
  nc_close(ncid);
  return fileGRU;
}



