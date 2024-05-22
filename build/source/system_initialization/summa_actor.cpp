#include "summa_actor.hpp"
#include "job_actor.hpp"
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

behavior summa_actor(stateful_actor<summa_actor_state>* self, int start_gru, 
                     int num_gru, Summa_Actor_Settings summa_actor_settings, 
                     File_Access_Actor_Settings file_access_actor_settings,
                     Job_Actor_Settings job_actor_settings, 
                     HRU_Actor_Settings hru_actor_settings, actor parent) {
  self->state.summa_actor_timing = TimingInfo();
  self->state.summa_actor_timing.addTimePoint("total_duration");
  self->state.summa_actor_timing.updateStartPoint("total_duration");
  self->state.start_gru = start_gru;
  self->state.num_gru = num_gru;
  self->state.parent = parent;
  self->state.summa_actor_settings = summa_actor_settings;
  self->state.file_access_actor_settings = file_access_actor_settings;
  self->state.job_actor_settings = job_actor_settings;
  self->state.hru_actor_settings = hru_actor_settings;

  // Read in the file Manager
  auto& file_manager = self->state.file_manager;
  file_manager = std::make_unique<fileManager>(
      job_actor_settings.file_manager_path);
      
  auto err_msg = file_manager->setTimesDirsAndFiles();
  if (!err_msg.empty()) {
    aout(self) << "\n\nERROR--File Manager: " << err_msg << "\n\n";
    self->quit(); return {};
  }

  // Create the global state
  self->state.global_fortran_state = std::make_unique<summaGlobalData>();
  auto err = self->state.global_fortran_state->defineGlobalData();
  if (err != 0) {
    aout(self) << "ERROR--Global State: Unable To Define Global Data\n";
    self->quit(); return {};
  }

  self->state.file_gru = getNumGRUInFile(file_manager->settings_path_, 
                                         file_manager->local_attributes_);
  if (self->state.file_gru  == -1) 
    aout(self) << "***WARNING***: UNABLE TO VERIFY NUMBER OF GRUS" 
               << " - Job Actor MAY CRASH\n"
               << "Number of GRUs in File: " << self->state.file_gru << "\n";
  if (self->state.file_gru > 0) { 
    // Adjust num_gru if it exceeds the number of GRUs in the file
    if (self->state.start_gru + self->state.num_gru > self->state.file_gru) {
      self->state.num_gru = self->state.file_gru - self->state.start_gru + 1;
    }
  }

  // Create the log directory
  auto now = std::chrono::system_clock::now();
  auto now_c = std::chrono::system_clock::to_time_t(now);
  std::tm* now_tm = std::localtime(&now_c);

  std::stringstream ss;
  ss << std::put_time(now_tm, "%m_%d_%H:%M");

  std::string folder_name = "startgru-" + 
      std::to_string(self->state.start_gru) + "_endgru-" + 
      std::to_string(self->state.start_gru + self->state.num_gru - 1)
      + "_" + ss.str();
  if (!summa_actor_settings.log_dir.empty())
    folder_name = summa_actor_settings.log_dir + "/" + folder_name;
  std::filesystem::create_directories(folder_name);

  // Create the batch container
  auto& batch_container = self->state.batch_container;
  batch_container = std::make_unique<Batch_Container>(
      self->state.start_gru, self->state.num_gru, 
      summa_actor_settings.max_gru_per_job,
      folder_name);

  aout(self) << "\n\nStarting SUMMA With " 
             << batch_container->getBatchesRemaining() << " Batches\n\n";
   
  if (spawnJob(self) != 0) {
    aout(self) << "ERROR--Summa_Actor: Unable To Spawn Job\n";
    self->quit();
    exit(EXIT_FAILURE);
  }

  return {
    [&batch_container, self](done_job, int num_gru_failed, double job_duration, 
                             double read_duration, double write_duration) {
      int num_success = self->state.current_batch->getNumHRU() - num_gru_failed;
      batch_container->updateBatchStats(self->state.current_batch->getBatchID(), 
                                        job_duration, read_duration,
                                        write_duration, num_success, 
                                        num_gru_failed);
      
      self->state.num_gru_failed += num_gru_failed;
      
      if (!batch_container->hasUnsolvedBatches()) {
        finalizeSumma(self);
        return;
      }

      // Find another batch to solve
      if (spawnJob(self) != 0) {
        aout(self) << "ERROR--Summa_Actor: Unable To Spawn Job\n";
        self->quit();
        exit(EXIT_FAILURE);
      }
    },

    [=](err_atom, int err_code, std::string err_msg) {
      if (err_code == -2) { // Unrecoverable Error
        aout(self) << "Summa-Actor: Unrecoverable Error from job_actor\n" 
                   << "\t Error Message = " << err_msg << "\n";
        self->quit();
        exit(EXIT_FAILURE);
      } else {
        aout(self) << "Summa-Actor: Recoverable Error from job_actor\n" 
                   << "\t Error Message = " << err_msg << "\n"
                   << "\t Error Code = " << err_code << "\n"
                   << "IMPLEMENTATION NEEDED\n";
        self->quit();
        return;
      }
    }
  };
}


int spawnJob(stateful_actor<summa_actor_state>* self) {
  std::optional<Batch> batch = self->state.batch_container->getUnsolvedBatch();
  if (!batch.has_value()) {
    aout(self) << "ERROR--Summa_Actor: No Batches To Solve\n";
    self->quit(); 
    return -1;
  }
  self->state.current_batch = std::make_shared<Batch>(batch.value());
  self->state.current_job = self->spawn(
      job_actor, batch.value(),
      self->state.file_access_actor_settings, 
      self->state.job_actor_settings, self->state.hru_actor_settings, self);
  return 0;
}

void finalizeSumma(stateful_actor<summa_actor_state>* self) {
  auto& batch_container = self->state.batch_container;
  aout(self) << "All Batches Finished\n"
              << batch_container->getAllBatchInfoString();
  self->state.summa_actor_timing.updateEndPoint("total_duration");

  double total_dur_sec = self->state.summa_actor_timing.getDuration(
      "total_duration").value_or(-1.0);
  double total_dur_min = total_dur_sec / 60;
  double total_dur_hr = total_dur_min / 60;
  double read_dur_sec = batch_container->getTotalReadTime();
  double write_dur_sec = batch_container->getTotalWriteTime();
    
  aout(self) << "\n________________SUMMA INFO________________\n"
             << "Total Duration = " << total_dur_sec << " Seconds\n"
             << "Total Duration = " << total_dur_min << " Minutes\n"
             << "Total Duration = " << total_dur_hr << " Hours\n"
             << "Total Read Duration = " << read_dur_sec << "Seconds\n"
             << "Total Write Duration = " << write_dur_sec << "Seconds\n"
             << "Num Failed = " << self->state.num_gru_failed << "\n"
             << "___________________Program Finished__________________\n";
  
  self->send(self->state.parent, done_batch_v, total_dur_sec, 
              read_dur_sec, write_dur_sec);
  self->quit();
  return;
}



int getNumGRUInFile(const std::string &settingsPath, 
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



