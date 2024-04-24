#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "summa_actor.hpp"
#include "global.hpp"
#include "job_actor.hpp"
#include "json.hpp"
#include <iostream>
#include <chrono>
#include <string>
#include <fstream>
#include <netcdf.h>

using json = nlohmann::json;

namespace caf {
behavior summa_actor(stateful_actor<summa_actor_state>* self, 
    int startGRU, int numGRU, 
    Summa_Actor_Settings summa_actor_settings, 
    File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, 
    HRU_Actor_Settings hru_actor_settings, actor parent) {

  // Set Timing Variables
  self->state.summa_actor_timing = TimingInfo();
  self->state.summa_actor_timing.addTimePoint("total_duration");
  self->state.summa_actor_timing.updateStartPoint("total_duration");
  // Set Variables
  self->state.startGRU = startGRU;
  self->state.numGRU = numGRU;
  self->state.parent = parent;
  // Set Settings
  self->state.summa_actor_settings = summa_actor_settings;
  self->state.file_access_actor_settings = file_access_actor_settings;
  self->state.job_actor_settings = job_actor_settings;
  self->state.hru_actor_settings = hru_actor_settings;

  // Read in the file Manager
  auto& file_manager = self->state.file_manager;
  file_manager = std::make_unique<fileManager>(
      job_actor_settings.file_manager_path);
  // Set the directoires for the fortran side
  auto err_msg = file_manager->initFileManagerModule();
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

  self->state.fileGRU = getNumGRUInFile(file_manager->settings_path_, 
      file_manager->local_attributes_);
  if (self->state.fileGRU  == -1) 
    aout(self) << "***WARNING***: UNABLE TO VERIFY NUMBER OF GRUS" 
               << " - Job Actor MAY CRASH\n"
               << "Number of GRUs in File: " << self->state.fileGRU << "\n";

  if (self->state.fileGRU > 0) { 
    // Fix the number of GRUs if it exceeds the number of GRUs in the file
    if (self->state.startGRU + self->state.numGRU > self->state.fileGRU) {
      self->state.numGRU = self->state.fileGRU - self->state.startGRU + 1;
    }
  }
  // No else: if we cannot verify we try to run anyway
  self->state.batch_container = Batch_Container(self->state.startGRU, 
      self->state.numGRU, 
      self->state.summa_actor_settings.max_gru_per_job);
  
  aout(self) << "Starting SUMMA With "
             << self->state.batch_container.getBatchesRemaining() 
             << " Batches\n"
             << "###################################################\n"
             << self->state.batch_container.getBatchesAsString()
             << "###################################################\n";
  std::optional<Batch> batch = 
      self->state.batch_container.getUnsolvedBatch();
  if (!batch.has_value()) {
    aout(self) << "ERROR--Summa_Actor: No Batches To Solve\n";
    self->quit(); 
    return {};
  } 

  self->state.current_batch_id = batch->getBatchID();
  aout(self) << "Starting Batch " << self->state.current_batch_id + 1 << "\n";
  auto batch_val = batch.value();
  self->state.currentJob = self->spawn(job_actor, batch->getStartHRU(), 
      batch->getNumHRU(), self->state.file_access_actor_settings, 
      self->state.job_actor_settings, self->state.hru_actor_settings, self);

  return {
    [=](done_job, int numFailed, double job_duration, double read_duration, 
        double write_duration) {
      
      self->state.batch_container.updateBatch_success(
          self->state.current_batch_id, job_duration, read_duration,
          write_duration);

      aout(self) << "###########################################\n"
                 << "Job Finished: " 
                 << self->state.batch_container.getTotalBatches() - 
                 self->state.batch_container.getBatchesRemaining() 
                 << "/" << self->state.batch_container.getTotalBatches() << "\n"
                 << "###########################################\n";
      
      self->state.numFailed += numFailed;
      
    
      
      if (self->state.batch_container.hasUnsolvedBatches()) {
        spawnJob(self);
      } else {
        aout(self) << "All Batches Finished\n"
                   << self->state.batch_container.getAllBatchInfoString();
        self->state.summa_actor_timing.updateEndPoint("total_duration");

        double total_dur_sec = self->state.summa_actor_timing.getDuration(
            "total_duration").value_or(-1.0);
        double total_dur_min = total_dur_sec / 60;
        double total_dur_hr = total_dur_min / 60;
        double read_dur_sec = self->state.batch_container.getTotalReadTime();
        double write_dur_sec = self->state.batch_container.getTotalWriteTime();
         
        aout(self) << "\n________________SUMMA INFO________________\n"
                   << "Total Duration = " << total_dur_sec << " Seconds\n"
                   << "Total Duration = " << total_dur_min << " Minutes\n"
                   << "Total Duration = " << total_dur_hr << " Hours\n"
                   << "Total Read Duration = " << read_dur_sec << "Seconds\n"
                   << "Total Write Duration = " << write_dur_sec << "Seconds\n"
                   << "Num Failed = " << self->state.numFailed << "\n"
                   << "___________________Program Finished__________________\n";
        
        self->send(self->state.parent, done_batch_v, total_dur_sec, 
                   read_dur_sec, write_dur_sec);
        self->quit();
        // exit(0);
      }
    },

    [=](err_atom) {
      aout(self) << "Unrecoverable Error: Attempting To Fail Gracefully\n";
      self->quit();
    }
  };
}


void spawnJob(stateful_actor<summa_actor_state>* self) {
  std::optional<Batch> batch =
          self->state.batch_container.getUnsolvedBatch();
  self->state.current_batch_id = batch->getBatchID();
  aout(self) << "Starting Batch " << self->state.current_batch_id + 1 << "\n";
  auto batch_val = batch.value();
  self->state.currentJob = self->spawn(job_actor, batch->getStartHRU(), 
      batch->getNumHRU(), self->state.file_access_actor_settings, 
      self->state.job_actor_settings, self->state.hru_actor_settings, 
      self);
}

} // end namespace


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



