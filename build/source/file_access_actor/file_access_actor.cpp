#include "file_access_actor.hpp"

using json = nlohmann::json;
using namespace caf;

const int NOTIFY_ERR = -1;  // Error code for notification but not quitting
using chrono_time = std::chrono::time_point<std::chrono::high_resolution_clock>;

behavior FileAccessActor::make_behavior() {
  self_->println("\n----------File_Access_Actor Started----------\n");

  // Timing Info
  timing_info_ = TimingInfo();
  timing_info_.addTimePoint("write_duration");

  if (num_gru_info_.use_global_for_data_structures) {
    start_gru_ = num_gru_info_.start_gru_global;
    num_gru_ = num_gru_info_.num_gru_global;
  } else {
    start_gru_ = num_gru_info_.start_gru_local;
    num_gru_ = num_gru_info_.num_gru_local;
  }

  return {
    [this](init_file_access_actor, int file_gru, int num_hru) {
      int err = 0;
      self_->println("File Access Actor: Initializing\n");
      num_hru_ = num_hru;

      f_getNumTimeSteps(num_steps_);

      forcing_files_ = std::make_unique<forcingFileContainer>();
      if (forcing_files_->initForcingFiles() != 0) return -1;

      // Initialize output buffer
      output_buffer_ = std::make_unique<OutputBuffer>(
          fa_settings_, num_gru_info_, num_hru_, num_steps_);
      int chunk_return = output_buffer_->setChunkSize();
      self_->println("Chunk Size = {}\n", chunk_return);
      err = output_buffer_->defOutput(to_string(self_->address()));
      if (err != 0) {
        self_->println("File Access Actor: Error defOutput\n"
                       "\tMessage = Can't define output file\n");
        self_->quit();
        return -1;
      }
      err = output_buffer_->allocateOutputBuffer(num_steps_);

      timing_info_.updateEndPoint("init_duration");
      return num_steps_;
    },

    [this](access_forcing, int i_file, caf::actor response_ref) {
      if (forcing_files_->allFilesLoaded()) {
        self_->mail(new_forcing_file_v, forcing_files_->getNumSteps(i_file), 
                    i_file).send(response_ref);
        return;
      }
      auto err = forcing_files_->loadForcingFile(i_file, start_gru_, num_gru_);
      if (err != 0) {
        self_->println("File Access Actor: Error loadForcingFile\n"
                       "\tMessage = Can't load forcing file\n");
        self_->mail(err_atom_v, 0, 0, err, "Can't load forcing file\n")
            .send(parent_);
        self_->quit();
        return;
      }

      // Load files behind the scenes
      self_->mail(access_forcing_internal_v, i_file + 1).send(self_);
      self_->mail(new_forcing_file_v, forcing_files_->getNumSteps(i_file), 
                  i_file).send(response_ref);
    },

    [this](access_forcing_internal, int i_file) {
      if (forcing_files_->allFilesLoaded()) return;
      auto err = forcing_files_->loadForcingFile(i_file, start_gru_, num_gru_);
      if (err != 0) {
        self_->println("File Access Actor: Error loadForcingFile\n"
                       "\tMessage = Can't load forcing file\n");
        self_->mail(err_atom_v, 0, 0, err, "Can't load forcing file\n")
            .send(parent_);
        self_->quit();
        return;
      }
      self_->mail(access_forcing_internal_v, i_file + 1).send(self_);
    },

    [this](get_num_output_steps, int job_index) -> int {
      return output_buffer_->getNumStepsBuffer(job_index);
    },

    [this](write_output, int index_gru, caf::actor gru) {
      timing_info_.updateStartPoint("write_duration");

      auto update_status = output_buffer_->writeOutput(index_gru, gru);
      
      // Do nothing if optional is emtpy
      if (!update_status.has_value()) {
        timing_info_.updateEndPoint("write_duration");
        return;
      }
      
      // If error, send error message to parent
      if (update_status.value()->err != 0) {
        self_->println("File Access Actor: Error writeOutput\n"
                       "\tMessage = {}\n", update_status.value()->message);
        self_->mail(err_atom_v, 0, 0, update_status.value()->err, 
                    update_status.value()->message).send(parent_);
        self_->quit();
        return;
      }

      // If we get here, we successfully wrote to file
      for (auto gru : update_status.value()->actor_to_update) {
        self_->mail(num_steps_before_write_v, 
                    update_status.value()->num_steps_update).send(gru);
        self_->mail(run_hru_v).send(gru);
      }

      timing_info_.updateEndPoint("write_duration");
    },

    [this](write_restart, int gru, int gru_timestep, int gru_checkpoint, 
          int output_structure_index, int year, int month, int day, int hour) {
      // update hru progress vecs
      int gru_index = abs(start_gru_ - gru);
      hru_timesteps_[gru_index] = gru_timestep;
      hru_checkpoints_[gru_index] = gru_checkpoint;
      // find slowest time step of all hrus in job, stored hru_timesteps_
      int slowest_timestep = *std::min_element(
          hru_timesteps_.begin(), hru_timesteps_.end());
      int slowest_checkpoint = *std::min_element(
          hru_checkpoints_.begin(), hru_checkpoints_.end());
    
      // if the slowest hru is past the ith checkpoint (current threshold)
      if (slowest_checkpoint >= completed_checkpoints_) {
        Output_Partition *output_partition = 
            output_container_->getOutputPartition(gru - 1);
        writeRestart(output_partition, start_gru_, num_gru_, 
                     output_structure_index, year, month, day, hour);
        completed_checkpoints_++;
      }
    },

    // Write Output From the Job Actor
    [this](write_output, int output_step) {
      timing_info_.updateStartPoint("write_duration");
      const int err = output_buffer_->writeOutputDA(output_step);
      timing_info_.updateEndPoint("write_duration");
      self_->mail(write_output_v, err).send(parent_);
    },

    [this](restart_failures) {
      output_container_->reconstruct();
    },

    [this](run_failure, int index_gru_job) {
      timing_info_.updateStartPoint("write_duration");
      auto update_status = output_buffer_->addFailedGRU(index_gru_job);

      if (!update_status.has_value()) {
        timing_info_.updateEndPoint("write_duration");
        return;
      }

      if (update_status.value()->err != 0) {
        self_->println("File Access Actor: Error writeOutput\n"
                       "\tMessage = {}\n", update_status.value()->message);
        self_->mail(err_atom_v, 0, 0, update_status.value()->err, 
                    update_status.value()->message).send(parent_);
        self_->quit();
        return;
      }

      for (auto gru : update_status.value()->actor_to_update) {
        self_->mail(num_steps_before_write_v, 
                    update_status.value()->num_steps_update).send(gru);
        self_->mail(run_hru_v).send(gru);
      }

      timing_info_.updateEndPoint("write_duration");
    },

    [this](finalize) {
      self_->println("\n________________" 
                     "FILE_ACCESS_ACTOR TIMING INFO RESULTS________________\n"
                     "Total Read Duration = {}\n"
                     "Total Write Duration = {}\n"
                     "\n__________________________________________________\n",
                     forcing_files_->getReadDuration(),
                     timing_info_.getDuration("write_duration").value_or(-1.0));
      
      output_buffer_.reset();

      self_->quit();
      return std::make_tuple(forcing_files_->getReadDuration(),
                             timing_info_.getDuration("write_duration")
                             .value_or(-1.0));                       
                     
    },

    [=](const caf::exit_msg& em) {
      self_->println("File Access Actor: Received Exit Message");
    },
  };
}


void FileAccessActor::writeRestart(Output_Partition* partition, int start_gru, 
                                   int num_gru, int timestep, int year, 
                                   int month, int day, int hour){  
  int err = 0;
  // writeRestart_fortran(handle_ncid_, start_gru, num_gru, timestep, year, month, 
  //                      day, hour, err);
}


