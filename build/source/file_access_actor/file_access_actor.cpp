#include "file_access_actor.hpp"

using json = nlohmann::json;
using namespace caf;

const int NOTIFY_ERR = -1;  // Error code for notification but not quitting

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

  num_output_steps_ = fa_settings_.num_timesteps_in_output_buffer_;

  return {
    [this](init_file_access_actor, int file_gru, int num_hru) {
      self_->println("File Access Actor: Initializing\n");
      num_hru_ = num_hru;

      forcing_files_ = std::make_unique<forcingFileContainer>();
      if (forcing_files_->initForcingFiles() != 0) return -1;

      int err = 0;
      std::unique_ptr<char[]> message(new char[256]);
      fileAccessActor_init_fortran(num_steps_, num_output_steps_, num_gru_, err, 
                                   &message);
      if (err != 0) {
        self_->println("\n\nFile Access Actor: Error fileAccessActor_init\n"
                       "\tMessage = {}\n\n", message.get());
        return -1;
      }

      if (num_steps_ < num_output_steps_) {
        num_output_steps_ = num_steps_;
        fa_settings_.num_timesteps_in_output_buffer_ = num_steps_;
      }

      // Set up the output container
      if (!num_gru_info_.use_global_for_data_structures) {
        output_container_ = std::make_unique<Output_Container>(
            fa_settings_.num_partitions_in_output_buffer_, num_gru_,
            fa_settings_.num_timesteps_in_output_buffer_, num_steps_);
      }

      self_->println("Creating Output File\n");
      std::string actor_address = "";  
      if (num_gru_info_.use_global_for_data_structures) {
        actor_address = "_" + to_string(self_->address());
      }

      if (fa_settings_.output_file_suffix_ != "") {
        actor_address = "_" + fa_settings_.output_file_suffix_;
        num_gru_info_.use_global_for_data_structures = true;
      }

      defOutputFortran(handle_ncid_, start_gru_, num_gru_, num_hru_, file_gru, 
                       num_gru_info_.use_global_for_data_structures,
                       actor_address.c_str(), err);
      if (err != 0) return -1;

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

    [this](get_num_output_steps) -> int {
      return num_output_steps_;
    },

    [this](write_output, int index_gru, int index_hru, caf::actor hru_actor) {
      timing_info_.updateStartPoint("write_duration");

      Output_Partition *output_partition = 
          output_container_->getOutputPartition(index_gru);

      output_partition->setGRUReadyToWrite(hru_actor);
        
      if (output_partition->isReadyToWrite()) {
        writeOutput(output_partition);
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

    [this](write_output, int steps_to_write, int start_gru, 
           int max_gru) -> int {
      timing_info_.updateStartPoint("write_duration");
      std::unique_ptr<char[]> err_msg(new char[256]);
      int err = 0;
      writeOutput_fortran(handle_ncid_, steps_to_write, start_gru, max_gru, 
                          write_params_flag_, err, &err_msg);
      if (err != 0) {
        self_->println("File Access Actor: Error writeOutput from job\n"
                       "\tMessage = {}\n", err_msg.get());
        self_->mail(err_atom_v, 0, 0, err, err_msg.get()).send(parent_);
      }

      if (write_params_flag_) write_params_flag_ = false;
      timing_info_.updateEndPoint("write_duration");
      return err;
    },

    [this](restart_failures) {
      output_container_->reconstruct();
    },

    [this](run_failure, int local_gru_index) {
      timing_info_.updateStartPoint("write_duration");
      Output_Partition *output_partition = 
          output_container_->getOutputPartition(local_gru_index);
        
      output_partition->addFailedGRUIndex(local_gru_index);

      if (output_partition->isReadyToWrite()) {
        writeOutput(output_partition);
      }
      timing_info_.updateEndPoint("write_duration");
    },

    [this](finalize) {
      self_->println("File Access Actor: Deallocating Structures\n");
      FileAccessActor_DeallocateStructures(handle_ncid_);
      self_->println("\n________________" 
                     "FILE_ACCESS_ACTOR TIMING INFO RESULTS________________\n"
                      "Total Read Duration = {}\n"
                     "Total Write Duration = {}\n"
                     "\n__________________________________________________\n",
                      forcing_files_->getReadDuration(),
                      timing_info_.getDuration("write_duration").value_or(-1.0));
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

void FileAccessActor::writeOutput(Output_Partition* partition) {              
  int num_timesteps_to_write = partition->getNumStoredTimesteps();
  int start_gru = partition->getStartGRUIndex();
  int max_gru = partition->getMaxGRUIndex();
  if (start_gru > max_gru) {
    self_->println("File Access Actor: Error writeOutput\n"
                   "\tMessage = start_gru > max_gru\n");
    self_->mail(err_atom_v, 0, 0, NOTIFY_ERR, "start_gru > max_gru")
        .send(parent_);
    return;
  }
  bool write_param_flag = partition->isWriteParams();
  int err = 0;
  std::unique_ptr<char[]> err_msg(new char[256]);
  writeOutput_fortran(handle_ncid_, num_timesteps_to_write, start_gru, max_gru, 
                      write_param_flag, err, &err_msg);
  if (err != 0) {
    self_->println("File Access Actor: Error writeOutput\n\tMessage = {} \n", 
                   err_msg.get());
    self_->mail(err_atom_v, 0, 0, NOTIFY_ERR, err_msg.get()).send(parent_);
  }

  partition->updateTimeSteps();

  int num_steps_before_next_write = partition->getNumStoredTimesteps();

  std::vector<caf::actor> hrus_to_update = partition->getReadyToWriteList();
  for (int i = 0; i < hrus_to_update.size(); i++) {
    self_->mail(num_steps_before_write_v, num_steps_before_next_write)
        .send(hrus_to_update[i]);
    self_->mail(run_hru_v).send(hrus_to_update[i]);
  }

  partition->resetReadyToWriteList();
}

void FileAccessActor::writeRestart(Output_Partition* partition, int start_gru, 
                                   int num_gru, int timestep, int year, 
                                   int month, int day, int hour){  
  int err = 0;
  writeRestart_fortran(handle_ncid_, start_gru, num_gru, timestep, year, month, 
                       day, hour, err);
}


