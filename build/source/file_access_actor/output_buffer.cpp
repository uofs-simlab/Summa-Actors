#include "output_buffer.hpp"


using chrono_time = std::chrono::time_point<std::chrono::high_resolution_clock>;

// ****************************************************************************
// OutputBuffer
// ****************************************************************************

int OutputBuffer::getNumStepsBuffer(int gru_index) {
  int partition_index = findPartitionIndex(gru_index);
  if (partition_index == -1) {
    std::cout << "Error: FileAccessActor -- addFailedGRU: "
              << "Could not find partition for GRU: " << gru_index << "\n";
    return -1;
  }


  return partitions_[partition_index]->getNumStepsBuffer();
} 

int OutputBuffer::defOutput(const std::string& actor_address) {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);

  int start_gru = num_gru_info_.start_gru_local;
  int num_gru = num_gru_info_.num_gru_local;
  int file_gru = num_gru_info_.file_gru;

  std::string output_extention = "";
  if (num_gru_info_.use_global_for_data_structures) {
    output_extention = actor_address;
  }

  if (fa_settings_.output_file_suffix_ != "") {
    output_extention = fa_settings_.output_file_suffix_;
    num_gru_info_.use_global_for_data_structures = true;
  }

  //turning off writing to file for now
  /*
  f_defOutput(handle_ncid_.get(), start_gru, num_gru, num_hru_, file_gru,
              num_gru_info_.use_global_for_data_structures, 
              output_extention.c_str(), err, &message);
	      */

  err = 0;
  if (err != 0) {
    std::cout << "Error: File Access Actor -- f_defOutput: " 
              << message.get() << "\n";
  }
  
  return err;
}


int OutputBuffer::setChunkSize() {
  int chunk_size = std::ceil(static_cast<double>(num_gru_info_.num_gru_local) 
                             / fa_settings_.num_partitions_in_output_buffer_);
  f_setChunkSize(chunk_size);
  return chunk_size;
}


int OutputBuffer::allocateOutputBuffer(int num_timesteps) {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  int num_gru = num_gru_info_.num_gru_local;

  if(num_timesteps < fa_settings_.num_timesteps_in_output_buffer_) {
    fa_settings_.num_timesteps_in_output_buffer_ = num_timesteps;
  }

  f_allocateOutputBuffer(fa_settings_.num_timesteps_in_output_buffer_,
                         num_gru, err, &message);
  if (err != 0) {
    std::cout << "Error: FileAccessActor -- f_allocateOutputBuffer: " 
              << message.get() << "\n";
  }
  return err;
}

const int OutputBuffer::writeOutputDA(const int output_step) {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  int start_gru = partitions_[0]->getStartGru();
  int end_gru = partitions_[0]->getEndGru();
  f_writeOutputDA(handle_ncid_.get(), output_step, start_gru, end_gru, 
                  write_params_da_, err, &message);
  if (err != 0) {
    std::cout << "Error: FileAccessActor -- f_writeOutputDA: " 
              << message.get() << "\n";
  }

  // Only write Parameters Once
  if (write_params_da_) {write_params_da_ = false;}
  return err;
}

const std::optional<WriteOutputReturn*> OutputBuffer::writeOutput(
    int index_gru, caf::actor gru) {
  int partition_index = findPartitionIndex(index_gru);
  if (partition_index == -1) {
    std::cout << "Error: FileAccessActor -- addFailedGRU: "
              << "Could not find partition for GRU: " << index_gru << "\n";
    return {};
  }
  // Will write if the partition is full
  // TODO: This is a bit of a hack, the handle_ncid should be a shared pointer
  return partitions_[partition_index]->writeOutput(gru, handle_ncid_.get());
}

const std::optional<WriteOutputReturn*> OutputBuffer::addFailedGRU(int index) {
  f_addFailedGru(index);
  
  failed_grus_.push_back(index);

  int partition_index = findPartitionIndex(index);
  if (partition_index == -1) {
    std::cout << "Error: FileAccessActor -- addFailedGRU: "
              << "Could not find partition for GRU: " << index << "\n";
    return {};
  }


  partitions_[partition_index]->decrementNumGRU();
  return partitions_[partition_index]->writeOutput(handle_ncid_.get());
}

// 
int OutputBuffer::findPartitionIndex(int index) {
  for (int i = 0; i < partitions_.size(); i++) {
    if (index >= partitions_[i]->getStartGru() && 
        index <= partitions_[i]->getEndGru()) {
      return i;
    }
  }
  return -1;
}


// Reconstruction of the partitions for rerunning failed GRUs
void OutputBuffer::reconstruct() {
  // clear all partitons
  partitions_.clear();

  f_resetFailedGru();
  for (int gru : failed_grus_) {
    f_resetOutputTimestep(gru);
    partitions_.push_back(std::make_unique<OutputPartition>(
        gru, 1, num_buffer_steps_, num_timesteps_));
  }
}



// ****************************************************************************
// OutputPartition
// ****************************************************************************
const std::optional<WriteOutputReturn*> OutputPartition::writeOutput(
    caf::actor gru, void* handle_ncid) {
  ready_to_write_.push_back(gru);
  if (ready_to_write_.size() == num_gru_ && ready_to_write_.size() > 0) {
    // Write the output
    int err = 0;
    std::unique_ptr<char[]> message(new char[256]);
    bool write_params = isWriteParams();
    
    /*
    writeOutput_fortran(handle_ncid, num_steps_buffer_, start_gru_, end_gru_, 
                        write_params, err, &message);
    
    */

    // recalculate the number of steps to send to grus
    steps_remaining_ -= num_steps_buffer_;
    if (steps_remaining_ < num_steps_buffer_) {
      num_steps_buffer_ = steps_remaining_;
    }

    write_status_.err = err;
    write_status_.message = message.get();
    write_status_.actor_to_update = ready_to_write_;
    write_status_.num_steps_update = num_steps_buffer_;

    // Reset the partition for the next set of writes
    ready_to_write_.clear();

    f_setFailedGruMissing(start_gru_, end_gru_);
    
    return std::optional<WriteOutputReturn*>(&write_status_);
  }
  return {};
}

const std::optional<WriteOutputReturn*> OutputPartition::writeOutput(
    void* handle_ncid) {
  if (isReadyToWrite()) {
    // Write the output
    int err = 0;
    std::unique_ptr<char[]> message(new char[256]);
    bool write_params = isWriteParams();
    //writeOutput_fortran(handle_ncid, num_steps_buffer_, start_gru_, end_gru_, 
    //                    write_params, err, &message);
    // recalculate the number of steps to send to grus
    steps_remaining_ -= num_steps_buffer_;
    if (steps_remaining_ < num_steps_buffer_) {
      num_steps_buffer_ = steps_remaining_;
    }

    write_status_.err = err;
    write_status_.message = message.get();
    write_status_.actor_to_update = ready_to_write_;
    write_status_.num_steps_update = num_steps_buffer_;

    // Reset the partition for the next set of writes
    ready_to_write_.clear();
    return std::optional<WriteOutputReturn*>(&write_status_);
  }
  return {};
}

bool OutputPartition::isWriteParams() {
  if (write_params_) {
    write_params_ = false;
    return true;
  }
  return write_params_;
}

