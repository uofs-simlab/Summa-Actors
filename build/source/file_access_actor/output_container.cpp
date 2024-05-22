#include "output_container.hpp"


//###################################################################
// Output_Partition
//###################################################################
Output_Partition::Output_Partition(int start_local_gru_index, 
                                   int num_local_grus, 
                                   int num_timesteps_simulation, 
                                   int num_stored_timesteps) {
  this->start_local_gru_index = start_local_gru_index;
  this->num_local_grus = num_local_grus;
  this->num_timesteps_simulation = num_timesteps_simulation;
  this->num_stored_timesteps = num_stored_timesteps;
  this->end_local_gru_index = start_local_gru_index + num_local_grus - 1;
  this->num_active_grus = num_local_grus;

}

Output_Partition::~Output_Partition() {
  // nop
}

void Output_Partition::setGRUReadyToWrite(caf::actor gru_actor) {
  this->ready_to_write_list.push_back(gru_actor);
}

bool Output_Partition::isReadyToWrite() {
  return this->ready_to_write_list.size() == this->num_active_grus && 
         this->ready_to_write_list.size() > 0;
}

int Output_Partition::getMaxGRUIndex() {
  return this->end_local_gru_index;
}

int Output_Partition::getNumStoredTimesteps() {
  return this->num_stored_timesteps;
}

int Output_Partition::getStartGRUIndex() {
  return this->start_local_gru_index;
}

void Output_Partition::updateTimeSteps() {
    
  // Update the number of timesteps remaining in the simulation
  this->num_timesteps_simulation -= this->num_stored_timesteps;
  
  // Reset the number of timesteps to store for the next run
  if (this->num_timesteps_simulation < this->num_stored_timesteps) {
    this->num_stored_timesteps = this->num_timesteps_simulation;
  }

}

std::vector<caf::actor> Output_Partition::getReadyToWriteList() {
  return this->ready_to_write_list;
}

void Output_Partition::resetReadyToWriteList() {
  this->ready_to_write_list.clear();
}

void Output_Partition::addFailedGRUIndex(int local_gru_index) {
  // Special case where the failing GRU is the last or first GRU in the partition
  // This will affect writing of output if a failed GRU is the last or first GRU
  if (local_gru_index == this->end_local_gru_index) {
    this->end_local_gru_index -= 1;
  } else if (local_gru_index == this->start_local_gru_index) {
    this->start_local_gru_index += 1;
  }

  this->num_active_grus -= 1;

  this->failed_gru_index_list.push_back(local_gru_index);
}

int Output_Partition::getNumActiveGRUs() {
  return this->num_active_grus;
}

int Output_Partition::getNumLocalGRUs() {
  return this->num_local_grus;
}

int Output_Partition::getRemainingTimesteps() {
  return this->num_timesteps_simulation;
}

std::vector<int> Output_Partition::getFailedGRUIndexList() {
  return this->failed_gru_index_list;
}

bool Output_Partition::isWriteParams() {
  if (this->write_params) {
    this->write_params = false;
    return true;
  }
  return this->write_params;
}


//###################################################################
// Output_Container
//###################################################################

Output_Container::Output_Container(int num_partitions, int num_grus, int num_stored_timesteps, int num_timesteps_simulation) {
  this->num_grus = num_grus;
  this->num_timesteps_simulation = num_timesteps_simulation;
  this->num_stored_timesteps = num_stored_timesteps;

  // Set the number of partitions - avoiding division with a remainder
  if (num_partitions > num_grus) {
    this->num_partitions = num_grus;
  } else {
    this->num_partitions = num_partitions;
  }

  // Initialize the output partitions
  int start_gru_counter = 1;
  this->num_grus_per_partition = std::round(this->num_grus / this->num_partitions);
  for (int i = 0; i < num_partitions - 1; i++) {
    this->output_partitions.push_back(new Output_Partition(start_gru_counter, this->num_grus_per_partition, this->num_timesteps_simulation, this->num_stored_timesteps));
    start_gru_counter += this->num_grus_per_partition;
  }
  // The last partition will have the remainder of the GRUs
  this->output_partitions.push_back(new Output_Partition(start_gru_counter, this->num_grus - start_gru_counter + 1, this->num_timesteps_simulation, this->num_stored_timesteps));
}

Output_Container::~Output_Container() {
  for (int i = 0; i < this->num_partitions; i++) {
    delete this->output_partitions[i];
  }
}

Output_Partition* Output_Container::getOutputPartition(int local_gru_index) {
  int partition_index = this->findPartition(local_gru_index);
  return this->output_partitions[partition_index];
}

int Output_Container::findPartition(int local_gru_index) {
  // If we are not rerunning failed GRUs, then the partition index can be found within a block of GRUs
  if (!this->rerunning_failed_hrus) {
    int partition_index = (local_gru_index - 1) / this->num_grus_per_partition;
    // correct the value if too large (more than the number of partitions)
    if (partition_index > this->num_partitions - 1) {
      partition_index = this->num_partitions - 1;
    }
    return partition_index;
  } else {
    // If we are rerunning failed GRUs, they may not be grouped in blocks, so we need to use the failed GRU index list
    std::vector<int>::iterator it = std::find(this->failed_gru_index_list.begin(), this->failed_gru_index_list.end(), local_gru_index);
    if (it != this->failed_gru_index_list.end()) {
      return std::distance(this->failed_gru_index_list.begin(), it);
    } else {
      std::cout << "GRU index: " << local_gru_index << std::endl;
      for (int i = 0; i < this->failed_gru_index_list.size(); i++) {
        std::cout << this->failed_gru_index_list[i] << std::endl;
      }
      throw std::runtime_error("GRU index not found in failed GRU index list");
    }
  }
}

int Output_Container::getNumPartitions() {
  return this->num_partitions;
}

std::vector<int> Output_Container::getFailedGRUIndexList() {
  return this->failed_gru_index_list;
}

void Output_Container::reconstruct() {

  this->failed_gru_index_list;

  // Loop over all partitions getting the failed GRU index list
  for (int i = 0; i < this->num_partitions; i++) {
    std::vector<int> partition_failed_gru_index_list = this->output_partitions[i]->getFailedGRUIndexList();
      failed_gru_index_list.insert(failed_gru_index_list.end(), 
                                   partition_failed_gru_index_list.begin(), 
                                   partition_failed_gru_index_list.end());
      delete this->output_partitions[i];
    }
    this->output_partitions.clear();

    std::sort(this->failed_gru_index_list.begin(), this->failed_gru_index_list.end());
    // Reconstruct the output partitions
    this->num_partitions = failed_gru_index_list.size();
    this->num_grus = failed_gru_index_list.size();
    this->num_grus_per_partition = 1;
    for (int i = 0; i < failed_gru_index_list.size(); i++){
      this->output_partitions.push_back(new 
          Output_Partition(failed_gru_index_list[i], 1, 
                           this->num_timesteps_simulation, 
                           this->num_stored_timesteps));
    }

  this->rerunning_failed_hrus = true;
}


