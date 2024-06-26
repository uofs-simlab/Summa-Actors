#pragma once

#include "caf/actor.hpp"
#include <optional>
#include <cmath>
#include "fortran_data_types.hpp"
#include <vector>
#include <iostream>




/*
 * This class manages a portion of the HRUs in the model.
 * All HRUs are grouped into partitions/objects of this class.
 */
class Output_Partition {
  private:
    int start_local_gru_index;    // The index of the first GRU in the partition
    int end_local_gru_index;      // The index of the last GRU in the partition
    int num_local_grus;           // The number of GRUs in the partition
    int num_active_grus;          // The number of GRUs that have not failed
    int num_timesteps_simulation; // The number of timesteps in the simulation
    int num_stored_timesteps;     // The number of timesteps held within the partition
    bool write_params = true;     // Flag to write the parameters to the output file (only performed once)

    std::vector<caf::actor> ready_to_write_list;
    std::vector<int> failed_gru_index_list; // The list of GRUs that have failed
  public:
    Output_Partition(int start_local_gru_index, int num_local_grus, int num_timesteps_simulation, int num_timesteps_buffer);
    ~Output_Partition();

    // Set the GRU to ready to write
    void setGRUReadyToWrite(caf::actor gru_actor);

    // Check if all GRUs are ready to write
    bool isReadyToWrite();

    // Get the max index of the GRUs in the partition
    int getMaxGRUIndex();

    // Get the number of timesteps stored in the partition
    int getNumStoredTimesteps();

    // Get the start gru index
    int getStartGRUIndex();

    // Update the number of timesteps remaining in the simulation
    void updateTimeSteps();

    // Get the list of GRUs that have written so we can send them the next set of timesteps
    std::vector<caf::actor> getReadyToWriteList();

    // Reset the list of GRUs that are ready to write
    void resetReadyToWriteList();

    // Add a GRU index to the list of failed GRUs
    void addFailedGRUIndex(int local_gru_index);

    std::vector<int> getFailedGRUIndexList();

    int getNumActiveGRUs();

    int getNumLocalGRUs();

    int getRemainingTimesteps();

    bool isWriteParams();

};


/*
 * This class is used to store informaiton about when 
 * HRUs are ready to write. This class does not store
 * the data of the HRUs only the information about if 
 * HRUs are ready to write and which HRUs should be 
 * written to the output file.
*/
class Output_Container {
  private:
    int num_partitions; // The number of partitions in the model
    int num_grus_per_partition; // The average number of GRUs per partition
    int num_grus; // The number of GRUs in the model
    int num_timesteps_simulation; // The number of timesteps in the simulation
    int num_stored_timesteps; // The number of timesteps a partion can hold before needing to write

    bool rerunning_failed_hrus = false;
    std::vector<Output_Partition*> output_partitions; // This is the main output partition
    std::vector<int> failed_gru_index_list; // The list of GRUs that have failed
    // Private Method


  public:
    Output_Container(int num_partitions, int num_grus, int num_timesteps_simulation, int num_timesteps_buffer);
    ~Output_Container();

    int findPartition(int local_gru_index);

    int getNumPartitions();

    // The output container needs to be restructured when rerunning the failed GRUs.
    void reconstruct();

    Output_Partition* getOutputPartition(int local_gru_index);

    std::vector<int> getFailedGRUIndexList();

};
