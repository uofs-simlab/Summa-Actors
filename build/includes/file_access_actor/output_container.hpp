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


struct hru_output_handles {
    // Statistic Structures
    void* handle_forc_stat        = new_handle_var_dlength();
    void* handle_prog_stat        = new_handle_var_dlength();
    void* handle_diag_stat        = new_handle_var_dlength();
    void* handle_flux_stat        = new_handle_var_dlength();
    void* handle_indx_stat        = new_handle_var_dlength();
    void* handle_bvar_stat        = new_handle_var_dlength();
    // primary data structures (scalars)
    void* handle_time_struct      = new_handle_var_i();
    void* handle_forc_struct      = new_handle_var_d();
    void* handle_attr_struct      = new_handle_var_d();
    void* handle_type_struct      = new_handle_var_i();
    void* handle_id_struct        = new_handle_var_i8();
    // primary data structures (variable length vectors)
    void* handle_indx_struct      = new_handle_var_ilength();
    void* handle_mpar_struct      = new_handle_var_dlength();
    void* handle_prog_struct      = new_handle_var_dlength();
    void* handle_diag_struct      = new_handle_var_dlength();
    void* handle_flux_struct      = new_handle_var_dlength();
    // basin-average structures
    void* handle_bpar_struct      = new_handle_var_d();
    void* handle_bvar_struct      = new_handle_var_dlength();
    // ancillary data structures
    void* handle_dpar_struct      = new_handle_var_d();
    void* handle_finalize_stats   = new_handle_var_i();
    void* handle_output_timestep  = new_handle_var_i();

    ~hru_output_handles() {
        // statistics structures
        delete_handle_var_dlength(handle_forc_stat);
        delete_handle_var_dlength(handle_prog_stat);
        delete_handle_var_dlength(handle_diag_stat);
        delete_handle_var_dlength(handle_flux_stat);
        delete_handle_var_dlength(handle_indx_stat);
        delete_handle_var_dlength(handle_bvar_stat);
        // primary data structures (scalars)
        delete_handle_var_i(handle_time_struct);
        delete_handle_var_d(handle_forc_struct);
        delete_handle_var_d(handle_attr_struct);
        delete_handle_var_i(handle_type_struct);
        delete_handle_var_i8(handle_id_struct);
        // primary data structures (variable length vectors)
        delete_handle_var_ilength(handle_indx_struct);
        delete_handle_var_dlength(handle_mpar_struct);
        delete_handle_var_dlength(handle_prog_struct);
        delete_handle_var_dlength(handle_diag_struct);
        delete_handle_var_dlength(handle_flux_struct);
        // basin-average structures
        delete_handle_var_d(handle_bpar_struct);
        delete_handle_var_dlength(handle_bvar_struct);
        // ancillary data structures
        delete_handle_var_d(handle_dpar_struct);
        // counter variables
        delete_handle_var_i(handle_output_timestep);
        delete_handle_flagVec(handle_finalize_stats);
    }
};

struct hru_output_info {
    caf::actor hru_actor;
    int index_hru;
    int index_gru;
    bool ready_to_write;
};


struct output_partition {
    int start_gru;
    int num_gru;
    int num_active_gru;            
    int num_timesteps;     
    int simulation_timesteps_remaining;
    int grus_ready_to_write;
    // 2D matrix of output handles
    std::vector<std::shared_ptr<hru_output_info>> hru_info_and_data;
};



// Take an unintialized vector of output partitions and initialize it
void initArrayOfOuputPartitions(std::vector<std::shared_ptr<output_partition>>& output_partitions, 
    int num_partitions, int num_gru, int num_timesteps,  int simulation_timesteps_remaining);

// Add a HRU that is ready to write to the output structure
std::optional<int> addReadyToWriteHRU(std::vector<std::shared_ptr<output_partition>>& output_partitions, 
    caf::actor hru_actor, int gru_index, int hru_index);


// find which partition the HRU belongs to
int findPatritionIndex(int grus_per_partition, int gru_index, int num_partitions);

// After writing to a file, update the number of timesteps remaining in the simulation
void updateSimulationTimestepsRemaining(std::shared_ptr<output_partition>& output_partition);

// After writing to a file, check if we need to send the hru a modified timestep value b/c we have less simulation timesteps remaining than 
// the number of timesteps in the output file
void updateNumTimeForPartition(std::shared_ptr<output_partition> &output_partition);

void resetReadyToWrite(std::shared_ptr<output_partition> &output_partition);

/*
 * Reduce the number of GRUs the partition is waiting on to write to file by 1
 * Check if the partition is ready to write to file and return the partition index if it is
*/
std::optional<int> updatePartitionWithFailedHRU(std::vector<std::shared_ptr<output_partition>>& output_partitions, 
    int local_gru_index);
