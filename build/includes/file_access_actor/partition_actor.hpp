#pragma once

#include "caf/all.hpp"
namespace caf {

/*
* This struct stores the information about a specific HRU
*/
struct hru_output_info {
    caf::actor hru_actor;
    int local_hru_index;
    int local_gru_index;
    bool ready_to_write;
};


struct partition_actor_state {
    int start_local_gru_index; // The index of the first GRU in the partition
    int num_local_grus; // The number of GRUs in the partition
    int num_timesteps_simulation; // The number of timesteps in the simulation
    int num_timesteps_buffer; // The number of timesteps this actor can hold before needing to write
    int num_non_failed_grus = 0; // The number of GRUs that have not failed
    bool grus_ready_to_write = false; // Whether or not all GRUs in the partition are ready to write

    std::vector<std::unique_ptr<hru_output_info>> gru_list; // The GRU actors and their data


};


behavior partition_actor(stateful_actor<partition_actor_state>* self, int start_local_gru_index, 
    int num_local_grus, int num_timesteps_simulation, int num_timesteps_buffer);


}