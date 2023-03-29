#include "output_container.hpp"



void initArrayOfOuputPartitions(std::vector<std::shared_ptr<output_partition>>& output_partitions, 
    int num_partitions, int num_gru_run_domain, int num_timesteps,  int simulation_timesteps_remaining) {

    int start_gru_counter = 1;
    int num_gru_per_partition = std::round(num_gru_run_domain / num_partitions);
    for (int i = 0; i < num_partitions - 1; i++) {
        output_partitions.push_back(std::make_shared<output_partition>());
        output_partitions[i]->start_gru = start_gru_counter;
        output_partitions[i]->num_gru = num_gru_per_partition;
        output_partitions[i]->num_active_gru = num_gru_per_partition;
        output_partitions[i]->num_timesteps = num_timesteps;
        output_partitions[i]->simulation_timesteps_remaining = simulation_timesteps_remaining;
        output_partitions[i]->grus_ready_to_write = 0;
        for (int a = 0; a < num_gru_per_partition; a++) {
            output_partitions[i]->hru_info_and_data.push_back(std::make_shared<hru_output_info>());
        }

        start_gru_counter += num_gru_per_partition;
    }
    // The last partition may not easily divide with the number of GRUs
    output_partitions.push_back(std::make_shared<output_partition>());
    output_partitions[num_partitions - 1]->start_gru = start_gru_counter;
    output_partitions[num_partitions - 1]->num_gru = num_gru_run_domain - start_gru_counter + 1;
    output_partitions[num_partitions - 1]->num_active_gru = num_gru_run_domain - start_gru_counter + 1;
    output_partitions[num_partitions - 1]->num_timesteps = num_timesteps;
    output_partitions[num_partitions - 1]->simulation_timesteps_remaining = simulation_timesteps_remaining;
    output_partitions[num_partitions - 1]->grus_ready_to_write = 0;
    for (int a = 0; a < num_gru_run_domain - start_gru_counter + 1; a++) {
        output_partitions[num_partitions - 1]->hru_info_and_data.push_back(std::make_shared<hru_output_info>());
    }

}

std::optional<int> addReadyToWriteHRU(std::vector<std::shared_ptr<output_partition>>& output_partitions, 
    caf::actor hru_actor, int gru_index, int hru_index) {
    
    int partition_index = findPatritionIndex(output_partitions[0]->num_gru, gru_index, output_partitions.size());
    int gru_index_in_partition = gru_index - output_partitions[partition_index]->start_gru;

    output_partitions[partition_index]->hru_info_and_data[gru_index_in_partition]->hru_actor = hru_actor;
    output_partitions[partition_index]->hru_info_and_data[gru_index_in_partition]->index_hru = hru_index;
    output_partitions[partition_index]->hru_info_and_data[gru_index_in_partition]->index_gru = gru_index;
    output_partitions[partition_index]->hru_info_and_data[gru_index_in_partition]->ready_to_write = true;
    output_partitions[partition_index]->grus_ready_to_write += 1;
    // If all grus are ready to write then return the partition index
    if (output_partitions[partition_index]->grus_ready_to_write == output_partitions[partition_index]->num_active_gru) {
        return partition_index;
    }
    else {
        return {};
    }

}

int findPatritionIndex(int grus_per_partition, int gru_index, int num_partitions) {
    int partition_index;

    partition_index = (gru_index - 1) / grus_per_partition;
    // The last partion will not be the same size as the others in some cases
    // So we have to correct the value
    if (partition_index >= num_partitions) {
        partition_index = num_partitions - 1;
    }

    return partition_index;
}

void resetReadyToWrite(std::shared_ptr<output_partition>& output_partition) {
    for (auto &hru_info_and_data : output_partition->hru_info_and_data) {
        hru_info_and_data->ready_to_write = false;
    }
    output_partition->grus_ready_to_write = 0;
}


void updateSimulationTimestepsRemaining(std::shared_ptr<output_partition>& output_partition) {
    output_partition->simulation_timesteps_remaining -= output_partition->num_timesteps;
}

void updateNumTimeForPartition(std::shared_ptr<output_partition> &output_partition) {
    if (output_partition->simulation_timesteps_remaining < output_partition->num_timesteps) {
        output_partition->num_timesteps = output_partition->simulation_timesteps_remaining;
    }
}

std::optional<int> updatePartitionWithFailedHRU(std::vector<std::shared_ptr<output_partition>>& output_partitions, 
    int local_gru_index) {
    int partition_index = findPatritionIndex(output_partitions[0]->num_gru, local_gru_index, output_partitions.size());
    output_partitions[partition_index]->num_active_gru -= 1;    

    // Check if the partition is now ready to write
    if (output_partitions[partition_index]->grus_ready_to_write == output_partitions[partition_index]->num_active_gru) {
        return partition_index;
    }
    else {
        return {};
    }


}

