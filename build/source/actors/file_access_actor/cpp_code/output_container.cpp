#include "output_container.hpp"



void initArrayOfOuputPartitions(std::vector<std::shared_ptr<output_partition>>& output_partitions, 
    int num_partitions, int num_gru_run_domain, int num_timesteps,  int simulation_timesteps_remaining) {

    int start_gru_counter = 1;
    int num_gru_per_partition = std::round(num_gru_run_domain / num_partitions);
    for (int i = 0; i < num_partitions - 1; i++) {
        output_partitions.push_back(std::make_shared<output_partition>());
        output_partitions[i]->start_gru = start_gru_counter;
        output_partitions[i]->num_gru = num_gru_per_partition;
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
    if (output_partitions[partition_index]->grus_ready_to_write == output_partitions[partition_index]->num_gru) {
        return partition_index;
    }
    else {
        return {};
    }

}



// std::optional<int> addHRUOutput(std::vector<std::shared_ptr<output_partition>>& output_partitions, 
//     caf::actor hru_actor, int gru_index, int hru_index, std::shared_ptr<hru_output_handles>& timestep_output) {
    
//     int partition_index = findPatritionIndex(output_partitions[0]->num_gru, gru_index, output_partitions.size());
//     int gru_index_in_partition = gru_index - output_partitions[partition_index]->start_gru;
//     // set some housekeeping data
//     output_partitions[partition_index]->hru_info_and_data[gru_index_in_partition]->hru_actor = hru_actor;
//     output_partitions[partition_index]->hru_info_and_data[gru_index_in_partition]->index_hru = hru_index;
//     output_partitions[partition_index]->hru_info_and_data[gru_index_in_partition]->index_gru = gru_index;
//     output_partitions[partition_index]->hru_info_and_data[gru_index_in_partition]->output_data.push_back(timestep_output);
//     // If all hru_info_and_data are full then return the partition index
//     if (isPartitionFull(output_partitions[partition_index])) {
//         return partition_index;
//     }
//     else {
//         return {};
//     }
// }

int findGRUIndexInPartition(int gru_index, int start_gru) {
    return gru_index - start_gru;
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

// bool isPartitionFull(std::shared_ptr<output_partition> &output_partition) {
//     for (auto &hru_info_and_data : output_partition->hru_info_and_data) {
//         if (hru_info_and_data->output_data.size() != output_partition->num_timesteps) {
//             // double check we are under if not throw an error
//             if (hru_info_and_data->output_data.size() > output_partition->num_timesteps) {
//                 throw "hru_info_and_data.output_data.size() > output_partition.num_timesteps";
//             }
//             return false;
//         }
//     }
//     return true;
// }


// std::vector<std::vector<std::shared_ptr<hru_output_handles>>> getOutputHandlesFromPartition(int partition_index, std::vector<std::shared_ptr<output_partition>>& output_partitions) {
//     std::vector<std::vector<std::shared_ptr<hru_output_handles>>> output_handles;
//     for (auto &hru_info_and_data : output_partitions[partition_index]->hru_info_and_data) {
//         output_handles.push_back(hru_info_and_data->output_data);
//     }
//     return output_handles;
// }


// void clearOutputPartition(std::shared_ptr<output_partition>& output_partition) {
//     for (auto &hru_info_and_data : output_partition->hru_info_and_data) {
//         hru_info_and_data->output_data.clear();
//     }
// }

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






Output_Container::Output_Container(int max_hrus, int max_steps) {
    this->max_hrus = max_hrus;
    this->max_steps = max_steps;
    for (int i = 0; i < max_hrus; i++) {
        std::vector<hru_output_handles> hru_output_handles;
        this->hru_output_handles_vector.push_back(hru_output_handles);
    }
    
}

Output_Container::~Output_Container(){};

void Output_Container::insertOutput(int hru_index, hru_output_handles hru_output) {
    // adjust hru_index to be 0 based
    hru_index = hru_index - 1;
    try {
        if (hru_index < 0 || hru_index >= this->max_hrus)
        throw "HRU index out of bounds";
            
        if (this->hru_output_handles_vector[hru_index].size() < this->max_steps)
            this->hru_output_handles_vector[hru_index].push_back(hru_output);
        else
            throw "HRU output buffer full";

    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
}


bool Output_Container::isFull(int hru_index) {
    // adjust hru_index to be 0 based
    hru_index = hru_index - 1;
    try {
        if (hru_index < 0 || hru_index >= this->max_hrus)
        throw "HRU index out of bounds";
            
        if (this->hru_output_handles_vector[hru_index].size() == this->max_steps)
            return true;
        else
            return false;

    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    return false;
}

std::vector<std::vector<hru_output_handles>> Output_Container::getAllHRUOutput() {
    return this->hru_output_handles_vector;
}


void Output_Container::clearAll() {
    for (int i = 0; i < this->max_hrus; i++) {
        this->hru_output_handles_vector[i].clear();
    }
}

