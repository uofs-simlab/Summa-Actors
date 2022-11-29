#include "output_container.hpp"



void initArrayOfOuputPartitions(std::vector<output_partition>& output_partitions, int num_partitions, int num_gru_run_domain, int num_timesteps) {
    output_partitions.resize(num_partitions);
    int start_gru_counter = 1;
    int num_gru_per_partition = std::round(num_gru_run_domain / num_partitions);
    for (int i = 0; i < num_partitions - 1; i++) {
        output_partitions[i].start_gru = start_gru_counter;
        output_partitions[i].num_gru = num_gru_per_partition;
        output_partitions[i].num_timesteps = num_timesteps;
        output_partitions[i].hru_info_and_data.resize(num_gru_per_partition);
        start_gru_counter += num_gru_per_partition;
    }
    // The last partion may not easily divide with the number of GRUs
    output_partitions[num_partitions - 1].start_gru = start_gru_counter;
    output_partitions[num_partitions - 1].num_gru = num_gru_run_domain - start_gru_counter + 1;
    output_partitions[num_partitions - 1].num_timesteps = num_timesteps;
    output_partitions[num_partitions - 1].hru_info_and_data.resize(num_gru_run_domain - start_gru_counter + 1);
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

