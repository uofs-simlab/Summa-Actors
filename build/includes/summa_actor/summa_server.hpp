#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "batch_manager.hpp"
#include "client.hpp"
#include <string>
#include <optional>


namespace caf {

struct summa_server_state {
    int total_hru_count;
    int num_clients;
    int num_hru_per_batch;
    int batches_remaining = 0;
    int batches_solved = 0;
    std::string config_path;
    std::vector<Batch> batch_list;
    std::vector<Batch> solved_batches;
    std::vector<Batch> failed_batches;
    std::vector<Client> client_list;
    std::string csv_output_name;
};

behavior summa_server(stateful_actor<summa_server_state>* self, std::string config_path);
int assembleBatches(stateful_actor<summa_server_state>* self);
std::optional<int> getUnsolvedBatchID(stateful_actor<summa_server_state>* self);
}