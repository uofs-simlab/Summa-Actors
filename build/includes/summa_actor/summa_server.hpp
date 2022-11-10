#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "batch_manager.hpp"
#include "settings_functions.hpp"
#include "client.hpp"
#include <string>
#include <optional>


namespace caf {

struct summa_server_state {
    actor backup_server = nullptr;
    actor backup_server2 = nullptr;
    strong_actor_ptr current_server; // if server is a backup then this will be set to the lead server
    int num_clients;
    int batches_remaining = 0;
    int batches_solved = 0;
    
    std::string config_path;
    std::vector<Batch> batch_list;
    std::vector<Batch> solved_batches;
    std::vector<Batch> failed_batches;
    std::vector<Client> client_list;
    std::string csv_output_name = "/batch_results.csv";
    
    Client_Container *client_container;
    Batch_Container *batch_container;

    caf::actor health_check_reminder_actor;

    Distributed_Settings distributed_settings;
    Summa_Actor_Settings summa_actor_settings;
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings;
    HRU_Actor_Settings hru_actor_settings;

};

behavior summa_server(stateful_actor<summa_server_state>* self, Distributed_Settings distributed_settings, 
    Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings);

void sendClientsHeartbeat(stateful_actor<summa_server_state>* self);

behavior client_health_check_reminder(event_based_actor* self);

int assembleBatches(stateful_actor<summa_server_state>* self);

std::optional<int> getUnsolvedBatchID(stateful_actor<summa_server_state>* self);

void initializeCSVOutput(std::string csv_output_path, std::string csv_output_name);

void connecting(stateful_actor<summa_server_state>*, const std::string& host, uint16_t port);
}