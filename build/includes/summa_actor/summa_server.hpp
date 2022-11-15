#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "batch/batch.hpp"
#include "batch/batch_container.hpp"
#include "client/client.hpp"
#include "client/client_container.hpp"
#include "settings_functions.hpp"
#include "global.hpp"
#include "message_atoms.hpp"
#include <string>
#include <optional>
#include <thread>
#include <chrono>
#include <iostream>


namespace caf {

struct summa_server_state {
    actor backup_server = nullptr;
    actor backup_server2 = nullptr;
    strong_actor_ptr current_server; // if server is a backup then this will be set to the lead server
    actor current_server_actor;
    
    std::string csv_output_name = "/batch_results.csv";
    
    Client_Container *client_container;
    Batch_Container *batch_container;
    std::vector<caf::actor> backup_servers_list;

    // Settings Structures
    Distributed_Settings distributed_settings;
    Summa_Actor_Settings summa_actor_settings;
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings;
    HRU_Actor_Settings hru_actor_settings;

};


behavior summa_server_init(stateful_actor<summa_server_state>* self, Distributed_Settings distributed_settings, 
    Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings);

behavior summa_server(stateful_actor<summa_server_state>* self);

int assembleBatches(stateful_actor<summa_server_state>* self);

void initializeCSVOutput(std::string csv_output_path, std::string csv_output_name);

void printRemainingBatches(stateful_actor<summa_server_state>* self);
}