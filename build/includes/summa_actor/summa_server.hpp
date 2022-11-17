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
    strong_actor_ptr current_server; // if server is a backup then this will be set to the lead server
    actor current_server_actor;

    std::string hostname;


    std::string csv_output_name = "/batch_results.csv";
    
    Client_Container client_container;
    Batch_Container batch_container;
    
    // Actor Reference, Hostname
    std::vector<std::tuple<caf::actor, std::string>> backup_servers_list;

    // Settings Structures
    Distributed_Settings distributed_settings;
    Summa_Actor_Settings summa_actor_settings;
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings;
    HRU_Actor_Settings hru_actor_settings;

};

// Summa Server setup behaviour - initializes the state for the server
behavior summa_server_init(stateful_actor<summa_server_state>* self, Distributed_Settings distributed_settings, 
    Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings);

// Summa Server behaviour - handles messages from clients
behavior summa_server(stateful_actor<summa_server_state>* self);

// Summa Server backup behaviour - handles the exit messages for clients
behavior summa_server_exit(stateful_actor<summa_server_state>* self);

// Creates the csv file that holds the results of the batches
void initializeCSVOutput(std::string csv_output_path, std::string csv_output_name);

// Send all connected actors the updated backup servers list
void sendAllBackupServersList(stateful_actor<summa_server_state>* self);

// Look for the lost backup server in the backup servers list and remove it
void findAndRemoveLostBackupServer(stateful_actor<summa_server_state>* self, actor_addr lost_backup_server);

// Check for an idle client to send the failed or next batch we find that is not assigned
void checkForIdleClients(stateful_actor<summa_server_state>* self);

void notifyBackupServersOfRemovedClient(stateful_actor<summa_server_state>* self, Client client);

// Convience function to keep code clean - just does what you think it does
void printRemainingBatches(stateful_actor<summa_server_state>* self);

} // namespace caf
