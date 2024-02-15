#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "batch.hpp"
#include "batch_container.hpp"
#include "client.hpp"
#include "client_container.hpp"
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
  // Server reference -- set if this is a backup server
  strong_actor_ptr current_server;
  actor current_server_actor;
  
  // Our hostnme
  std::string hostname;

  // Output CSV file
  std::string csv_file_path;
  std::string csv_output_name = "/batch_results.csv";

  // Containers
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

  // Timing vars
  using chrono_time = std::chrono::time_point<std::chrono::system_clock>;
  chrono_time start_time;
  chrono_time end_time;
  bool started_simulation = false;

};

// Summa Server behaviour - handles messages from clients
behavior summa_server(stateful_actor<summa_server_state>* self,
                      Distributed_Settings distributed_settings, 
                      Summa_Actor_Settings summa_actor_settings, 
                      File_Access_Actor_Settings file_access_actor_settings,
                      Job_Actor_Settings job_actor_settings, 
                      HRU_Actor_Settings hru_actor_settings);

// Summa Server backup behaviour - handles the exit messages for clients
behavior summa_server_exit(stateful_actor<summa_server_state>* self);

// Creates the csv file that holds the results of the batches
void initializeCSVOutput(std::string csv_output_path);

// Send all connected actors the updated backup servers list
void sendAllBackupServersList(stateful_actor<summa_server_state>* self);

// Look for the lost backup server in the backup servers list and remove it
void findAndRemoveLostBackupServer(stateful_actor<summa_server_state>* self, 
                                   actor_addr lost_backup_server);

// Check for an idle client to send the failed or next batch we find that is not assigned
void checkForIdleClients(stateful_actor<summa_server_state>* self);

void notifyBackupServersOfRemovedClient(stateful_actor<summa_server_state>* self, Client client);

// Finds the batch the lost client was working on and reassigns it to another client if available
// If no client is available then the batch is added back to the list to be reassigned later
void resolveLostClient(stateful_actor<summa_server_state>* self, Client client);

// Removes the backup server from the list of backup servers
// All connected actors are then notified of the change
void resolveLostBackupServer(stateful_actor<summa_server_state>* self, const down_msg& dm);

// Convience function to keep code clean - just does what you think it does
void printRemainingBatches(stateful_actor<summa_server_state>* self);


} // namespace caf
