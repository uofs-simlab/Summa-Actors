#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "batch.hpp"
#include "batch_container.hpp"
#include "client.hpp"
#include "client_container.hpp"
#include "settings_functions.hpp"
#include "message_atoms.hpp"
#include <string>
#include <optional>
#include <thread>
#include <chrono>
#include <iostream>


namespace caf {

class SummaServer {
  caf::event_based_actor* self_;

  // Server reference -- set if this is a backup server
  strong_actor_ptr current_server_;
  actor current_server_actor_;
  bool backup_;

  
  // Our hostnme
  std::string hostname_;

  // Output CSV file
  std::string csv_file_path_;
  std::string csv_output_name_ = "/batch_results.csv";

  // Containers
  Client_Container client_container_;
  DynamicBatchContainer batch_container_;
  // Actor Reference, Hostname
  std::vector<std::tuple<caf::actor, std::string>> backup_servers_list_;

  // Settings Structures
  Settings settings_;

  // Timing vars
  using chrono_time = std::chrono::time_point<std::chrono::system_clock>;
  chrono_time start_time_;
  chrono_time end_time_;
  bool started_simulation_ = false;

  public:
    SummaServer(caf::event_based_actor* self, Settings settings, bool backup)
    : self_(self), settings_(settings), backup_(backup) {};
  
    caf::behavior make_behavior();
    caf::behavior summa_server();
    caf::behavior summa_server_exit();

    void sendAllBackupServersList();
    void findAndRemoveLostBackupServer(actor_addr lost_backup_server);
    void checkForIdleClients();
    void notifyBackupServersOfRemovedClient(Client client);
    void resolveLostClient(Client client);
    void resolveLostBackupServer(actor_addr dm);
    void printRemainingBatches();

    caf::behavior summa_backup_server_init();
    caf::behavior summa_backup_server();

    void connecting_backup(const std::string& host, uint16_t port);


};

// // Summa Server behaviour - handles messages from clients
// behavior summa_server(stateful_actor<summa_server_state>* self,
//                       DistributedSettings distributed_settings, 
//                       SummaActorSettings summa_actor_settings, 
//                       FileAccessActorSettings file_access_actor_settings,
//                       JobActorSettings job_actor_settings, 
//                       HRUActorSettings hru_actor_settings);

// // Summa Server backup behaviour - handles the exit messages for clients
// behavior summa_server_exit(stateful_actor<summa_server_state>* self);

// Creates the csv file that holds the results of the batches
void initializeCSVOutput(std::string csv_output_path);

// Send all connected actors the updated backup servers list
// void sendAllBackupServersList(stateful_actor<summa_server_state>* self);

// Look for the lost backup server in the backup servers list and remove it
// void findAndRemoveLostBackupServer(stateful_actor<summa_server_state>* self, 
                                  //  actor_addr lost_backup_server);

// Check for an idle client to send the failed or next batch we find that is not assigned
// void checkForIdleClients(stateful_actor<summa_server_state>* self);

// void notifyBackupServersOfRemovedClient(stateful_actor<summa_server_state>* self, Client client);

// Finds the batch the lost client was working on and reassigns it to another client if available
// If no client is available then the batch is added back to the list to be reassigned later
// void resolveLostClient(stateful_actor<summa_server_state>* self, Client client);

// Removes the backup server from the list of backup servers
// All connected actors are then notified of the change
// void resolveLostBackupServer(stateful_actor<summa_server_state>* self, const down_msg& dm);

// Convience function to keep code clean - just does what you think it does
// void printRemainingBatches(stateful_actor<summa_server_state>* self);


} // namespace caf
