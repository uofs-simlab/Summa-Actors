#include "summa_server.hpp"


using namespace caf;

behavior SummaServerActor::make_behavior() {
  auto res = self_->system().middleman().publish(
      self_, settings_.distributed_settings_.port_);
  if (!res) {
    self_->println("SummaServerActor: Failed to publish actor on port {}",
                   settings_.distributed_settings_.port_);
    return {};
  }
  self_->println("SummaServerActor Started on port {}", 
                 settings_.distributed_settings_.port_);

  batch_container_ = std::make_unique<BatchContainer>(
      1, settings_.distributed_settings_.total_hru_count_,
      settings_.distributed_settings_.num_hru_per_batch_);
  self_->println("SummaServerActor: Starting with {} Batches", 
                 batch_container_->getBatchesRemaining());

  // Spawn local node actor
  auto local_client = self_->spawn(actor_from_state<SummaClientActor>, "", 
                                   settings_, self_);

  return {
    [this](connect_atom, const std::string& hostname) {
      auto client = actor_cast<actor>(self_->current_sender());
      self_->println("Actor trying to connect with hostname {}", hostname);
      
      self_->monitor(client, [this, client](const error& err) {
        self_->println("Lost Connection With A Connected Actor");

        connected_clients_.erase(std::remove(connected_clients_.begin(), 
            connected_clients_.end(), client), connected_clients_.end());
        self_->println("SummaServerActor: {} Clients Remaining", 
                       connected_clients_.size());
      });

      connected_clients_.push_back(client);

      std::optional<Batch> batch = batch_container_->getUnsolvedBatch();
      if (batch.has_value()) {
        self_->mail(batch.value()).send(client);
      } else {
        self_->println("No batches left to assign"
                      "- Waiting for All Clients to finish");
      }
    },

    [=](done_batch, Batch& batch) {
      auto client = actor_cast<actor>(self_->current_sender());
      self_->println("SummaServerActor: Received Completed Batch From Client");

      batch_container_->updateBatchStats(batch.getBatchID(), batch.getRunTime(), 
          batch.getReadTime(), batch.getWriteTime(), 10, 0);
      self_->println("SummaServerActor: Batch Stats: {}", batch.toString());

      if(!batch_container_->hasUnsolvedBatches()) {
        self_->println("SummaServerActor: All Batches Completed");
        self_->quit();
        return;
      }
      std::optional<Batch> new_batch = batch_container_->getUnsolvedBatch();
      if (new_batch.has_value()) {
        self_->mail(new_batch.value()).send(client);
      } else {
        self_->println("No batches left to assign"
                      "- Waiting for All Clients to finish");
      }
    },

  };
}

// behavior summa_server(stateful_actor<summa_server_state>* self,
//                       Distributed_Settings distributed_settings, 
//                       Summa_Actor_Settings summa_actor_settings, 
//                       File_Access_Actor_Settings file_access_actor_settings,
//                       Job_Actor_Settings job_actor_settings, 
//                       HRU_Actor_Settings hru_actor_settings) {
  
//   aout(self) << "Summa-Server-Actor Started\n";
  
//   self->set_down_handler([=](const down_msg& dm) {
//     aout(self) << "\n\n ********** DOWN HANDLER ********** \n"
//                << "Lost Connection With A Connected Actor\n";
//     std::optional<Client> client = 
//         self->state.client_container.getClient(dm.source);
//     if (client.has_value())
//       resolveLostClient(self, client.value());
//     else
//       resolveLostBackupServer(self, dm);
//   });

//   self->state.distributed_settings = distributed_settings;
//   self->state.summa_actor_settings = summa_actor_settings; 
//   self->state.file_access_actor_settings = file_access_actor_settings;
//   self->state.job_actor_settings = job_actor_settings;
//   self->state.hru_actor_settings = hru_actor_settings;

//   self->state.client_container = Client_Container();
//   // TODO: Batch Container should have start gru passed to it
//   self->state.batch_container = Batch_Container(
//           1,
//           self->state.distributed_settings.total_hru_count,
//           self->state.distributed_settings.num_hru_per_batch);


//   // Publish the server actor
//   auto is_published = self->system().middleman().publish(self, 
//                             self->state.distributed_settings.port);
//   if (!is_published) {
//     aout(self) << "Failed to publish actor\n";
//     self->quit();
//     return {};
//   }
//   self->state.current_server_actor = self;
//   aout(self) << "Server is Running \n";
  
//   return {
//     [=] (is_lead_server, caf::actor client_actor) {
//       self->send(client_actor, is_lead_server_v, true, self);
//     },

//     // A message from a client requesting to connect
//     [=](connect_to_server, actor client_actor, std::string hostname) {
//       aout(self) << "\nActor trying to connect with hostname " 
//                  << hostname << "\n";
      
//       // Check if the simulation has started (first-actor connected)
//       if (!self->state.started_simulation) {
//         self->state.started_simulation = true;
//         self->state.start_time = std::chrono::system_clock::now();
//       }

                 
//       // Check if the client is already connected
//       std::optional<Client> client = 
//           self->state.client_container.getClient(client_actor.address());

//       if (client.has_value()) {
//         aout(self) << "Client is already connected\n";
//         aout(self) << "Updating " << hostname << " with current backup servers\n";
//         self->send(client.value().getActor(), update_backup_server_list_v, 
//                    self->state.backup_servers_list);
//         std::optional<Batch> batch = client.value().getBatch();
//         if (batch.has_value())
//             return;
//       } else {
//         self->state.client_container.addClient(client_actor, hostname);
//         self->monitor(client_actor);
//         // Tell client they are connected
//         self->send(client_actor, connect_to_server_v, 
//                    self->state.summa_actor_settings, 
//                    self->state.file_access_actor_settings, 
//                    self->state.job_actor_settings, 
//                    self->state.hru_actor_settings,
//                    self->state.backup_servers_list);
            
//         std::optional<Batch> batch = self->state.batch_container.getUnsolvedBatch();
//         if (batch.has_value()) {
//           self->state.client_container.setBatchForClient(client_actor, batch);
//           aout(self) << "SENDING: " << batch.value().toString() << "\n";
//           self->send(client_actor, batch.value());
//           for (auto& backup_server : self->state.backup_servers_list) {
//               caf::actor backup_server_actor = std::get<0>(backup_server);
//               self->send(backup_server_actor, new_client_v, client_actor, hostname);
//               self->send(backup_server_actor, new_assigned_batch_v, client_actor, batch.value());
//           }
//         } else {
//           aout(self) << "No batches left to assign - Waiting for All Clients to finish\n";
//           // Let Backup Servers know that a new client has connected
//           for (auto& backup_server : self->state.backup_servers_list) {
//               caf::actor backup_server_actor = std::get<0>(backup_server);
//               self->send(backup_server_actor, new_client_v, client_actor, 
//                          hostname);
//           }
//         }
//       } 
//     },

//     [=](connect_as_backup, actor backup_server, std::string hostname) {
//       aout(self) << "\nReceived Connection Request From a backup server " << hostname <<  "\n";
//       self->monitor(backup_server);
//       // Check if the backup server is already connected
//       auto backup_server_iterator = find(self->state.backup_servers_list.begin(), self->state.backup_servers_list.end(), std::make_tuple(backup_server, hostname));

//       if (backup_server_iterator != self->state.backup_servers_list.end()) {
//         aout(self) << "Backup Server is already connected\n";
//       } else {
//         aout(self) << "Adding Backup Server to list\n";
//         self->state.backup_servers_list.push_back(std::make_tuple(backup_server, hostname));
//       }

//       self->send(backup_server, connect_as_backup_v); // confirm connection with sender
//       // Now we need to send the backup actor our current state
//       self->send(backup_server, update_with_current_state_v, self->state.batch_container, self->state.client_container);
//       sendAllBackupServersList(self);
//     }, 

//     [=](done_batch, actor client_actor, Batch& batch) {
//       aout(self) << "\nReceived Completed Batch From Client\n";
//       aout(self) << batch.toString() << "\n\n";\
//       Client client = self->state.client_container.getClient(client_actor.address()).value();

//       self->state.batch_container.updateBatch_success(batch, self->state.csv_file_path, client.getHostname());
//       printRemainingBatches(self);

//       std::optional<Batch> new_batch = self->state.batch_container.getUnsolvedBatch();
        
//       if (new_batch.has_value()) {
//         // send clients new batch and update backup servers
//         self->state.client_container.setBatchForClient(client_actor, new_batch);
//         self->send(client_actor, new_batch.value());
//         for (auto& backup_server : self->state.backup_servers_list) {
//           caf::actor backup_server_actor = std::get<0>(backup_server);
//           self->send(backup_server_actor, done_batch_v, client_actor, batch);
//           self->send(backup_server_actor, new_assigned_batch_v, client_actor, new_batch.value());
//         }
//       } else {
//         // We may be done
//         if (!self->state.batch_container.hasUnsolvedBatches()) {
//           // We are done
//           self->become(summa_server_exit(self));
//           return;    
//         }

//         // No Batches left to assign but waiting for all clients to finish
//         aout(self) << "No batches left to assign - Waiting for All Clients to finish\n";
//         self->state.client_container.setBatchForClient(client_actor, {});
//         for (auto& backup_server : self->state.backup_servers_list) {
//           caf::actor backup_server_actor = std::get<0>(backup_server);
//           self->send(backup_server_actor, done_batch_v, client_actor, batch);
//           self->send(backup_server_actor, no_more_batches_v, client_actor);
//         }
//       }
//     }, 
//   };
// }

// behavior summa_server_exit(stateful_actor<summa_server_state>* self) {
//   aout(self) << "SUMMA Simulation is complete\n";
//   aout(self) << "Telling Clients to Exit\n";
//   while(!self->state.client_container.isEmpty()) {
//       Client client = self->state.client_container.removeClient_fromBack();
//       caf::actor client_actor = client.getActor();
//       self->send(client_actor, time_to_exit_v);
//   }
//   aout(self) << "Telling Backup Servers to Exit\n";
//   for (auto& backup_server : self->state.backup_servers_list) {
//       caf::actor backup_server_actor = std::get<0>(backup_server);
//       self->send(backup_server_actor, time_to_exit_v);
//   }

//   // Print timing
//   self->state.end_time = std::chrono::system_clock::now();
//   std::chrono::duration<double> elapsed_seconds = 
//       self->state.end_time - self->state.start_time;

//   aout(self) << "Elapsed Time: " << elapsed_seconds.count() << "s\n";
//   self->quit();
//   return {};
// }


// void initializeCSVOutput(std::string csv_output_path) {
//   std::ofstream csv_output;
//   csv_output.open(csv_output_path, std::ios_base::out);
//   csv_output << 
//       "Batch_ID,"  <<
//       "Start_HRU," <<
//       "Num_HRU,"   << 
//       "Hostname,"  <<
//       "Run_Time,"  <<
//       "Read_Time," <<
//       "Write_Time\n";
//   csv_output.close();
// }

// void printRemainingBatches(stateful_actor<summa_server_state>* self) {
//   aout(self) << "******************\n Batches Remaining: " 
//              << self->state.batch_container.getBatchesRemaining()
//              << "\n******************\n\n";
// }

// void sendAllBackupServersList(stateful_actor<summa_server_state>* self) {
//   std::vector<Client> clients = self->state.client_container.getClientList();
//   for (Client client : clients) {
//     self->send(client.getActor(), update_backup_server_list_v, 
//                self->state.backup_servers_list);
//   }
  
//   for(std::tuple<actor, std::string> backup_server : self->state.backup_servers_list) {
//     self->send(std::get<0>(backup_server), update_backup_server_list_v, 
//                self->state.backup_servers_list);
//   }
// }

// void findAndRemoveLostBackupServer(stateful_actor<summa_server_state>* self, actor_addr lost_backup_server) {
//   for (int i = 0; i < self->state.backup_servers_list.size(); i++) {
//     if (std::get<0>(self->state.backup_servers_list[i]) == lost_backup_server) {
//       aout(self) << "Removed backup server with hostname: "
//                  << std::get<1>(self->state.backup_servers_list[i]) << "\n";
//       self->state.backup_servers_list.erase(
//           self->state.backup_servers_list.begin() + i);
//       break;
//     }
//   }
// }

// void checkForIdleClients(stateful_actor<summa_server_state>* self) {
//   aout(self) << "Looking for an idle Client\n";
//   std::optional<Client> client = self->state.client_container.getIdleClient();
//   if (client.has_value()) {
//     aout(self) << "Found an idle Client\n";
//     std::optional<Batch> new_batch = self->state.batch_container.getUnsolvedBatch();
//     if (new_batch.has_value()) {
//       // send clients new batch and update backup servers
//       aout(self) << "Found a batch to assign\n";
//       self->state.client_container.setBatchForClient(client.value().getActor(), 
//                                                      new_batch);
//       self->send(client.value().getActor(), new_batch.value());
//       for (auto& backup_server : self->state.backup_servers_list) {
//         caf::actor backup_server_actor = std::get<0>(backup_server);
//         self->send(backup_server_actor, new_assigned_batch_v, 
//                    client.value().getActor(), new_batch.value());
//       }
//     }
//   } else {
//       aout(self) << "No idle clients found, batch will be added to the back of the list\n";
//   }
// }

// void notifyBackupServersOfRemovedClient(stateful_actor<summa_server_state>* self, Client client) {
//   for (auto& backup_server : self->state.backup_servers_list) {
//     caf::actor backup_server_actor = std::get<0>(backup_server);
//     self->send(backup_server_actor, client_removed_v, client);
//   }
// }

// void resolveLostClient(stateful_actor<summa_server_state>* self, Client client) {
//   aout(self) << "Lost Client: " << client.getHostname() << "\n";
//   std::optional<Batch> batch = client.getBatch();
//   self->state.batch_container.setBatchUnassigned(batch.value());
//   self->state.client_container.removeClient(client);
//   notifyBackupServersOfRemovedClient(self, client);
//   checkForIdleClients(self);
// }

// void resolveLostBackupServer(stateful_actor<summa_server_state>* self, const down_msg& dm) {
//   aout(self) << "Lost Backup Server\n";
//   findAndRemoveLostBackupServer(self, dm.source);
//   sendAllBackupServersList(self);
// }
 
