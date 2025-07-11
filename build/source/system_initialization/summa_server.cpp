#include "summa_server.hpp"

namespace caf {

behavior SummaServer::make_behavior() {
  if (backup_) {
    return summa_backup_server_init();
  } else {
    return summa_server();
  }
}

behavior SummaServer::summa_server() {
  backup_ = false;

  self_->println("Summa-Server-Actor Started\n");
  

  client_container_ = Client_Container();
  // TODO: Batch Container should have start gru passed to it
  batch_container_ = BatchContainer(
          1,
          settings_.distributed_settings_.total_hru_count_,
          settings_.distributed_settings_.num_hru_per_batch_, "");


  // Publish the server actor
  auto is_published = self_->system().middleman().publish(self_, 
                            settings_.distributed_settings_.port_);
  if (!is_published) {
    self_->println("Failed to publish actor\n");
    self_->quit();
    return {};
  }
  current_server_actor_ = self_;
  self_->println("Server is Running \n");
  
  
  return {
    [=] (is_lead_server, caf::actor client_actor) {
      self_->mail(is_lead_server_v, true, self_).send(client_actor);
    },

    // A message from a client requesting to connect
    [=](connect_to_server, actor client_actor, std::string hostname) {
      self_->println("\nActor trying to connect with hostname ", 
                 hostname, "\n");
      
      // Check if the simulation has started (first-actor connected)
      if (!started_simulation_) {
        started_simulation_ = true;
        start_time_ = std::chrono::system_clock::now();
      }

                 
      // Check if the client is already connected
      std::optional<Client> client = 
          client_container_.getClient(client_actor.address());

      if (client.has_value()) {
        self_->println("Client is already connected\n");
        self_->println("Updating ", hostname, " with current backup servers\n");
        self_->mail(update_backup_server_list_v, 
                   backup_servers_list_).send(client.value().getActor());
        std::optional<Batch> batch = client.value().getBatch();
        if (batch.has_value())
            return;
      } else {
        client_container_.addClient(client_actor, hostname);
        self_->monitor(client_actor, [this, source=client_actor.address()](const error& err) {
          self_->println("\n\n ********** DOWN HANDLER ********** \n",
               "Lost Connection With A Connected Actor\n");
          std::optional<Client> client = 
          client_container_.getClient(source);
          if (client.has_value())
            resolveLostClient(client.value());
          else
            resolveLostBackupServer(source);
        });
        // Tell client they are connected
        self_->mail(connect_to_server_v, settings_).send(client_actor);
            
        std::optional<Batch> batch = batch_container_.getUnsolvedBatch();
        if (batch.has_value()) {
          client_container_.setBatchForClient(client_actor, batch);
          self_->println("SENDING: ", batch.value().toString(), "\n");
          self_->mail(batch.value()).send(client_actor);
          for (auto& backup_server : backup_servers_list_) {
              caf::actor backup_server_actor = std::get<0>(backup_server);
              self_->mail(new_client_v, client_actor, hostname).send(backup_server_actor);
              self_->mail(new_assigned_batch_v, client_actor, batch.value()).send(backup_server_actor);
          }
        } else {
          self_->println("No batches left to assign - Waiting for All Clients to finish\n");
          // Let Backup Servers know that a new client has connected
          for (auto& backup_server : backup_servers_list_) {
              caf::actor backup_server_actor = std::get<0>(backup_server);
              self_->mail(new_client_v, client_actor, 
                         hostname).send(backup_server_actor);
          }
        }
      } 
    },

    [=](connect_as_backup, actor backup_server, std::string hostname) {
      self_->println("\nReceived Connection Request From a backup server ", hostname,  "\n");
      self_->monitor(backup_server, [this, source=backup_server.address()](const error& err) {
          self_->println("\n\n ********** DOWN HANDLER ********** \n",
               "Lost Connection With A Connected Actor\n");
          std::optional<Client> client = 
          client_container_.getClient(source);
          if (client.has_value())
            resolveLostClient(client.value());
          else
            resolveLostBackupServer(source);
        });
      // Check if the backup server is already connected
      auto backup_server_iterator = find(backup_servers_list_.begin(), backup_servers_list_.end(), std::make_tuple(backup_server, hostname));

      if (backup_server_iterator != backup_servers_list_.end()) {
        self_->println("Backup Server is already connected\n");
      } else {
        self_->println("Adding Backup Server to list\n");
        backup_servers_list_.push_back(std::make_tuple(backup_server, hostname));
      }

      self_->mail(connect_as_backup_v).send(backup_server); // confirm connection with sender
      // Now we need to send the backup actor our current state
      self_->mail(update_with_current_state_v, batch_container_, client_container_).send(backup_server);
      sendAllBackupServersList();
    }, 

    [=](done_batch, actor client_actor, Batch& batch) {
      self_->println("\nReceived Completed Batch From Client\n");
      self_->println(batch.toString(), "\n\n");\
      Client client = client_container_.getClient(client_actor.address()).value();

      batch_container_.updateBatch_success(batch, csv_file_path_, client.getHostname());
      printRemainingBatches();

      std::optional<Batch> new_batch = batch_container_.getUnsolvedBatch();
        
      if (new_batch.has_value()) {
        // send clients new batch and update backup servers
        client_container_.setBatchForClient(client_actor, new_batch);
        self_->mail(new_batch.value()).send(client_actor);
        for (auto& backup_server : backup_servers_list_) {
          caf::actor backup_server_actor = std::get<0>(backup_server);
          self_->mail(done_batch_v, client_actor, batch).send(backup_server_actor);
          self_->mail(new_assigned_batch_v, client_actor, new_batch.value()).send(backup_server_actor);
        }
      } else {
        // We may be done
        if (!batch_container_.hasUnsolvedBatches()) {
          // We are done
          self_->become(summa_server_exit());
          return;    
        }

        // No Batches left to assign but waiting for all clients to finish
        self_->println("No batches left to assign - Waiting for All Clients to finish\n");
        client_container_.setBatchForClient(client_actor, {});
        for (auto& backup_server : backup_servers_list_) {
          caf::actor backup_server_actor = std::get<0>(backup_server);
          self_->mail(done_batch_v, client_actor, batch).send(backup_server_actor);
          self_->mail(no_more_batches_v, client_actor).send(backup_server_actor);
        }
      }
    }, 
  };
}

behavior SummaServer::summa_server_exit() {
  self_->println("SUMMA Simulation is complete\n");
  self_->println("Telling Clients to Exit\n");
  while(!client_container_.isEmpty()) {
      Client client = client_container_.removeClient_fromBack();
      caf::actor client_actor = client.getActor();
      self_->mail(time_to_exit_v).send(client_actor);
  }
  self_->println("Telling Backup Servers to Exit\n");
  for (auto& backup_server : backup_servers_list_) {
      caf::actor backup_server_actor = std::get<0>(backup_server);
      self_->mail(time_to_exit_v).send(backup_server_actor);
  }

  // Print timing
  end_time_ = std::chrono::system_clock::now();
  std::chrono::duration<double> elapsed_seconds = 
      end_time_ - start_time_;

  self_->println("Elapsed Time: ", elapsed_seconds.count(), "s\n");
  self_->quit();
  return {};
}


void initializeCSVOutput(std::string csv_output_path) {
  std::ofstream csv_output;
  csv_output.open(csv_output_path, std::ios_base::out);
  csv_output << 
      "Batch_ID,"  <<
      "Start_HRU," <<
      "Num_HRU,"   << 
      "Hostname,"  <<
      "Run_Time,"  <<
      "Read_Time," <<
      "Write_Time\n";
  csv_output.close();
}

void SummaServer::printRemainingBatches() {
  self_->println("******************\n Batches Remaining: ", 
             batch_container_.getBatchesRemaining(),
             "\n******************\n\n");
}

void SummaServer::sendAllBackupServersList() {
  std::vector<Client> clients = client_container_.getClientList();
  for (Client client : clients) {
    self_->mail(client.getActor(), update_backup_server_list_v, 
               backup_servers_list_).send(client.getActor());
  }
  
  for(std::tuple<actor, std::string> backup_server : backup_servers_list_) {
    self_->mail(update_backup_server_list_v, 
               backup_servers_list_).send(std::get<0>(backup_server));
  }
}

void SummaServer::findAndRemoveLostBackupServer(actor_addr lost_backup_server) {
  for (int i = 0; i < backup_servers_list_.size(); i++) {
    if (std::get<0>(backup_servers_list_[i]) == lost_backup_server) {
      self_->println("Removed backup server with hostname: ",
                 std::get<1>(backup_servers_list_[i]), "\n");
      backup_servers_list_.erase(
        backup_servers_list_.begin() + i);
      break;
    }
  }
}

void SummaServer::checkForIdleClients() {
  self_->println("Looking for an idle Client\n");
  std::optional<Client> client = client_container_.getIdleClient();
  if (client.has_value()) {
    self_->println("Found an idle Client\n");
    std::optional<Batch> new_batch = batch_container_.getUnsolvedBatch();
    if (new_batch.has_value()) {
      // send clients new batch and update backup servers
      self_->println("Found a batch to assign\n");
      client_container_.setBatchForClient(client.value().getActor(), new_batch);
      self_->mail(new_batch.value()).send(client.value().getActor());
      for (auto& backup_server : backup_servers_list_) {
        caf::actor backup_server_actor = std::get<0>(backup_server);
        self_->mail(new_assigned_batch_v, client.value().getActor(), new_batch.value()).send(backup_server_actor);
      }
    }
  } else {
      self_->println("No idle clients found, batch will be added to the back of the list\n");
  }
}

void SummaServer::notifyBackupServersOfRemovedClient(Client client) {
  for (auto& backup_server : backup_servers_list_) {
    caf::actor backup_server_actor = std::get<0>(backup_server);
    self_->mail(client_removed_v, client).send(backup_server_actor);
  }
}

void SummaServer::resolveLostClient(Client client) {
  self_->println("Lost Client: ", client.getHostname(), "\n");
  std::optional<Batch> batch = client.getBatch();
  batch_container_.setBatchUnassigned(batch.value());
  client_container_.removeClient(client);
  notifyBackupServersOfRemovedClient(client);
  checkForIdleClients();
}

void SummaServer::resolveLostBackupServer(actor_addr source) {
  self_->println("Lost Backup Server\n");
  findAndRemoveLostBackupServer(source);
  sendAllBackupServersList();
}
 
} // end namespace
