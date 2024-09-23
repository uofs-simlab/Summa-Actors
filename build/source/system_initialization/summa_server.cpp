#include "summa_server.hpp"

#define NOBATCH 2

using namespace caf;

behavior SummaServerActor::make_behavior() {
  start_ = std::chrono::system_clock::now();

  return {
    // Initalize server from main
    [this](init) {
      auto err = createLogger();
      if (err == FAILURE) return;

      err = publishServer();
      if (err == FAILURE) return;

      err = createBatchContainers(settings_.simulations_file_);
      if (err == FAILURE) return;

      // Spawn local node actor
      auto local_client = self_->spawn(
          actor_from_state<SummaClientActor>, "", settings_, self_);
    },
    
    // Reinitialize the server (original server disconnected)
    [this](reinit, std::unordered_map<caf::actor, Client> connected_clients, 
           std::vector<BatchContainer> simulations) {
      
      // Set up the Server
      auto err = publishServer();
      if (err == FAILURE) return;
      
      err = recreateLogger();
      if (err == FAILURE) return;

      // Establish the server's state
      connected_clients_ = connected_clients;
      simulations_ = std::move(simulations);

      // Remove old local client based on hostname
      for (auto& c : connected_clients_) {
        if (c.second.getHostname() == "local") {
          // Check if the client has a batch assigned
          auto client_batch = c.second.getBatch();
          if (client_batch.has_value()) {
            for (auto& sim : simulations_) {
              if (client_batch.value().getName() == sim.getName()) {
                sim.setBatchUnassigned(client_batch.value());
                batch_logger_->logBatch("r", client_batch.value(), 
                    "disconnect");
                break;
              }
            }
          }
          connected_clients_.erase(c.first);
        }
      }

      // Set the client with our hostname to local
      gethostname(hostname_, HOST_NAME_MAX);
      for (auto& c : connected_clients_) {
        if (c.second.getHostname() == hostname_) {
          c.second.setLocal();
        }
      }
    },

    // Initial Connection from Client
    [this](connect_atom, const std::string& hostname) {
      auto client_actor = actor_cast<actor>(self_->current_sender());
      self_->println("Actor trying to connect with hostname {}", hostname);

      // Check if the client is already connected
      auto insert_result = connected_clients_.emplace(client_actor, 
          Client(client_actor, hostname));

      auto client_it = insert_result.first;
      if (!insert_result.second) {
        self_->println("SummaServerActor: Client Already Connected");
        logger_->log("SummaServerActor: Client Already Connected");
        logger_->log("\t" + client_it->second.toString());
        logger_->log("\t" + std::to_string(connected_clients_.size()) + 
                     " Clients Connected\n"); 
      } else {
        self_->println("SummaServerActor: Client Connected");
        logger_->log("SummaServerActor: Client Connected");
        logger_->log("\t" + client_it->second.toString());
        logger_->log("\t" + std::to_string(connected_clients_.size()) + 
                     " Clients Connected\n");  
      }
      
      // Monitor the client actor for connection loss
      self_->monitor(client_actor, [this, client_actor](const error& err) {
        if (simulation_finished_) return;
        // Report the client disconnection        
        self_->println("SummaServerActor: Actor Connection Lost");
        logger_->log("SummaServerActor: Actor Connection Lost");

        // Find the client in the connected clients map
        auto client_handle = connected_clients_.extract(client_actor); 
        if (!client_handle) {
          self_->println("SummaServerActor: Error Extracting Client"
                        " \tClient Not Found in Connected Clients");
          return;
        }

        // Find batch the client had so it can be reassigned
        auto client = client_handle.mapped();
        logger_->log("SummaServerActor: " + client.toString() + 
            " Disconnected");
        logger_->log("\t" + std::to_string(connected_clients_.size()) + 
            " Clients Connected\n");
        auto client_batch = client.getBatch();
        if (client_batch.has_value()) {
          for (auto& sim : simulations_) {
            if (client_batch.value().getName() == sim.getName()) {
              sim.setBatchUnassigned(client_batch.value());
              batch_logger_->logBatch("r", client_batch.value(), "disconnect");
              break;
            }
          }
        }
        
        // Send the connected clients list to all clients
        for (auto& c : connected_clients_) {
          self_->mail(simulations_).send(c.second.getActor());
          self_->mail(connected_clients_).send(c.second.getActor());
        }
      }); // End Monitor

      auto res = assignBatch(client_actor);
      if (res == FAILURE) {
        self_->println("SummaServerActor: Error Assigning Batch to Client");
        logger_->log("SummaServerActor: Error Assigning Batch to Client");
      }
    },

    [=](reconnect, const std::string& hostname) {
      auto client_actor = actor_cast<actor>(self_->current_sender());
      self_->println("Actor trying to reconnect with hostname {}", hostname);

      // Check if the client is already connected
      auto insert_result = connected_clients_.emplace(client_actor, 
          Client(client_actor, hostname));

      auto client_it = insert_result.first;
      if (!insert_result.second) {
        self_->println("SummaServerActor: Client Already Connected");
        logger_->log("SummaServerActor: Client Already Connected");
        logger_->log("\t" + client_it->second.toString());
        logger_->log("\t" + std::to_string(connected_clients_.size()) + 
                     " Clients Connected\n"); 
      } else {
        self_->println("SummaServerActor: Client Connected");
        logger_->log("SummaServerActor: Client Connected");
        logger_->log("\t" + client_it->second.toString());
        logger_->log("\t" + std::to_string(connected_clients_.size()) + 
                     " Clients Connected\n");  
      }
      
      // Monitor the client actor for connection loss
      self_->monitor(client_actor, [this, client_actor](const error& err) {
        if (simulation_finished_) return;
        // Report the client disconnection        
        self_->println("SummaServerActor: Actor Connection Lost");
        logger_->log("SummaServerActor: Actor Connection Lost");

        // Find the client in the connected clients map
        auto client_handle = connected_clients_.extract(client_actor); 
        if (!client_handle) {
          self_->println("SummaServerActor: Error Extracting Client"
                        " \tClient Not Found in Connected Clients");
          return;
        }
      }); // End Monitor

      // Send the state to the client
      self_->mail(simulations_).send(client_actor);
      self_->mail(connected_clients_).send(client_actor);
    },

    [=](done_batch, Batch& batch) {
      auto client_actor = actor_cast<actor>(self_->current_sender());
      logger_->log("SummaServerActor: Received Completed Batch From " + 
          to_string(client_actor.address()));

      // Find the simulation that the batch belongs to and update the stats
      for (auto& sim : simulations_) {
        if (batch.getName() == sim.getName()) {
          sim.updateBatchStats(batch.getBatchID(), batch.getRunTime(), 
              batch.getReadTime(), batch.getWriteTime(), 10, 0);
          logger_->log("SummaServerActor: Batch Stats: " + batch.toString());
          logger_->log("SummaServerActor: " + sim.getName() + " Simulation\n"
              "\tBatches Remaining: " + 
              std::to_string(sim.getBatchesRemaining()));

          batch_logger_->logBatch("r", batch, "completed");
          break;
        }
      }

      // Find the client a new batch
      auto res = assignBatch(client_actor);
      if (res == FAILURE) {
        self_->println("SummaServerActor: Error Assigning Batch to Client");
        logger_->log("SummaServerActor: Error Assigning Batch to Client");
      }
    },
  };
}

int SummaServerActor::createLogger() {
  if (!settings_.enable_logging_) {
    self_->println("SummaServerActor: Logging Disabled");
    return SUCCESS;
  }
  
  // Create the log directory
  self_->println("SummaServerActor: Logging Enabled");
  bool err = createDirectory(settings_.log_dir_);
  if (!err) {
    self_->println("SummaServerActor: Error Creating Log Directory");
    return FAILURE;
  }

  // Create summa_server.txt log file
  std::string log_file = settings_.log_dir_ + "summa_server";
  logger_ = std::make_unique<Logger>(log_file);
  logger_->log("SummaServerActor: Logger Created");

  // Create batch_logger.csv file
  std::string batch_log_file = settings_.log_dir_ + "batch_logger.csv";
  batch_logger_ = std::make_unique<BatchLogger>(batch_log_file);
  logger_->log("SummaServerActor: BatchLogger Created\n"
               "\tBatch Log File: " + batch_log_file);

  return SUCCESS;
}

int SummaServerActor::recreateLogger() {
  if (!settings_.enable_logging_) {
    self_->println("SummaServerActor: Logging Disabled");
    return SUCCESS;
  }

  // recreate summa_server.txt log file
  std::string log_file = settings_.log_dir_ + "summa_server";
  logger_ = std::make_unique<Logger>(log_file, false);
  logger_->log("SummaServerActor: Logger Recreated");

  // recreate batch_logger.csv file
  std::string batch_log_file = settings_.log_dir_ + "batch_logger.csv";
  batch_logger_ = std::make_unique<BatchLogger>(batch_log_file, false);
  logger_->log("SummaServerActor: BatchLogger Recreated\n"
               "\tBatch Log File: " + batch_log_file);
  
  return SUCCESS;
}

int SummaServerActor::publishServer() {
  // Publish the server actor to the network
  auto res = self_->system().middleman().publish(
      self_, settings_.distributed_settings_.port_);
  if (!res) {
    self_->println("SummaServerActor: Failed to publish actor on port {}",
                   settings_.distributed_settings_.port_);
    return FAILURE;
  }
  self_->println("SummaServerActor Started on port {}", 
                 settings_.distributed_settings_.port_);
  return SUCCESS;
}

int SummaServerActor::createBatchContainers(std::string simulations_config) {
  using json = nlohmann::json;
  self_->println("SummaServerActor: Creating Batch Containers From {}",
      simulations_config);

  std::string name, file_manager;
  int start_gru, num_gru, batch_size;

  // Open Json File and Create Json Object
  std::ifstream sim_file(simulations_config);
  json json_obj;
  if (!sim_file.is_open()) {
    self_->println("SummaServerActor: Error opening simulations file");
    return FAILURE;
  }
  sim_file >> json_obj;
  sim_file.close();

  // Parse Json File
  try {
    // Check if the Simulations_Configurations key exists
    if (json_obj.find("Simulations_Configurations") == json_obj.end()) {
      self_->println("SummaServerActor: Error reading simulations file");
      return FAILURE;
    }

    json simulations = json_obj["Simulations_Configurations"];
    for (auto& simulation : simulations) {
      name = simulation["name"];
      file_manager = simulation["file_manager"];
      start_gru = simulation["start_gru"];
      num_gru = simulation["num_gru"];
      batch_size = simulation["batch_size"];

      simulations_.push_back(BatchContainer(name, file_manager, start_gru, 
          num_gru, batch_size, settings_));
      self_->println(simulations_.back().toString());
    } 

  } catch (json::exception& e) {
    self_->println("SummaServerActor: Error reading simulations file: {}", 
        e.what());
    return FAILURE;
  }
  
  logger_->log("SummaServerActor: Batch Containers Created");
  logger_->log("\tTotal Simulations: " + std::to_string(simulations_.size()));
  for (auto& simulation : simulations_) {
    logger_->log(simulation.toString());
    logger_->log(simulation.getBatchesAsString());
  } 
  return SUCCESS; 
}

int SummaServerActor::assignBatch(caf::actor client_actor) {
  auto res = NOBATCH;
  for (auto& sim : simulations_) {
    std::optional<Batch> batch = sim.getUnsolvedBatch();
    if (batch.has_value()) {
      auto client_it = connected_clients_.find(client_actor);
      if (client_it == connected_clients_.end()) {
        logger_->log("SummaServerActor: Error Assigning Batch to " + 
            to_string(client_actor.address()));
        res = FAILURE;
        break;
      }
      
      auto& client = client_it->second;
      client.setBatch(batch.value());
      self_->mail(batch.value()).send(client_actor);

      logger_->log("SummaServerActor: Assigning Batch to " + 
          to_string(client_actor.address()));
      logger_->log("\t" + batch.value().toString());
      batch_logger_->logBatch("s", batch.value(), "assigned");
      res = SUCCESS;
      break;
    }
  }


  if (res == NOBATCH) {
    bool simulation_finished_ = true;
    self_->println("SummaServerActor: No Batches Left to Assign");

    for (auto& sim : simulations_) {
      if (sim.getBatchesRemaining() > 0) {
        simulation_finished_ = false;
        break;
      }
    }

    if (simulation_finished_) {
      self_->println("SummaServerActor: Simulation Finished");
      logger_->log("SummaServerActor: Simulation Finished");
      end_ = std::chrono::system_clock::now();
      auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(
          end_ - start_);
      self_->println("SummaServerActor: Simulation Time: {} seconds", 
                     elapsed.count());
      logger_->log("SummaServerActor: Simulation Time: " +
                  std::to_string(elapsed.count()) + " seconds");
      for (auto& c : connected_clients_) {
        self_->mail(time_to_exit_v).send(c.second.getActor());
      }
      exit(EXIT_SUCCESS);
    }

    // Print batches remaining
    for (auto& sim : simulations_) {
      self_->println("SummaServerActor: Simulation: {} Batches Remaining: {}", 
                     sim.getName(), sim.getBatchesRemaining());
      logger_->log("SummaServerActor: Simulation: " + sim.getName() + 
                   " Batches Remaining: " + 
                   std::to_string(sim.getBatchesRemaining()));
    }
  } else if (res == FAILURE) {
    self_->println("SummaServerActor: Error Assigning Batch to Client");
  }

  // Update the all connected clients with the updated batches
  for (auto& c : connected_clients_) {
    self_->mail(simulations_).send(c.second.getActor());
    self_->mail(connected_clients_).send(c.second.getActor());
  }

  return res;
}