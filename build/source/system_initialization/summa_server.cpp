#include "summa_server.hpp"

#define NOBATCH 2

using namespace caf;

behavior SummaServerActor::make_behavior() {
  start_ = std::chrono::system_clock::now();
  auto err = createLogger();
  if (err == FAILURE) return {};

  err = publishServer();
  if (err == FAILURE) return {};

  err = createBatchContainers(settings_.simulations_file_);
  if (err == FAILURE) return {};

  // Spawn local node actor
  auto local_client = self_->spawn(actor_from_state<SummaClientActor>, 
                                   "", settings_, self_);

  return {
    // Initial Connection from Client
    [this](connect_atom, const std::string& hostname) {
      auto client_actor = actor_cast<actor>(self_->current_sender());
      self_->println("Actor trying to connect with hostname {}", hostname);

      // Check if the client is already connected
      auto client = std::make_unique<Client>(client_actor, hostname);
      auto result = connected_clients_.insert(std::move(client));
      if (!result.second) {
        self_->println("SummaServerActor: Client Already Connected");
        logger_->log("SummaServerActor: Client Already Connected");
        logger_->log("\t" + result.first->get()->toString());
        logger_->log("\t" + std::to_string(connected_clients_.size()) + 
                     " Clients Connected\n"); 
      } else {
        self_->println("SummaServerActor: Client Connected");
        logger_->log("SummaServerActor: Client Connected");
        logger_->log("\t" + result.first->get()->toString());
        logger_->log("\t" + std::to_string(connected_clients_.size()) + 
                     " Clients Connected\n");  
      }
      
      // Monitor the client actor for connection loss
      self_->monitor(client_actor, [this, client_actor](const error& err) {
        active_clients_--;
        if (!simulation_finished_) {
          self_->println("SummaServerActor: Actor Connection Lost");
          logger_->log("SummaServerActor: Actor Connection Lost");
          auto client_it = std::find_if(connected_clients_.begin(), 
              connected_clients_.end(), 
              [&client_actor](const std::unique_ptr<Client>& client) {
                return client->getActor() == client_actor;
          });
          if (client_it != connected_clients_.end()) {
            auto extracted_client = connected_clients_.extract(client_it);
            if (extracted_client) {
              logger_->log("SummaServerActor: " + 
                  to_string(extracted_client.value()->getActor().address())
                  + " Disconnected");
              logger_->log("\t" + std::to_string(connected_clients_.size()) + 
                  " Clients Connected\n");
              auto client_batch = extracted_client.value()->getBatch();
              if (client_batch.has_value()) {
                for (auto& sim : simulations_) {
                  if (client_batch.value().getName() == sim->getName()) {
                    sim->setBatchUnassigned(client_batch.value());
                    batch_logger_->logBatch("r", client_batch.value(), 
                        "disconnected");
                    break;
                  }
                }
              }
            }
          }
        }
      });

      auto res = assignBatch(client_actor);
      if (res == FAILURE) {
        self_->println("SummaServerActor: Error Assigning Batch to Client");
        logger_->log("SummaServerActor: Error Assigning Batch to Client");
      }
    },

    [=](done_batch, Batch& batch) {
      active_clients_--;
      auto client_actor = actor_cast<actor>(self_->current_sender());
      logger_->log("SummaServerActor: Received Completed Batch From " + 
          to_string(client_actor.address()));
      logger_->log("\t" + std::to_string(active_clients_) + " Active Clients");

      // Find the simulation that the batch belongs to and update the stats
      for (auto& sim : simulations_) {
        if (batch.getName() == sim->getName()) {
          sim->updateBatchStats(batch.getBatchID(), batch.getRunTime(), 
              batch.getReadTime(), batch.getWriteTime(), 10, 0);
          logger_->log("SummaServerActor: Batch Stats: " + batch.toString());
          logger_->log("SummaServerActor: " + sim->getName() + " Simulation\n"
              "\tBatches Remaining: " + 
              std::to_string(sim->getBatchesRemaining()));

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

      simulations_.push_back(std::make_unique<BatchContainer>(name, 
          file_manager, start_gru, num_gru, batch_size, settings_));
      self_->println(simulations_.back()->toString());
    } 

  } catch (json::exception& e) {
    self_->println("SummaServerActor: Error reading simulations file: {}", 
        e.what());
    return FAILURE;
  }
  
  logger_->log("SummaServerActor: Batch Containers Created");
  logger_->log("\tTotal Simulations: " + std::to_string(simulations_.size()));
  for (auto& simulation : simulations_) {
    logger_->log(simulation->toString());
    logger_->log(simulation->getBatchesAsString());
  } 
  return SUCCESS; 
}

int SummaServerActor::assignBatch(caf::actor client_actor) {
  auto res = NOBATCH;
  for (auto& sim : simulations_) {
    std::optional<Batch> batch = sim->getUnsolvedBatch();
    if (batch.has_value()) {
      auto client_it = std::find_if(connected_clients_.begin(), 
          connected_clients_.end(), 
          [&client_actor](const std::unique_ptr<Client>& client) {
            return client->getActor() == client_actor;
      });
      if (client_it != connected_clients_.end()) {
        client_it->get()->setBatch(batch.value());
        logger_->log("SummaServerActor: Assigning Batch to " + 
            to_string(client_actor.address()));
        logger_->log("\t" + batch.value().toString());
      } else {
        logger_->log("SummaServerActor: Error Assigning Batch to " + 
            to_string(client_actor.address()));
        res = FAILURE;
        break;
      }
      self_->mail(batch.value()).send(client_actor);
      batch_logger_->logBatch("s", batch.value(), "assigned");
      res = SUCCESS;
      break;
    }
  }

  switch (res) {
    case SUCCESS:
      active_clients_++;
      break;
    case NOBATCH:
      self_->println("SummaServerActor: No Batches Left to Assign");
      if (active_clients_ == 0) {
        simulation_finished_ = true;
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
          self_->mail(time_to_exit_v).send(c->getActor());
        }
        exit(EXIT_SUCCESS);
      }
      self_->println("SummaServerActor: Clients Still Active: {}", 
                     active_clients_);
      logger_->log("SummaServerActor: Clients Still Active: " + 
                   std::to_string(active_clients_));
      break;
    case FAILURE:
      self_->println("SummaServerActor: Error Assigning Batch to Client");
      break;
  }

  return res;
}