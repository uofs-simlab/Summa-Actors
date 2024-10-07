#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "batch.hpp"
#include "batch_container.hpp"
#include "client.hpp"
#include "client_container.hpp"
#include "summa_client.hpp"
#include "settings_functions.hpp"
#include "message_atoms.hpp"
#include "json.hpp"
#include <string>
#include <optional>
#include <thread>
#include <chrono>
#include <iostream>
#include <unordered_map>
#include <sys/stat.h>  

struct Simulation_Meta {
  std::string name_;
  std::string file_manager_;
  int start_gru_;
  int num_gru_;
  int batch_size_;

  Simulation_Meta(std::string name, std::string file_manager, int start_gru, 
      int num_gru, int batch_size) : name_(name), file_manager_(file_manager), 
      start_gru_(start_gru), num_gru_(num_gru), batch_size_(batch_size) {};

  std::string toString() {
    return "Simulation: " + name_ + "\n" +
           "\tFile Manager: " + file_manager_ + "\n" +
           "\tStart GRU: " + std::to_string(start_gru_) + "\n" +
           "\tNum GRU: " + std::to_string(num_gru_) + "\n" +
           "\tBatch Size: " + std::to_string(batch_size_) + "\n";
  }

};

class SummaServerActor {
  private:
    caf::event_based_actor* self_;

    caf::strong_actor_ptr current_server;
    caf::actor current_server_actor;
    
    std::string hostname;
    char hostname_[HOST_NAME_MAX];

    // Output CSV file
    std::string csv_file_path;
    std::string csv_output_name = "/batch_results.csv";

    // Containers
    std::unordered_map<caf::actor, Client> connected_clients_;
    std::vector<BatchContainer> simulations_;
    std::unordered_map<std::string, Simulation_Meta> sim_meta_data_;
    
    // Actor Reference, Hostname
    std::vector<std::tuple<caf::actor, std::string>> backup_servers_list;

    // Settings Structures
    Settings settings_;

    std::unique_ptr<Logger> logger_;
    std::unique_ptr<BatchLogger> batch_logger_;

    bool simulation_finished_ = false;

    // Timing vars
    using chrono_time = std::chrono::time_point<std::chrono::system_clock>;
    chrono_time start_;
    chrono_time end_;
    bool started_simulation = false;
  
  public:
    SummaServerActor(caf::event_based_actor* self, Settings settings) :
        self_(self), settings_(settings) {};
    caf::behavior make_behavior();
    
    int createLogger();
    int recreateLogger();
    int publishServer();
    int setSimulationState(const std::string &sim_config);
    int createBatchContainers();
    int assignBatch(caf::actor client_actor);
    int handleDisconnect(caf::actor client_actor);
    void addClient(caf::actor client_actor, const std::string &hostname);
    void removeLocalClient();
    int updateServerSF(); // SF = Server File
    int readSimulationsFile(const std::string &sim_config);
};