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


class SummaServerActor {
  private:
    caf::event_based_actor* self_;

    caf::strong_actor_ptr current_server;
    caf::actor current_server_actor;
    
    std::string hostname;

    // Output CSV file
    std::string csv_file_path;
    std::string csv_output_name = "/batch_results.csv";

    // Containers
    std::unordered_map<caf::actor, Client> connected_clients_;
    int active_clients_ = 0;
        
    std::vector<BatchContainer> simulations_;
    
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
    int createBatchContainers(std::string simulations_config);
    int assignBatch(caf::actor client_actor);
};