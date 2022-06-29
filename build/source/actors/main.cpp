#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "summa_actor.hpp"
#include "summa_client.hpp"
#include "summa_server.hpp"
#include "global.hpp"
#include "message_atoms.hpp"
#include <string>
#include <bits/stdc++.h>
#include <unistd.h>
#include <iostream>
#include "json.hpp"
#include "batch_manager.hpp"

using namespace caf;


/* Configuration class that handles the config and 
/  command line options for the actors program */
class config : public actor_system_config {
    public:
        int startGRU = -1;
        int countGRU = -1;
        std::string config_path = "";
        bool debugMode = false;
        bool server_mode = false;
    
    config() {
        opt_group{custom_options_, "global"}
            .add(startGRU, "gru,g", "Starting GRU Index")
            .add(countGRU, "numGRU,n", "Total Number of GRUs")
            .add(config_path, "config,c", "Path name of the config directory")
            .add(debugMode, "debug-mode,b", "enable debug mode")
            .add(server_mode, "server-mode,s", "enable server mode");
    }
};

void run_client(actor_system& system, const config& cfg) {
    scoped_actor self{system};
    std::string host;
    uint16_t port;

    aout(self) << "Starting SUMMA-Client in Distributed Mode\n";
    host = getSettings(cfg.config_path, "DistributedSettings", "host", host).value_or("");
    port = getSettings(cfg.config_path, "DistributedSettings", "port", port).value_or(-1);
    
    if (host == "" || port == -1) {
       aout(self) << "ERROR: run_client() host and port - CHECK SETTINGS FILE\n";
       return;
    }

    auto c = system.spawn(summa_client);
    if (!host.empty() && port > 0) {
        anon_send(c, connect_atom_v, host, port);
    } else {
        aout(self) << "No Server Config" << std::endl;
    }
   
}

void run_server(actor_system& system, const config& cfg) {
    scoped_actor self{system};
    uint16_t port;

    port = getSettings(cfg.config_path, "DistributedSettings", "port", port).value_or(-1);
    if (port == -1) {
        aout(self) << "ERROR: run_server() port - CHECK SETTINGS FILE\n";
        return;
    }
    auto server = system.spawn(summa_server, cfg.config_path);
    aout(self) << "Attempting to publish summa_server_actor on port " << port << std::endl;
    auto is_port = io::publish(server, port);
    if (!is_port) {
        std::cerr << "********PUBLISH FAILED*******" << to_string(is_port.error()) << "\n";
        return;
    }
    aout(self) << "Successfully Published summa_server_actor on port " << *is_port << "\n";
    std::string dummy;
    std::getline(std::cin, dummy);
    std::cout << "...cya" << std::endl;
    anon_send_exit(server, exit_reason::user_shutdown);
}




void caf_main(actor_system& sys, const config& cfg) {
    scoped_actor self{sys};
    std::string key_1 = "DistributedSettings";
    std::string key_2 = "distributed-mode";
    bool distributed_mode = false;

    distributed_mode = getSettings(cfg.config_path, key_1, key_2, distributed_mode).value_or(false);
    if (distributed_mode) {
        // only command line arguments needed are config_path and server-mode
        auto system = cfg.server_mode ? run_server : run_client;
        system(sys, cfg);

    } else {
        // Configure command line arguments
        if (cfg.startGRU == -1) {
            aout(self) << "Starting GRU was not defined!! " << 
                "startGRU is set with the \"-g\" option\n";
            aout(self) << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
            return;
        }
        if (cfg.countGRU == -1) {
            aout(self) << "Number of GRUs was not defined!! " <<
                "countGRU is set with the \"-n\" option\n";
            aout(self) << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
            return;
        }
        if (cfg.config_path == "") {
            aout(self) << "File Manager was not defined!! " << 
                "fileManger is set with the \"-c\" option\n";
            aout(self) << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
            return;
        }

        auto summa = sys.spawn(summa_actor, cfg.startGRU, cfg.countGRU, cfg.config_path, self);
    }
    
}

CAF_MAIN(id_block::summa, io::middleman)
