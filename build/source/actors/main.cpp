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

using namespace caf;


/* Configuration class that handles the config and 
/  command line options for the actors program */
class config : public actor_system_config {
    public:
        int startGRU = -1;
        int countGRU = -1;
        std::string configPath = "";
        bool debugMode = false;
        uint16_t port = 4444;
        std::string host = "cnic-giws-cpu-19001-02";
        bool server_mode = false;
        bool distributed = false;
    
    config() {
        opt_group{custom_options_, "global"}
            .add(startGRU, "gru,g", "Starting GRU Index")
            .add(countGRU, "numGRU,n", "Total Number of GRUs")
            .add(configPath, "config,c", "Path name of the config directory")
            .add(debugMode, "debug-mode,b", "enable debug mode")
            .add(distributed, "distributed-mode,d", "enable distributed mode")
            .add(port, "port,p", "set port")
            .add(host, "host,h", "set Host (ignored in server mode)")
            .add(server_mode, "server-mode,s", "enable server mode");
    }
};

void run_client(actor_system& system, const config& cfg) {
    scoped_actor self{system};
    if (cfg.distributed) {
        aout(self) << "Starting SUMMA-Client in Distributed Mode\n";
        auto c = system.spawn(summa_client);
        if (!cfg.host.empty() && cfg.port > 0) {
            anon_send(c, connect_atom_v, cfg.host, cfg.port);
        } else {
            aout(self) << "No Server Config" << std::endl;
        }

    } else {
        aout(self) << "Starting SUMMA in non-distributed mode \n"; 
        auto summa = system.spawn(summa_actor, cfg.startGRU, cfg.countGRU, cfg.configPath, self);
    }
   
}


void run_server(actor_system& system, const config& cfg) {
    scoped_actor self{system};
    auto server = system.spawn(summa_server, cfg.configPath);
    aout(self) << "Attempting to publish summa_server_actor" << cfg.port << std::endl;
    auto is_port = io::publish(server, cfg.port);
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
    if (cfg.configPath == "") {
        aout(self) << "File Manager was not defined!! " << 
            "fileManger is set with the \"-c\" option\n";
        aout(self) << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
        return;
    }
    if (cfg.debugMode) {
        aout(self) << "Starting SUMMA-Actors in DebugMode\n";
        bool debug = true;
    }

    // Start the Actors
    if (cfg.distributed) {
        aout(self) << "Starting SUMMA-Actors in Distributed Mode \n";
        auto system = cfg.server_mode ? run_server : run_client;
        system(sys, cfg);
    } else {
        auto summa = sys.spawn(summa_actor, cfg.startGRU, cfg.countGRU, cfg.configPath, self);
    }
    // start SUMMA
    // auto system = cfg.server_mode ? run_server : run_client;
    // system(sys, cfg);
}

CAF_MAIN(id_block::summa, io::middleman)