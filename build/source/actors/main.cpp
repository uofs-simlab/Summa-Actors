#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "summa_actor.hpp"
#include "summa_client.hpp"
#include "summa_server.hpp"
#include "global.hpp"
#include "settings_functions.hpp"
#include "message_atoms.hpp"
#include <string>
#include <bits/stdc++.h>
#include <unistd.h>
#include <iostream>
#include "json.hpp"
#include "batch_manager.hpp"
#include <optional>

using namespace caf;


/* Configuration class that handles the config and 
/  command line options for the actors program */
class config : public actor_system_config {
    public:
        int startGRU = -1;
        int countGRU = -1;
        std::string config_file = "";
        bool debugMode = false;
        bool server_mode = false;
    
    config() {
        opt_group{custom_options_, "global"}
            .add(startGRU, "gru,g", "Starting GRU Index")
            .add(countGRU, "numGRU,n", "Total Number of GRUs")
            .add(config_file, "config,c", "Path name of the config directory")
            .add(debugMode, "debug-mode,b", "enable debug mode")
            .add(server_mode, "server-mode,s", "enable server mode");
    }
};

void run_client(actor_system& system, const config& cfg) {
    scoped_actor self{system};
    std::string host;
    uint16_t port;

    aout(self) << "Starting SUMMA-Client in Distributed Mode\n";
    host = getSettings(cfg.config_file, "DistributedSettings", "host", host).value_or("");
    port = getSettings(cfg.config_file, "DistributedSettings", "port", port).value_or(-1);
    
    if (host == "" || port == -1) {
       aout(self) << "ERROR: run_client() host and port - CHECK SETTINGS FILE\n";
       return;
    }
    std::optional<std::string> path = cfg.config_file;
    auto c = system.spawn(summa_client, path);
    if (!host.empty() && port > 0) {
        anon_send(c, connect_atom_v, host, port);
    } else {
        aout(self) << "No Server Config" << std::endl;
    }
   
}

void run_server(actor_system& system, const config& cfg) {
    scoped_actor self{system};
    uint16_t port;

    port = getSettings(cfg.config_file, "DistributedSettings", "port", port).value_or(-1);
    if (port == -1) {
        aout(self) << "ERROR: run_server() port - CHECK SETTINGS FILE\n";
        return;
    }
    auto server = system.spawn(summa_server, cfg.config_file);
    aout(self) << "Attempting to publish summa_server_actor on port " << port << std::endl;
    auto is_port = io::publish(server, port);
    if (!is_port) {
        std::cerr << "********PUBLISH FAILED*******" << to_string(is_port.error()) << "\n";
        return;
    }
    aout(self) << "Successfully Published summa_server_actor on port " << *is_port << "\n";
}




void caf_main(actor_system& sys, const config& cfg) {
    scoped_actor self{sys};
    int err;
    Distributed_Settings distributed_settings;
    Summa_Actor_Settings summa_actor_settings;
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings;
    HRU_Actor_Settings hru_actor_settings;
    err = read_settings_from_json(cfg.config_file,
                                  distributed_settings, 
                                  summa_actor_settings, 
                                  file_access_actor_settings,
                                  job_actor_settings, 
                                  hru_actor_settings);


    aout(self) << "Printing Settings For SUMMA Simulation\n";
    check_settings_from_json(distributed_settings,
        summa_actor_settings, file_access_actor_settings, job_actor_settings,
        hru_actor_settings);

    if (distributed_settings.distributed_mode) {
        // only command line arguments needed are config_file and server-mode
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
        if (cfg.config_file == "") {
            aout(self) << "File Manager was not defined!! " << 
                "fileManger is set with the \"-c\" option\n";
            aout(self) << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
            return;
        }

        auto summa = sys.spawn(summa_actor, cfg.startGRU, cfg.countGRU, summa_actor_settings, 
            file_access_actor_settings, job_actor_settings, hru_actor_settings, self);
    }
    
}

CAF_MAIN(id_block::summa, io::middleman)
