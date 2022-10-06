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

void run_client(actor_system& system, const config& cfg, Distributed_Settings distributed_settings) {
    scoped_actor self{system};

    aout(self) << "Starting SUMMA-Client in Distributed Mode\n";
    
    if (distributed_settings.hostname == "" || distributed_settings.port == -1) {
       aout(self) << "ERROR: run_client() host and port - CHECK SETTINGS FILE\n";
       return;
    }
    std::optional<std::string> path = cfg.config_file;
    auto c = system.spawn(summa_client, path);
    if (!distributed_settings.hostname.empty() && distributed_settings.port > 0) {
        anon_send(c, connect_atom_v, distributed_settings.hostname , (uint16_t) distributed_settings.port );
    } else {
        aout(self) << "No Server Config" << std::endl;
    }
   
}

void run_server(actor_system& system, const config& cfg, Distributed_Settings distributed_settings, 
    Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings) {
    scoped_actor self{system};
    int err;

    if (distributed_settings.port == -1) {
        aout(self) << "ERROR: run_server() port - CHECK SETTINGS FILE\n";
        return;
    }
    auto server = system.spawn(summa_server, distributed_settings,
                        summa_actor_settings, 
                        file_access_actor_settings, 
                        job_actor_settings, 
                        hru_actor_settings);
                        
    aout(self) << "Attempting to publish summa_server_actor on port " << distributed_settings.port << std::endl;
    auto is_port = io::publish(server, distributed_settings.port);
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
        if (cfg.server_mode) {
            run_server(sys, cfg, distributed_settings, summa_actor_settings, 
                file_access_actor_settings, job_actor_settings, hru_actor_settings);
        } else {
            run_client(sys, cfg, distributed_settings);
        }

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
