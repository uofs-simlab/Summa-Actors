#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "summa_actor.hpp"
#include "summa_client.hpp"
#include "summa_server.hpp"
#include "summa_backup_server.hpp"
#include "global.hpp"
#include "settings_functions.hpp"
#include "message_atoms.hpp"
#include "client/client.hpp"
#include <string>
#include <bits/stdc++.h>
#include <unistd.h>
#include <sys/stat.h>
#include <iostream>
#include "json.hpp"
#include <memory>
#include <optional>

using namespace caf;

const std::string command_line_help = "Summa-Actors is in active development and some features may not be available.\n"
    "Usage: summa_actors -m master_file [-g startGRU countGRU] [-c config_file] [-b backup_server] [-s server_mode]\n"
    "Available options: \n"
    "\t-m, --master:         Define path/name of master file (can be specified in config)\n"
    "\t-g, --gru:            Run a subset of countGRU GRUs starting from index startGRU \n"
    "\t-c, --config:         Path name of the config directory \n"
    "\t-b, --backup-server:  Start backup server, requires a server and config_file \n"
    "\t-s, --server-mode:    Enable server mode \n"
    "\t-h, --help:           Print this help message \n"
    "\nUnimplemented Options: \n"
    "\t-n --newFile          Define frequency [noNewFiles,newFileEveryOct1] of new output files\n"
    "\t-s --suffix           Add fileSuffix to the output files\n"
    "\t-h --hru              Run a single HRU with index of iHRU\n"
    "\t-r --restart          Define frequency [y,m,d,e,never] to write restart files\n"
    "\t-p --progress         Define frequency [m,d,h,never] to print progress\n"
    "\t-v --version          Display version information of the current build\n";


/* Configuration class that handles the config and 
/  command line options for the actors program */
class config : public actor_system_config {
    public:
        int startGRU = -1;
        int countGRU = -1;
        std::string config_file = "";
        bool backup_server = false;
        bool server_mode = false;
        std::string master_file = "";
        bool help = false;
    
    config() {
        opt_group{custom_options_, "global"}
            .add(master_file,   "master,m", "Path/name of master file")
            .add(startGRU,      "gru,g", "Starting GRU Index")
            .add(countGRU,      "countGRU,t", "Number of GRUs to run in subset")
            .add(config_file,   "config,c", "Path name of the config directory")
            .add(backup_server, "backup-server,b", "flag to denote if the server starting is a backup server")
            .add(server_mode,   "server-mode,s", "enable server mode")
            .add(help,          "help,h", "Print this help message");
    }
};

void publish_server(caf::actor actor_to_publish, int port_number) {
    std::cout << "Attempting to publish summa_server_actor on port " << port_number << std::endl;
    auto is_port = io::publish(actor_to_publish, port_number);
    if (!is_port) {
        std::cerr << "********PUBLISH FAILED*******" << to_string(is_port.error()) << "\n";
        return;
    }
    std::cout << "Successfully Published summa_server_actor on port " << *is_port << "\n";
}

void connect_client(caf::actor client_to_connect, std::string host_to_connect_to, int port_number) {
    if (!host_to_connect_to.empty() && port_number > 0) {
        uint16_t port = 4444;
        anon_send(client_to_connect, connect_atom_v, host_to_connect_to, (uint16_t) port );

    } else {
        std::cerr << "No Server Config" << std::endl;
    }
}

void run_client(actor_system& system, const config& cfg, Distributed_Settings distributed_settings) {
    scoped_actor self{system};

    aout(self) << "Starting SUMMA-Client in Distributed Mode\n";
    
    auto client = system.spawn(summa_client_init);
    for (auto host : distributed_settings.servers_list) {
        connect_client(client, host, distributed_settings.port);
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

    // Check if we have are the backup server
    if (cfg.backup_server) {          
        auto server = system.spawn(summa_backup_server_init,
                                   distributed_settings,
                                   summa_actor_settings,
                                   file_access_actor_settings,
                                   job_actor_settings,
                                   hru_actor_settings);

        publish_server(server, distributed_settings.port);
        connect_client(server, distributed_settings.servers_list[0], distributed_settings.port);

    } else {     
        auto server = system.spawn(summa_server_init, 
                                   distributed_settings,
                                   summa_actor_settings, 
                                   file_access_actor_settings, 
                                   job_actor_settings, 
                                   hru_actor_settings);  
                 
        publish_server(server, distributed_settings.port);
    }

}


void caf_main(actor_system& sys, const config& cfg) {
    scoped_actor self{sys};
    int err;

    aout(self) << "Starting SUMMA-Actors\n";
    aout(self) << "Master File = " << cfg.master_file << "\n";
    aout(self) << "Config File = " << cfg.config_file << "\n";
    aout(self) << "Starting GRU = " << cfg.startGRU << "\n";
    aout(self) << "Count GRU = " << cfg.countGRU << "\n";




    return;
    struct stat file_to_check;
    // Check if config file exists
    if (stat(cfg.config_file.c_str(), &file_to_check) != 0) {
        aout(self) << "Config File Path Does Not Exist\n"
                   << "EXAMPLE: ./summa_actors -g 1 -n 10 -c location/of/config \n";
        return;
    }

    Distributed_Settings distributed_settings = readDistributedSettings(cfg.config_file);
    Summa_Actor_Settings summa_actor_settings = readSummaActorSettings(cfg.config_file);
    File_Access_Actor_Settings file_access_actor_settings = readFileAccessActorSettings(cfg.config_file);
    Job_Actor_Settings job_actor_settings = readJobActorSettings(cfg.config_file);
    HRU_Actor_Settings hru_actor_settings = readHRUActorSettings(cfg.config_file);
    
    aout(self) << "Printing Settings For SUMMA Simulation\n";
    check_settings_from_json(distributed_settings,
                             summa_actor_settings, 
                             file_access_actor_settings, 
                             job_actor_settings,
                             hru_actor_settings);

    if (distributed_settings.distributed_mode) {
        // only command line arguments needed are config_file and server-mode
        if (cfg.server_mode) {
            run_server(sys, 
                       cfg, 
                       distributed_settings, 
                       summa_actor_settings, 
                       file_access_actor_settings, 
                       job_actor_settings, 
                       hru_actor_settings);
        } else {
            run_client(sys, 
                       cfg, 
                       distributed_settings);
        }

    } else {
        // Configure command line arguments
        if (cfg.startGRU == -1) {
            aout(self) << "Starting GRU was not defined!! " 
                       << "startGRU is set with the \"-g\" option\n"
                       << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
            return;
        }
        if (cfg.countGRU == -1) {
            aout(self) << "Number of GRUs was not defined!! "
                       << "countGRU is set with the \"-n\" option\n"
                       << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
            return;
        }
        if (cfg.config_file == "") {
            aout(self) << "File Manager was not defined!! "
                       << "fileManger is set with the \"-c\" option\n"
                       << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
            return;
        }

        auto summa = sys.spawn(summa_actor, 
                               cfg.startGRU, 
                               cfg.countGRU, 
                               summa_actor_settings, 
                               file_access_actor_settings, 
                               job_actor_settings, 
                               hru_actor_settings, 
                               self);
    }
    
}

int main(int argc, char** argv) {
    // Parse command line arguments

    // Look through the arguments for the -g flag, and prepend the option after the number with --countGRU
    // for 

    std::vector<std::string> args(argv, argv + argc);
    for (auto it = args.begin(); it != args.end(); ++it) {
        if (*it == "-g" && std::next(it) != args.end()) {
            auto count_gru = std::find_if(std::next(it), args.end(), [](const std::string& arg) {
                return std::isdigit(arg.front());
            });
            if (count_gru != args.end()) {
                args.insert(std::next(count_gru), "-t");
            }
            break;
        }
    }
    for (const auto& arg : args) {
        std::cout << arg << " ";
    }
    std::cout << std::endl;

    char** argv2 = new char*[args.size()];
    for (int i = 0; i < args.size(); ++i) {
        argv2[i] = new char[args[i].size() + 1];
        strcpy(argv2[i], args[i].c_str());
    }

    argc = args.size();
    exec_main_init_meta_objects<id_block::summa, io::middleman>();
    caf::core::init_global_meta_objects(); 
    return exec_main<>(caf_main, argc, argv2);

}
