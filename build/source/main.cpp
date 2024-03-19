#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "summa_actor.hpp"
#include "summa_client.hpp"
#include "summa_server.hpp"
#include "summa_backup_server.hpp"
#include "job_actor.hpp"
#include "node_actor.hpp"
#include "global.hpp"
#include "settings_functions.hpp"
#include "message_atoms.hpp"
#include "client.hpp"
#include <string>
#include <bits/stdc++.h>
#include <unistd.h>
#include <sys/stat.h>
#include <iostream>
#include "json.hpp"
#include <memory>
#include <optional>
#include <filesystem>

using namespace caf;

const std::string command_line_help = "Summa-Actors is in active development and some features may not be available.\n"
    "Usage: summa_actors -m master_file [-g startGRU countGRU] [-c config_file] [-b backup_server] [-s server_mode]\n"
    "Available options: \n"
    "\t-m, --master:         Define path/name of master file (can be specified in config)\n"
    "\t-g, --gru:            Run a subset of countGRU GRUs starting from index startGRU \n"
    "\t-c, --config:         Path name of the Summa-Actors config file (optional but recommended)\n"
    "\t    --gen-config:     Generate a config file \n"
    "\t    --host:           Hostname of the server \n"
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
        std::string master_file = "";
        std::string config_file = "";
        std::string host = "";
        bool generate_config = false;
        bool backup_server = false;
        bool server_mode = false;
        bool help = false;
        
    
    config() {
        opt_group{custom_options_, "global"}
            .add(master_file,   "master,m", "Path/name of master file")
            .add(startGRU,      "gru,g", "Starting GRU Index")
            .add(countGRU,      "countGRU,t", "Number of GRUs to run in subset")
            .add(config_file,   "config,c", "Path name of the config directory")
            .add(generate_config, "gen-config", "Generate a config file")
            .add(backup_server, "backup-server,b", "flag to denote if the server starting is a backup server")
            .add(server_mode,   "server-mode,s", "enable server mode")
            .add(host,          "host", "Hostname of the server")
            .add(help,          "help,h", "Print this help message");
    }
};


void run_client(actor_system& system, const config& cfg, Distributed_Settings distributed_settings) {
    scoped_actor self{system};

    aout(self) << "Starting SUMMA-Client in Distributed Mode\n";
    
    auto client = system.spawn(summa_client, distributed_settings);   
}

void run_server(actor_system& system, const config& cfg, 
                Distributed_Settings distributed_settings, 
                Summa_Actor_Settings summa_actor_settings, 
                File_Access_Actor_Settings file_access_actor_settings,
                Job_Actor_Settings job_actor_settings, 
                HRU_Actor_Settings hru_actor_settings) {
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

  } else {  
    aout(self) << "\n\n*****Starting SUMMA-Server*****\n\n";
    auto server = system.spawn(summa_server, 
                               distributed_settings,
                               summa_actor_settings, 
                               file_access_actor_settings, 
                               job_actor_settings, 
                               hru_actor_settings);
  }

}


void caf_main(actor_system& sys, const config& cfg) {
  scoped_actor self{sys};
  int err;

  if (cfg.generate_config) {
    std::cout << "Generating Config File" << std::endl;
    generate_config_file();
    return;
  }

  // Check if the master file was if not check if the config file was specified
  if (!std::filesystem::exists((std::filesystem::path) cfg.master_file)) {
    if (!std::filesystem::exists((std::filesystem::path) cfg.config_file)) {
      aout(self) << "\n\n**** Config (-c) or Master File (-m) "
                 << "Does Not Exist or Not Specified!! ****\n\n" 
                 << command_line_help << std::endl;
      return;
    }
  }

  Distributed_Settings distributed_settings = readDistributedSettings(cfg.config_file);
  Summa_Actor_Settings summa_actor_settings = readSummaActorSettings(cfg.config_file);
  File_Access_Actor_Settings file_access_actor_settings = readFileAccessActorSettings(cfg.config_file);
  Job_Actor_Settings job_actor_settings = readJobActorSettings(cfg.config_file);
  HRU_Actor_Settings hru_actor_settings = readHRUActorSettings(cfg.config_file);

  // -m setting overides config file
  if (cfg.master_file != "")
    job_actor_settings.file_manager_path = cfg.master_file;
  
  check_settings_from_json(distributed_settings, summa_actor_settings, 
                           file_access_actor_settings, job_actor_settings,
                           hru_actor_settings);

  if (distributed_settings.distributed_mode && 
      !job_actor_settings.data_assimilation_mode) {
    // only command line arguments needed are config_file and server-mode
    if (cfg.server_mode) {
      run_server(sys, cfg, distributed_settings, summa_actor_settings, 
                  file_access_actor_settings, job_actor_settings, 
                  hru_actor_settings);
    } else {
      run_client(sys,cfg, distributed_settings);
    }

  } else if (distributed_settings.distributed_mode &&
             job_actor_settings.data_assimilation_mode &&
             cfg.server_mode) {
    
    auto dist_summa = sys.spawn(distributed_job_actor,
                                cfg.startGRU,
                                cfg.countGRU,
                                distributed_settings,
                                file_access_actor_settings,
                                job_actor_settings,
                                hru_actor_settings);
  
  } else if (distributed_settings.distributed_mode &&
             job_actor_settings.data_assimilation_mode) {
    
    auto node = sys.spawn(node_actor, 
                          cfg.host,
                          self,
                          distributed_settings, 
                          file_access_actor_settings, 
                          job_actor_settings, 
                          hru_actor_settings);

  } else {
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

/**
 Command Line Arguments Behavior:
 - Inseart -t to distinguish the optional argument of countGRU. The original
   Summa implementation usest "-g startGRU countGRU" to specify the number of 
   GRUs. CAF needs to have a way to differentiate the two arguments and we 
    chose to use -t for countGRU.
*/
int main(int argc, char** argv) {
    
  // Convert char** argv to vector<string> args
  std::vector<std::string> args(argv, argv + argc);

  // Insert -t for countGRU so CAF can differentiate the argument
  for (auto it = args.begin(); it != args.end(); ++it) {

    // Find -g and insert -t after it
    if (*it == "-g" && std::next(it) != args.end()) {
      auto count_gru = std::find_if(std::next(it), args.end(), 
                                    [](const std::string& arg) {
        return std::isdigit(arg.front());
      });
      if (count_gru != args.end()) {
        args.insert(std::next(count_gru), "-t");
      } else {
        std::cerr << "Error: -g requires a countGRU argument" << std::endl;
        return 1;
      }
      break;
    }
    else if (*it == "-h" || *it == "--help") {
      std::cout << command_line_help << std::endl;
      return 0;
    }
  }

  // Convert vector<string> args to char** argv2
  char** argv2 = new char*[args.size()];
  for (int i = 0; i < args.size(); ++i) {
      argv2[i] = new char[args[i].size() + 1];
      strcpy(argv2[i], args[i].c_str());
  }

  argc = args.size();
  exec_main_init_meta_objects<io::middleman, id_block::summa>();
  caf::core::init_global_meta_objects(); 
  return exec_main<io::middleman, id_block::summa>(caf_main, argc, argv2);

}

