// CAF Includes
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"
#include "summa_actor.hpp"
#include "da_server_actor.hpp"
#include "da_client_actor.hpp"
#include "message_atoms.hpp"
#include <iostream>
#include <fstream>

using namespace caf;

const std::string command_line_help = "Summa-Actors is in active development and some features may not be available.\n"
    "Usage: summa_actors -m master_file [-g startGRU countGRU] [-c config_file] [-b backup_server] [-s server_mode]\n"
    "Available options: \n"
    "\t-m, --master:         Define path/name of master file (can be specified in config)\n"
    "\t-g, --gru:            Run a subset of countGRU GRUs starting from index startGRU \n"
    "\t-c, --config:         Path name of the Summa-Actors config file (optional but recommended)\n"
    "\t-s  --suffix          Add fileSuffix to the output files\n"
    "\t    --gen-config:     Generate a config file \n"
    "\t    --host:           Hostname of the server \n"
    "\t-b, --backup-server:  Start backup server, requires a server and config_file \n"
    "\t    --server-mode:    Enable server mode \n"
    "\t-h, --help:           Print this help message \n"
    "\nUnimplemented Options: \n"
    "\t-n, --newFile         Define frequency [noNewFiles,newFileEveryOct1] of new output files\n"
    "\t-h, --hru             Run a single HRU with index of iHRU\n"
    "\t-r, --restart         Define frequency [y,m,d,e,never] to write restart files\n"
    "\t-p, --progress        Define frequency [m,d,h,never] to print progress\n"
    "\t-v, --version         Display version information of the current build\n";


/* Configuration class that handles the config and 
/  command line options for the actors program */
class config : public actor_system_config {
  public:
    int startGRU = -1;  
    int countGRU = -1;
    std::string master_file = "";
    std::string config_file = "";
    std::string host = "";
    std::string output_file_suffix = "";
    bool generate_config = false;
    bool backup_server = false;
    bool server_mode = false;
    bool help = false;
        
    
  config() {
    opt_group{custom_options_, "global"}
        .add(master_file,   "master,m", "Path/name of master file")
        .add(startGRU,      "gru,g", "Starting GRU Index")
        .add(countGRU,      "countGRU,t", "Number of GRUs to run in subset")
        .add(output_file_suffix, "suffix,s", "Add fileSuffix to the output files")
        .add(config_file,   "config,c", "Path name of the config directory")
        .add(generate_config, "gen-config", "Generate a config file")
        .add(backup_server, "backup-server,b", "flag to denote if the server starting is a backup server")
        .add(server_mode,   "server-mode", "enable server mode")
        .add(host,          "host", "Hostname of the server")
        .add(help,          "help,h", "Print this help message");
    }
};



int caf_main(actor_system& sys, const config& cfg) {
  scoped_actor self{sys};
  int err;
  sys.println("Starting SUMMA-Actors");

  Settings settings = Settings(cfg.config_file);
  if (cfg.generate_config) {
    settings.generateConfigFile();
    return EXIT_SUCCESS;
  }

  // Check if the master file was if not check if the config file was specified
  if (!std::ifstream(cfg.master_file)) {
    if (!std::ifstream(cfg.config_file)) {
      self->println("\n\n**** Config (-c) or Master File (-m) Does Not Exist or"
                    "Not Specified!! ****\n\nConfig File: {} \nMaster File: {}"
                    "\n\n{}", cfg.config_file, cfg.master_file, 
                    command_line_help);
      exit(EXIT_FAILURE);
    }
  }

  settings.readSettings();
  settings.printSettings();
  if (cfg.master_file != "")
    settings.job_actor_settings_.file_manager_path_ = cfg.master_file;
  if (cfg.output_file_suffix != "")
    settings.fa_actor_settings_.output_file_suffix_ = cfg.output_file_suffix;

  if (settings.distributed_settings_.distributed_mode_ &&
      settings.job_actor_settings_.data_assimilation_mode_) {
    
    cfg.server_mode ? 
        self->spawn(actor_from_state<DAServerActor>, cfg.startGRU, cfg.countGRU, 
                    settings) :
        self->spawn(actor_from_state<DAClientActor>, cfg.host, settings);

  } else {
    self->spawn(actor_from_state<SummaActor>, cfg.startGRU, cfg.countGRU, 
                settings, self);
  }
  return EXIT_SUCCESS;
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
        return EXIT_FAILURE;
      }
      break;
    }
    else if (*it == "-h" || *it == "--help") {
      std::cout << command_line_help << std::endl;
      return EXIT_SUCCESS;
    }
  }

  // Convert vector<string> args to char** argv2
  char** argv2 = new char*[args.size()];
  for (int i = 0; i < args.size(); ++i) {
      argv2[i] = new char[args[i].size() + 1];
      strcpy(argv2[i], args[i].c_str());
  }

  argc = args.size();
  exec_main_init_meta_objects<id_block::summa, io::middleman>();
  caf::core::init_global_meta_objects(); 
  return exec_main<id_block::summa, io::middleman>(caf_main, argc, argv2);
}

