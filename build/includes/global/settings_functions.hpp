#pragma once
#include <string>
#include <vector>
#include <optional>
#include <iostream>
#include <fstream>
#include <thread>
#include "json.hpp"

#define SUCCESS 0
#define FAILURE -1
#define MISSING_INT -9999
#define MISSING_DOUBLE -9999.0
#define OUTPUT_TIMESTEPS 500
#define NUM_PARTITIONS 8
#define OUTPUT_FREQUENCY 1000
#define GRU_PER_JOB 1000

using json = nlohmann::json;

class DistributedSettings {
  public:
    bool distributed_mode_;                    
    std::vector<std::string> servers_list_;    
    int port_;                                
    int total_hru_count_;
    int num_hru_per_batch_;
    int num_nodes_;                            
    bool load_balancing_;

    DistributedSettings(bool distributed_mode = false, 
                        std::vector<std::string> servers_list = {}, 
                        int port = 0, 
                        int total_hru_count = 0, 
                        int num_hru_per_batch = 0, 
                        int num_nodes = 0, 
                        bool load_balancing = false)
        : distributed_mode_(distributed_mode), 
          servers_list_(std::move(servers_list)), 
          port_(port), 
          total_hru_count_(total_hru_count), 
          num_hru_per_batch_(num_hru_per_batch), 
          num_nodes_(num_nodes), 
          load_balancing_(load_balancing) {};
    ~DistributedSettings() {};

    std::string toString() {
      std::string str = "Distributed Settings:\n";
      str += "Distributed Mode: " + std::to_string(distributed_mode_) + "\n";
      str += "Servers List: ";
      for (auto& server : servers_list_) {
        str += server + " ";
      }
      str += "\n";
      str += "Port: " + std::to_string(port_) + "\n";
      str += "Total HRU Count: " + std::to_string(total_hru_count_) + "\n";
      str += "Num HRU Per Batch: " + std::to_string(num_hru_per_batch_) + "\n";
      str += "Num Nodes: " + std::to_string(num_nodes_) + "\n";
      str += "Load Balancing: " + std::to_string(load_balancing_) + "\n";
      return str;
    }

    template<class Inspector>
    friend bool inspect(Inspector& insp, DistributedSettings& settings) {
      return insp.object(settings).fields(
             insp.field("distributed_mode", settings.distributed_mode_),
             insp.field("servers_list",     settings.servers_list_),
             insp.field("port",             settings.port_),
             insp.field("total_hru_count",  settings.total_hru_count_),
             insp.field("num_hru_per_batch",settings.num_hru_per_batch_),
             insp.field("num_nodes",        settings.num_nodes_),
             insp.field("load_balancing",   settings.load_balancing_));
    }
};

class SummaActorSettings {
  public:
    int max_gru_per_job_;
    bool enable_logging_;
    std::string log_dir_;     

    SummaActorSettings(int max_gru_per_job = 0, bool enable_logging = false, 
                       std::string log_dir = "")
        : max_gru_per_job_(max_gru_per_job), enable_logging_(enable_logging),
          log_dir_(log_dir) {};
    ~SummaActorSettings() {};

    std::string toString() {
      std::string str = "Summa Actor Settings:\n";
      str += "Max GRU Per Job: " + std::to_string(max_gru_per_job_) + "\n";
      str += "Enable Logging: " + std::to_string(enable_logging_) + "\n";
      str += "Log Directory: " + log_dir_ + "\n";
      return str;
    }

    template<class Inspector>
    friend bool inspect(Inspector& insp, SummaActorSettings& settings) {
      return insp.object(settings).fields(
             insp.field("max_gru_per_job", settings.max_gru_per_job_),
             insp.field("enable_logging", settings.enable_logging_),  
             insp.field("log_dir",         settings.log_dir_));
    }
};

class FileAccessActorSettings {
  public:
    int num_partitions_in_output_buffer_;
    int num_timesteps_in_output_buffer_;
    std::string output_file_suffix_; 

    FileAccessActorSettings(int num_partitions_in_output_buffer = 0, 
                            int num_timesteps_in_output_buffer = 0, 
                            std::string output_file_suffix = "")
        : num_partitions_in_output_buffer_(num_partitions_in_output_buffer),
          num_timesteps_in_output_buffer_(num_timesteps_in_output_buffer),
          output_file_suffix_(output_file_suffix) {};
    ~FileAccessActorSettings() {};

    std::string toString() {
      std::string str = "File Access Actor Settings:\n";
      str += "Num Partitions in Output Buffer: " + 
             std::to_string(num_partitions_in_output_buffer_) + "\n";
      str += "Num Timesteps in Output Buffer: " + 
             std::to_string(num_timesteps_in_output_buffer_) + "\n";
      str += "Output File Suffix: " + output_file_suffix_ + "\n";
      return str;
    }

    template<class Inspector>
    friend bool inspect(Inspector& insp, FileAccessActorSettings& settings) {
      return insp.object(settings).fields(
             insp.field("num_partitions_in_output_buffer", 
                settings.num_partitions_in_output_buffer_),
             insp.field("num_timesteps_in_output_buffer", 
                settings.num_timesteps_in_output_buffer_),
              insp.field("output_file_suffix", settings.output_file_suffix_));
    }
};

class JobActorSettings {
  public:
    std::string file_manager_path_;
    int max_run_attempts_; 
    bool data_assimilation_mode_; 
    int batch_size_; 

    JobActorSettings(std::string file_manager_path = "", 
                     int max_run_attempts = 1, 
                     bool data_assimilation_mode = false, 
                     int batch_size = -9999)
        : file_manager_path_(file_manager_path), 
          max_run_attempts_(max_run_attempts),
          data_assimilation_mode_(data_assimilation_mode),
          batch_size_(batch_size) {};

    ~JobActorSettings() {};

    std::string toString() {
      std::string str = "Job Actor Settings:\n";
      str += "File Manager Path: " + file_manager_path_ + "\n";
      str += "Max Run Attempts: " + std::to_string(max_run_attempts_) + "\n";
      str += "Data Assimilation Mode: " + std::to_string(data_assimilation_mode_) + "\n";
      str += "Batch Size: " + std::to_string(batch_size_) + "\n";
      return str;
    }

    template<class Inspector>
    friend bool inspect(Inspector& insp, JobActorSettings& settings) {
      return insp.object(settings).fields(
             insp.field("file_manager_path", settings.file_manager_path_),
             insp.field("max_run_attempts", settings.max_run_attempts_),
             insp.field("data_assimilation_mode", 
                settings.data_assimilation_mode_),
             insp.field("batch_size", settings.batch_size_));
    }
};

class HRUActorSettings {
  public:
    bool print_output_;
    int output_frequency_;

    double abs_tolWat_;
    double abs_tolNrg_;    
    double rel_tol_;
    int be_steps_;

    HRUActorSettings(bool print_output = false, int output_frequency = 1000,
        double abs_tolWat = -9999, double abs_tolNrg = -9999, double rel_tol = -9999, int be_steps = -9999) 
        : print_output_(print_output), output_frequency_(output_frequency), 
        abs_tolWat_(abs_tolWat), abs_tolNrg_(abs_tolNrg), rel_tol_(rel_tol), be_steps_(be_steps)  {};
    ~HRUActorSettings() {};

    std::string toString() {
      std::string str = "HRU Actor Settings:\n";
      str += "Print Output: " + std::to_string(print_output_) + "\n";
      str += "Output Frequency: " + std::to_string(output_frequency_) + "\n";
      str += "Abs Tol Water: " + std::to_string(abs_tolWat_) + "\n";
      str += "Abs Tol Energy: " + std::to_string(abs_tolNrg_) + "\n";
      str += "Rel Tol: " + std::to_string(rel_tol_) + "\n";
      str += "BE Steps: " + std::to_string(be_steps_) + "\n";
      return str;
    }

    template<class Inspector>
    friend bool inspect(Inspector& insp, HRUActorSettings& settings) {
      return insp.object(settings).fields(
             insp.field("print_output",     settings.print_output_),
             insp.field("output_frequency", settings.output_frequency_),
             insp.field("abs_tolWat",       settings.abs_tolWat_),
             insp.field("abs_tolNrg",       settings.abs_tolNrg_),
             insp.field("rel_tol",          settings.rel_tol_));
             insp.field("be_steps",         settings.be_steps_);
    }
};

class Settings {
  private:
    std::string json_file_;
  public:
    DistributedSettings distributed_settings_;
    SummaActorSettings summa_actor_settings_;
    FileAccessActorSettings fa_actor_settings_;
    JobActorSettings job_actor_settings_;
    HRUActorSettings hru_actor_settings_;

    Settings(std::string json_file = "") : json_file_(json_file) {};
    ~Settings() {};
    int readSettings();
    void generateConfigFile();
    void printSettings();

    template<typename T>
    std::optional<T> getSettings(json settings, std::string key_1, 
                                 std::string key_2) {
      try {
        if (settings.find(key_1) != settings.end()) {
          json key_1_settings = settings[key_1];

          // find value behind second key
          if (key_1_settings.find(key_2) != key_1_settings.end()) {
            return key_1_settings[key_2];
          } else 
            return {};

        } else {
          return {}; // return none in the optional (error value)
        }
      } catch (json::exception& e) {
        std::cout << e.what() << "\n" << key_1 << "\n" << key_2 << "\n";
        return {};
      }                           
    }

    std::optional<std::vector<std::string>> getSettingsArray(
        json settings, std::string key_1, std::string key_2);

    template<class Inspector>
    friend bool inspect(Inspector& insp, Settings& settings) {
      return insp.object(settings).fields(
             insp.field("distributed_settings", settings.distributed_settings_),
             insp.field("summa_actor_settings", settings.summa_actor_settings_),
             insp.field("fa_actor_settings", settings.fa_actor_settings_),
             insp.field("job_actor_settings", settings.job_actor_settings_),
             insp.field("hru_actor_settings", settings.hru_actor_settings_),
             insp.field("json_file", settings.json_file_));
    }
}; 