#pragma once
#include <string>
#include <vector>
#include <optional>
#include "json.hpp"
#include <bits/stdc++.h>
#include <sys/stat.h>

using json = nlohmann::json;

struct Distributed_Settings;
struct Summa_Actor_Settings;
struct File_Access_Actor_Settings;
struct Job_Actor_Settings;
struct HRU_Actor_Settings;


struct Distributed_Settings {
    bool distributed_mode;  // flag for starting summa in distributed mode
    std::string hostname;   // the hostname of the server actor
    int port;               // the port number of the server actor
    int total_hru_count;
    int num_hru_per_batch;
    int heartbeat_interval; // number of seconds between each heartbeat message
    int lost_node_threshold; // the maximum value the lost_potentail_indicator value can be before
    // we assume the node is lost
    std::vector<std::string> backup_servers;
};

template<class Inspector>
bool inspect(Inspector& inspector, Distributed_Settings& distributed_settings) {
    return inspector.object(distributed_settings).fields(
                inspector.field("distributed_mode", distributed_settings.distributed_mode),
                inspector.field("hostname",         distributed_settings.hostname),
                inspector.field("port",             distributed_settings.port),
                inspector.field("total_hru_count",  distributed_settings.total_hru_count),
                inspector.field("num_hru_per_batch",distributed_settings.num_hru_per_batch),
                inspector.field("heartbeat_interval",distributed_settings.heartbeat_interval),
                inspector.field("lost_node_threshold",distributed_settings.lost_node_threshold));
}


struct Summa_Actor_Settings {
    int max_gru_per_job;
};

template<class Inspector>
bool inspect(Inspector& inspector, Summa_Actor_Settings& summa_actor_settings) {
    return inspector.object(summa_actor_settings).fields(
                inspector.field("max_gru_per_job",  summa_actor_settings.max_gru_per_job));  
}


struct File_Access_Actor_Settings {
    int num_vectors_in_output_manager;
};

template<class Inspector>
bool inspect(Inspector& inspector, File_Access_Actor_Settings& file_access_actor_settings) {
    return inspector.object(file_access_actor_settings).fields(
                inspector.field("num_vectors_in_output_manager", 
                    file_access_actor_settings.num_vectors_in_output_manager));
}

struct Job_Actor_Settings {
    std::string file_manager_path;
    int output_structure_size;
    bool output_csv;
    std::string csv_path;
};

template<class Inspector>
bool inspect(Inspector& inspector, Job_Actor_Settings& job_actor_settings) {
    return inspector.object(job_actor_settings).fields(
                inspector.field("file_manager_path", job_actor_settings.file_manager_path),
                inspector.field("output_structure_size", job_actor_settings.output_structure_size),
                inspector.field("output_csv",        job_actor_settings.output_csv),
                inspector.field("csv_path",          job_actor_settings.csv_path));
}


struct HRU_Actor_Settings {
    bool print_output;
    int output_frequency;
};

template<class Inspector>
bool inspect(Inspector& inspector, HRU_Actor_Settings& hru_actor_settings) {
    return inspector.object(hru_actor_settings).fields(
                inspector.field("print_output",     hru_actor_settings.print_output),
                inspector.field("output_frequency", hru_actor_settings.output_frequency));
}

// Read in the settings from JSON
template <typename T>
std::optional<T> getSettings(std::string json_settings_file, std::string key_1, std::string key_2, 
    T return_value) {
    json settings;
    std::ifstream settings_file(json_settings_file);
    settings_file >> settings;
    settings_file.close();
    
    // find first key
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
        std::cout << e.what() << "\n";
        std::cout << key_1 << "\n";
        std::cout << key_2 << "\n";
        return {};
    }
   
}

// Get settings from settings file in array format
std::optional<std::vector<std::string>> getSettingsArray(std::string json_settings_file, std::string key_1, std::string key_2);


// Load in the settings from Json For SUMMA
int read_settings_from_json(std::string json_settings_file_path,
                            Distributed_Settings &distributed_settings, 
                            Summa_Actor_Settings &summa_actor_settings,
                            File_Access_Actor_Settings &file_access_actor_settings, 
                            Job_Actor_Settings &job_actor_settings, 
                            HRU_Actor_Settings &hru_actor_settings);

// Check the settings - Print them out to stdout
void check_settings_from_json(Distributed_Settings &distributed_settings, 
    Summa_Actor_Settings &summa_actor_settings, File_Access_Actor_Settings &file_access_actor_settings, 
    Job_Actor_Settings &job_actor_settings, HRU_Actor_Settings &hru_actor_settings);