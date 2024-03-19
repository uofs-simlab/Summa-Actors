#pragma once
#include <string>
#include <vector>
#include <thread>
#include <optional>
#include "json.hpp"
#include <bits/stdc++.h>
#include <sys/stat.h>

using json = nlohmann::json;

struct File_Access_Actor_Settings;
struct Job_Actor_Settings;
struct HRU_Actor_Settings;


// ####################################################################
//                    Distributed Settings
// ####################################################################
struct Distributed_Settings;

struct Distributed_Settings {
    bool distributed_mode;  // flag for starting summa in distributed mode
    std::vector<std::string> servers_list;   // the hostname of the server actor
    int port;               // the port number of the server actor
    int total_hru_count;
    int num_hru_per_batch;
    int total_nodes;  // For the data-assimilation mode
};

template<class Inspector>
bool inspect(Inspector& inspector, Distributed_Settings& distributed_settings) {
    return inspector.object(distributed_settings).fields(
                inspector.field("distributed_mode", distributed_settings.distributed_mode),
                inspector.field("servers_list",     distributed_settings.servers_list),
                inspector.field("port",             distributed_settings.port),
                inspector.field("total_hru_count",  distributed_settings.total_hru_count),
                inspector.field("num_hru_per_batch",distributed_settings.num_hru_per_batch));
}

Distributed_Settings readDistributedSettings(std::string json_settings_file);



// ####################################################################
//                     SUMMA Actor Settings
// ####################################################################
struct Summa_Actor_Settings;

struct Summa_Actor_Settings {
    int max_gru_per_job;
};

template<class Inspector>
bool inspect(Inspector& inspector, Summa_Actor_Settings& summa_actor_settings) {
    return inspector.object(summa_actor_settings).fields(
                inspector.field("max_gru_per_job",  summa_actor_settings.max_gru_per_job));  
}

Summa_Actor_Settings readSummaActorSettings(std::string json_settings_file);

// ####################################################################
//                      File Access Actor Settings
// ####################################################################

struct File_Access_Actor_Settings {
    int num_partitions_in_output_buffer;
    int num_timesteps_in_output_buffer;
};

template<class Inspector>
bool inspect(Inspector& inspector, File_Access_Actor_Settings& file_access_actor_settings) {
    return inspector.object(file_access_actor_settings).fields(
                inspector.field("num_partitions_in_output_buffer", 
                    file_access_actor_settings.num_partitions_in_output_buffer),
                     inspector.field("num_timesteps_in_output_buffer", 
                    file_access_actor_settings.num_timesteps_in_output_buffer));
}

File_Access_Actor_Settings readFileAccessActorSettings(std::string json_settings_file);

// ####################################################################
//                          Job Actor Settings
// ####################################################################

struct Job_Actor_Settings {
    std::string file_manager_path;
    int max_run_attempts; // maximum number of times to attempt to run each HRU in a job
    bool data_assimilation_mode; // All HRUs actors much finish before the next time step is started 
    int batch_size; // Initial condition for the number of HRUs to run in a batch
};

template<class Inspector>
bool inspect(Inspector& inspector, Job_Actor_Settings& job_actor_settings) {
    return inspector.object(job_actor_settings).fields(
                inspector.field("file_manager_path", job_actor_settings.file_manager_path),
                inspector.field("max_run_attempts",  job_actor_settings.max_run_attempts),
                inspector.field("data_assimilation_mode",  job_actor_settings.data_assimilation_mode));
}

Job_Actor_Settings readJobActorSettings(std::string json_settings_file);

// ####################################################################
//                          HRU Actor Settings
// ####################################################################

struct HRU_Actor_Settings {
    bool print_output;
    int output_frequency;
    int dt_init_factor; // factor to multiply the initial timestep by
    double rel_tol;
    double abs_tol;
    double relTolTempCas;
    double absTolTempCas;
    double relTolTempVeg;
    double absTolTempVeg;
    double relTolWatVeg;
    double absTolWatVeg;
    double relTolTempSoilSnow;
    double absTolTempSoilSnow;
    double relTolWatSnow;
    double absTolWatSnow;
    double relTolMatric;
    double absTolMatric;
    double relTolAquifr;
    double absTolAquifr;
};

template<class Inspector>
bool inspect(Inspector& inspector, HRU_Actor_Settings& hru_actor_settings) {
    return inspector.object(hru_actor_settings).fields(
                inspector.field("print_output",     hru_actor_settings.print_output),
                inspector.field("output_frequency", hru_actor_settings.output_frequency),
                inspector.field("dt_init_factor",   hru_actor_settings.dt_init_factor),
                inspector.field("rel_tol",          hru_actor_settings.rel_tol),
                inspector.field("abs_tol",          hru_actor_settings.abs_tol));
}

HRU_Actor_Settings readHRUActorSettings(std::string json_settings_file);

// ####################################################################
//                      Non Actor Specific Settings
// ####################################################################

int checkFileExists(std::string file_path);

/**
 * @brief Get the Settings from json
 * Template function that can be used with retrieving any singular type from the settings file
 */
template <typename T>
std::optional<T> getSettings(std::string json_settings_file, std::string key_1, std::string key_2, 
    T return_value) {
    json settings;
    std::ifstream settings_file(json_settings_file);
    if (!settings_file.good()) return {};
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

// Check the settings - Print them out to stdout
void check_settings_from_json(Distributed_Settings &distributed_settings, 
    Summa_Actor_Settings &summa_actor_settings, File_Access_Actor_Settings &file_access_actor_settings, 
    Job_Actor_Settings &job_actor_settings, HRU_Actor_Settings &hru_actor_settings);

// Output a default configuration file
void generate_config_file();