#pragma once

#include "caf/all.hpp"

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
};

template<class Inspector>
bool inspect(Inspector& inspector, Distributed_Settings& distributed_settings) {
    return inspector.object(distributed_settings).fields(
                inspector.field("distributed_mode", distributed_settings.distributed_mode),
                inspector.field("hostname",         distributed_settings.hostname),
                inspector.field("port",             distributed_settings.port),
                inspector.field("total_hru_count",  distributed_settings.total_hru_count),
                inspector.field("num_hru_per_batch",distributed_settings.num_hru_per_batch));
}


struct Summa_Actor_Settings {
    int output_structure_size;
    int max_gru_per_job;
};

template<class Inspector>
bool inspect(Inspector& inspector, Summa_Actor_Settings& summa_actor_settings) {
    return inspector.object(summa_actor_settings).fields(
                inspector.field("output_structure_size", summa_actor_settings.output_structure_size),
                inspector.field("max_gru_per_job",       summa_actor_settings.max_gru_per_job));  
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
    bool output_csv;
    std::string csv_path;
};

template<class Inspector>
bool inspect(Inspector& inspector, Job_Actor_Settings& job_actor_settings) {
    return inspector.object(job_actor_settings).fields(
                inspector.field("file_manager_path", job_actor_settings.file_manager_path),
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


int read_settings_from_json(std::string json_settings_file_path,
                            Distributed_Settings &distributed_settings, 
                            Summa_Actor_Settings &summa_actor_settings,
                            File_Access_Actor_Settings &file_access_actor_settings, 
                            Job_Actor_Settings &job_actor_settings, 
                            HRU_Actor_Settings &hru_actor_settings);

void check_settings_from_json(Distributed_Settings &distributed_settings, 
    Summa_Actor_Settings &summa_actor_settings, File_Access_Actor_Settings &file_access_actor_settings, 
    Job_Actor_Settings &job_actor_settings, HRU_Actor_Settings &hru_actor_settings);