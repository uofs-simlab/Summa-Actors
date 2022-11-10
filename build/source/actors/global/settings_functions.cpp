#include "settings_functions.hpp"

std::optional<std::vector<std::string>> getSettingsArray(std::string json_settings_file, 
    std::string key_1, std::string key_2) {
    json settings;
    std::ifstream settings_file(json_settings_file);
    settings_file >> settings;
    settings_file.close();
    std::vector<std::string> return_vector;

    // find first key
    try {
        if (settings.find(key_1) != settings.end()) {
            json key_1_settings = settings[key_1];

            // find value behind second key
            if (key_1_settings.find(key_2) != key_1_settings.end()) {
                for(auto& host : key_1_settings[key_2]) {
                    return_vector.push_back(host["hostname"]);
                }
                return return_vector;
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


int read_settings_from_json(std::string json_settings_file,
        Distributed_Settings &distributed_settings, 
        Summa_Actor_Settings &summa_actor_settings,
        File_Access_Actor_Settings &file_access_actor_settings, 
        Job_Actor_Settings &job_actor_settings, 
        HRU_Actor_Settings &hru_actor_settings) {
    
    // Check if File Exists
    struct stat buffer;
    if (stat(json_settings_file.c_str(), &buffer) != 0) {
        std::cout << "JSON Configuration File Could Not Be Found\n";
        return -1;
    }
    
    // read distributed settings
    std::string parent_key = "Distributed_Settings";
    distributed_settings.distributed_mode = getSettings(json_settings_file, parent_key, 
        "distributed_mode", distributed_settings.distributed_mode).value_or(false);

    distributed_settings.hostname = getSettings(json_settings_file, parent_key, 
        "hostname", distributed_settings.hostname).value_or("");

    distributed_settings.port = getSettings(json_settings_file, parent_key,
        "port", distributed_settings.port).value_or(-1);

    distributed_settings.total_hru_count = getSettings(json_settings_file, parent_key,
        "total_hru_count", distributed_settings.total_hru_count).value_or(-1);

    distributed_settings.num_hru_per_batch = getSettings(json_settings_file, parent_key,
        "num_hru_per_batch", distributed_settings.num_hru_per_batch).value_or(-1);

    distributed_settings.heartbeat_interval = getSettings(json_settings_file, parent_key,
        "heartbeat_interval", distributed_settings.heartbeat_interval).value_or(-1);
    
    distributed_settings.lost_node_threshold = getSettings(json_settings_file, parent_key,
        "lost_node_threshold", distributed_settings.lost_node_threshold).value_or(-1);

    distributed_settings.backup_servers = getSettingsArray(json_settings_file, parent_key,
        "backup_servers").value_or(std::vector<std::string>());
    
    // read settings for summa actor
    parent_key = "Summa_Actor";    
    summa_actor_settings.max_gru_per_job = getSettings(json_settings_file, parent_key,
        "max_gru_per_job", summa_actor_settings.max_gru_per_job).value_or(250);


    // read file access actor settings
    parent_key = "File_Access_Actor";
    file_access_actor_settings.num_vectors_in_output_manager = getSettings(json_settings_file, parent_key,
        "num_vectors_in_output_manager", file_access_actor_settings.num_vectors_in_output_manager).value_or(1);


    // read settings for job actor
    parent_key = "Job_Actor";
    job_actor_settings.file_manager_path = getSettings(json_settings_file, parent_key,
        "file_manager_path", job_actor_settings.file_manager_path).value_or("");

    job_actor_settings.output_structure_size = getSettings(json_settings_file, parent_key,
        "output_structure_size", job_actor_settings.output_structure_size).value_or(250);

    job_actor_settings.output_csv = getSettings(json_settings_file, parent_key,
        "output_csv", job_actor_settings.output_csv).value_or(false);

    job_actor_settings.csv_path = getSettings(json_settings_file, parent_key, 
        "csv_path", job_actor_settings.csv_path).value_or("");


    // read settings for hru_actor
    parent_key = "HRU_Actor";
    hru_actor_settings.print_output = getSettings(json_settings_file, parent_key, 
        "print_output", hru_actor_settings.print_output).value_or(true);

    hru_actor_settings.output_frequency = getSettings(json_settings_file, parent_key, 
        "output_frequency", hru_actor_settings.output_frequency).value_or(250);

    return 0;
}


void check_settings_from_json(Distributed_Settings &distributed_settings, 
    Summa_Actor_Settings &summa_actor_settings, File_Access_Actor_Settings &file_access_actor_settings, 
    Job_Actor_Settings &job_actor_settings, HRU_Actor_Settings &hru_actor_settings) {

    std::cout << "************ DISTRIBUTED_SETTINGS ************\n";
    std::cout << distributed_settings.distributed_mode << "\n";
    std::cout << distributed_settings.hostname << "\n";
    std::cout << distributed_settings.port << "\n";
    std::cout << distributed_settings.total_hru_count << "\n";
    std::cout << distributed_settings.num_hru_per_batch << "\n";
    std::cout << distributed_settings.heartbeat_interval << "\n";
    std::cout << distributed_settings.lost_node_threshold << "\n\n\n";

    std::cout << "************ SUMMA_ACTOR_SETTINGS ************\n";
    std::cout << summa_actor_settings.max_gru_per_job << "\n\n\n";

    std::cout << "************ FILE_ACCESS_ACTOR_SETTINGS ************\n";
    std::cout << file_access_actor_settings.num_vectors_in_output_manager << "\n\n\n";

    std::cout << "************ JOB_ACTOR_SETTINGS ************\n";
    std::cout << job_actor_settings.file_manager_path << "\n";
    std::cout << job_actor_settings.output_structure_size << "\n";
    std::cout << job_actor_settings.output_csv << "\n";
    std::cout << job_actor_settings.csv_path << "\n\n\n";

    std::cout << "************ HRU_ACTOR_SETTINGS ************\n";
    std::cout << hru_actor_settings.print_output << "\n";
    std::cout << hru_actor_settings.output_frequency << "\n\n\n"; 

}