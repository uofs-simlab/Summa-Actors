#include "settings_functions.hpp"

// Default Values
int default_partition_count = std::thread::hardware_concurrency() / 2;
int missing_value = -9999;
int default_gru_per_job = 250;
int default_output_frequency = 1000;
int default_timesteps_output_buffer = 500;
int default_dt_init_factor = 1;



std::optional<std::vector<std::string>> getSettingsArray(std::string json_settings_file, std::string key_1, std::string key_2) {
    json settings;
    std::ifstream settings_file(json_settings_file);
    if (!settings_file.good()) return {}; // return none in the optional
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

Distributed_Settings readDistributedSettings(std::string json_settings_file) {
    Distributed_Settings distributed_settings;
    std::string parent_key = "Distributed_Settings";

    distributed_settings.distributed_mode = getSettings(json_settings_file, parent_key, 
        "distributed_mode", distributed_settings.distributed_mode).value_or(false);

    distributed_settings.servers_list = getSettingsArray(json_settings_file, parent_key,
        "servers_list").value_or(std::vector<std::string>());

    distributed_settings.port = getSettings(json_settings_file, parent_key,
        "port", distributed_settings.port).value_or(missing_value);

    distributed_settings.total_hru_count = getSettings(json_settings_file, parent_key,
        "total_hru_count", distributed_settings.total_hru_count).value_or(missing_value);

    distributed_settings.num_hru_per_batch = getSettings(json_settings_file, parent_key,
        "num_hru_per_batch", distributed_settings.num_hru_per_batch).value_or(missing_value);
    
    return distributed_settings;
}

Summa_Actor_Settings readSummaActorSettings(std::string json_settings_file) {
    Summa_Actor_Settings summa_actor_settings;
    std::string parent_key = "Summa_Actor";
    
    summa_actor_settings.max_gru_per_job = getSettings(json_settings_file, parent_key,
        "max_gru_per_job", summa_actor_settings.max_gru_per_job).value_or(250);

    return summa_actor_settings;
}

File_Access_Actor_Settings readFileAccessActorSettings(std::string json_settings_file) {
    // read file access actor settings
    File_Access_Actor_Settings file_access_actor_settings;
    std::string parent_key = "File_Access_Actor";
    file_access_actor_settings.num_partitions_in_output_buffer = getSettings(json_settings_file, parent_key,
        "num_partitions_in_output_buffer", file_access_actor_settings.num_partitions_in_output_buffer).value_or(default_partition_count);
    file_access_actor_settings.num_timesteps_in_output_buffer = getSettings(json_settings_file, parent_key,
        "num_timesteps_in_output_buffer", file_access_actor_settings.num_timesteps_in_output_buffer).value_or(default_timesteps_output_buffer);

    return file_access_actor_settings;
}

Job_Actor_Settings readJobActorSettings(std::string json_settings_file) {
    // read settings for job actor
    Job_Actor_Settings job_actor_settings;
    std::string parent_key = "Job_Actor";
    job_actor_settings.file_manager_path = getSettings(json_settings_file, parent_key,
        "file_manager_path", job_actor_settings.file_manager_path).value_or("");
    
    job_actor_settings.max_run_attempts = getSettings(json_settings_file, parent_key,
        "max_run_attempts", job_actor_settings.max_run_attempts).value_or(1);

    return job_actor_settings;
}


HRU_Actor_Settings readHRUActorSettings(std::string json_settings_file) {
    // read settings for HRU actor
    HRU_Actor_Settings hru_actor_settings;
    std::string parent_key = "HRU_Actor";
    hru_actor_settings.print_output = getSettings(json_settings_file, parent_key, 
        "print_output", hru_actor_settings.print_output).value_or(true);

    hru_actor_settings.output_frequency = getSettings(json_settings_file, parent_key,
        "output_frequency", hru_actor_settings.output_frequency).value_or(default_output_frequency);

    hru_actor_settings.dt_init_factor = getSettings(json_settings_file, parent_key,
        "dt_init_factor", hru_actor_settings.dt_init_factor).value_or(1);


    /*
    Set Tolerances
    ---------------
    We can use rel_tol and abs_tol to set the tolerances for all the state variables.
    If we set rel_tol and abs_tol in the config.json file then we just don't include 
    the other tolerance values and they will be set to the value of rtol and atol.
    */
    hru_actor_settings.rel_tol = getSettings(json_settings_file, parent_key,
        "rel_tol", hru_actor_settings.rel_tol).value_or(missing_value);

    hru_actor_settings.abs_tol = getSettings(json_settings_file, parent_key,
        "abs_tol", hru_actor_settings.abs_tol).value_or(missing_value);

    double local_rtol;
    double local_atol;

    if (hru_actor_settings.rel_tol > 0) {
        local_rtol = hru_actor_settings.rel_tol;
    } else {
        local_rtol = 1e-6;
    }

    if (hru_actor_settings.abs_tol > 0) {
        local_atol = hru_actor_settings.abs_tol;
    } else {
        local_atol = 1e-6;
    }

    hru_actor_settings.relTolTempCas = getSettings(json_settings_file, parent_key,
        "relTolTempCas", hru_actor_settings.relTolTempCas).value_or(local_rtol);

    hru_actor_settings.absTolTempCas = getSettings(json_settings_file, parent_key,
        "absTolTempCas", hru_actor_settings.absTolTempCas).value_or(local_atol);

    hru_actor_settings.relTolTempVeg = getSettings(json_settings_file, parent_key,
        "relTolTempVeg", hru_actor_settings.relTolTempVeg).value_or(local_rtol);

    hru_actor_settings.absTolTempVeg = getSettings(json_settings_file, parent_key,
        "absTolTempVeg", hru_actor_settings.absTolTempVeg).value_or(local_atol);

    hru_actor_settings.relTolWatVeg = getSettings(json_settings_file, parent_key,
        "relTolWatVeg", hru_actor_settings.relTolWatVeg).value_or(local_rtol);

    hru_actor_settings.absTolWatVeg = getSettings(json_settings_file, parent_key,
        "absTolWatVeg", hru_actor_settings.absTolWatVeg).value_or(local_atol);

    hru_actor_settings.relTolTempSoilSnow = getSettings(json_settings_file, parent_key,
        "relTolTempSoilSnow", hru_actor_settings.relTolTempSoilSnow).value_or(local_rtol);

    hru_actor_settings.absTolTempSoilSnow = getSettings(json_settings_file, parent_key,
        "absTolTempSoilSnow", hru_actor_settings.absTolTempSoilSnow).value_or(local_atol);

    hru_actor_settings.relTolWatSnow = getSettings(json_settings_file, parent_key,
        "relTolWatSnow", hru_actor_settings.relTolWatSnow).value_or(local_rtol);

    hru_actor_settings.absTolWatSnow = getSettings(json_settings_file, parent_key,
        "absTolWatSnow", hru_actor_settings.absTolWatSnow).value_or(local_atol);

    hru_actor_settings.relTolMatric = getSettings(json_settings_file, parent_key,
        "relTolMatric", hru_actor_settings.relTolMatric).value_or(local_rtol);

    hru_actor_settings.absTolMatric = getSettings(json_settings_file, parent_key,
        "absTolMatric", hru_actor_settings.absTolMatric).value_or(local_atol);

    hru_actor_settings.relTolAquifr = getSettings(json_settings_file, parent_key,
        "relTolAquifr", hru_actor_settings.relTolAquifr).value_or(local_rtol);

    hru_actor_settings.absTolAquifr = getSettings(json_settings_file, parent_key,
        "absTolAquifr", hru_actor_settings.absTolAquifr).value_or(local_atol);

    return hru_actor_settings;
}



void check_settings_from_json(Distributed_Settings &distributed_settings, 
    Summa_Actor_Settings &summa_actor_settings, File_Access_Actor_Settings &file_access_actor_settings, 
    Job_Actor_Settings &job_actor_settings, HRU_Actor_Settings &hru_actor_settings) {

    std::cout << "************ DISTRIBUTED_SETTINGS ************\n"
              << distributed_settings.distributed_mode << "\n";
    for (auto& host : distributed_settings.servers_list) {
        std::cout << host << "\n";
    }
    std::cout << distributed_settings.port << "\n"
              << distributed_settings.total_hru_count << "\n"
              << distributed_settings.num_hru_per_batch << "\n"
              << "************ SUMMA_ACTOR_SETTINGS ************\n"
              << summa_actor_settings.max_gru_per_job << "\n\n\n"
              << "************ FILE_ACCESS_ACTOR_SETTINGS ************\n"
              << file_access_actor_settings.num_partitions_in_output_buffer << "\n"
              << file_access_actor_settings.num_timesteps_in_output_buffer << "\n\n\n"
              << "************ JOB_ACTOR_SETTINGS ************\n"
              << job_actor_settings.file_manager_path << "\n"
              << "************ HRU_ACTOR_SETTINGS ************\n"
              << hru_actor_settings.print_output << "\n"
              << hru_actor_settings.output_frequency << "\n"
              << "rel_tol: "            << hru_actor_settings.rel_tol << "\n"
              << "abs_tol: "            << hru_actor_settings.abs_tol << "\n"
              << "relTolTempCas: "      << hru_actor_settings.relTolTempCas << "\n"
              << "absTolTempCas: "      << hru_actor_settings.absTolTempCas << "\n"
              << "relTolTempVeg: "      << hru_actor_settings.relTolTempVeg << "\n"
              << "absTolTempVeg: "      << hru_actor_settings.absTolTempVeg << "\n"
              << "relTolWatVeg: "       << hru_actor_settings.relTolWatVeg << "\n"
              << "absTolWatVeg: "       << hru_actor_settings.absTolWatVeg << "\n"
              << "relTolTempSoilSnow: " << hru_actor_settings.relTolTempSoilSnow << "\n"
              << "absTolTempSoilSnow: " << hru_actor_settings.absTolTempSoilSnow << "\n"
              << "relTolWatSnow: "      << hru_actor_settings.relTolWatSnow << "\n"
              << "absTolWatSnow: "      << hru_actor_settings.absTolWatSnow << "\n"
              << "relTolMatric: "       << hru_actor_settings.relTolMatric << "\n"
              << "absTolMatric: "       << hru_actor_settings.absTolMatric << "\n"
              << "relTolAquifr: "       << hru_actor_settings.relTolAquifr << "\n"
              << "absTolAquifr: "       << hru_actor_settings.absTolAquifr << "\n\n\n";

}


void generate_config_file() {
    json config_file; 
    config_file["Distributed_Settings"] = {
        {"distributed_mode", false},
        {"port", missing_value},
        {"total_hru_count", missing_value},
        {"num_hru_per_batch", missing_value},
        {"servers_list", {
            {{"hostname", "host_1"}},
            {{"hostname", "host_2"}},
            {{"hostname", "host_3"}}
        }}
    };

    config_file["Summa_Actor"] = {
        {"max_gru_per_job", default_gru_per_job}
    };
    config_file["File_Access_Actor"] = {
        {"num_partitions_in_output_buffer", default_partition_count},
        {"num_timesteps_in_output_buffer", default_timesteps_output_buffer}
    };
    config_file["Job_Actor"] = {
        {"file_manager_path", "/home/username/summa_file_manager"},
        {"max_run_attempts", 1}
    };
    config_file["HRU_Actor"] = {
        {"print_output", true},
        {"output_frequency", default_output_frequency},
        {"dt_init_factor", 1},
        {"rel_tol", missing_value},
        {"abs_tol", missing_value}
    };

    std::ofstream config_file_stream("config.json");
    config_file_stream << std::setw(4) << config_file.dump(4) << std::endl;
    config_file_stream.close();
}