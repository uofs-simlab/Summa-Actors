#include "settings_functions.hpp"
extern "C" { 
  void f_set_default_tol(bool new_tol);
}

int Settings::readSettings() {
  std::ifstream settings_file(json_file_);
  json json_settings;
  if (!settings_file.good()) {
    std::cout << "Could not open settings file: " << json_file_ << 
                  "\n\tContinuing with default settings\n";
  } else {
    settings_file >> json_settings;
  }
  settings_file.close();

  distributed_settings_ = DistributedSettings(
    getSettings<bool>(json_settings, "Distributed_Settings", "distributed_mode")
        .value_or(false),
    getSettingsArray(json_settings, "Distributed_Settings", "servers_list")
        .value_or(std::vector<std::string> {}),
    getSettings<int>(json_settings, "Distributed_Settings", "port")
        .value_or(0),
    getSettings<int>(json_settings, "Distributed_Settings", "total_hru_count")
        .value_or(0),
    getSettings<int>(json_settings, "Distributed_Settings", "num_hru_per_batch")
        .value_or(0)
  );

  summa_actor_settings_ = SummaActorSettings(
    getSettings<int>(json_settings, "Summa_Actor", "max_gru_per_job")
        .value_or(GRU_PER_JOB),
    getSettings<bool>(json_settings, "Summa_Actor", "enable_logging")
        .value_or(false),
    getSettings<std::string>(json_settings, "Summa_Actor", "log_dir")
        .value_or("")
  );

  fa_actor_settings_ = FileAccessActorSettings(
    getSettings<int>(json_settings, "File_Access_Actor", 
        "num_partitions_in_output_buffer").value_or(NUM_PARTITIONS),
    getSettings<int>(json_settings, "File_Access_Actor", 
        "num_timesteps_in_output_buffer").value_or(OUTPUT_TIMESTEPS),
    getSettings<std::string>(json_settings,"File_Access_Actor", 
        "output_file_suffix").value_or("")
  );

  job_actor_settings_ = JobActorSettings(
    getSettings<std::string>(json_settings, "Job_Actor", "file_manager_path")
        .value_or(""),
    getSettings<int>(json_settings, "Job_Actor", "max_run_attempts")
        .value_or(1),
    getSettings<bool>(json_settings, "Job_Actor", "data_assimilation_mode")
        .value_or(false),
    getSettings<int>(json_settings, "Job_Actor", "batch_size")
        .value_or(MISSING_INT)
  );

  hru_actor_settings_ = HRUActorSettings(
    getSettings<bool>(json_settings, "HRU_Actor", "print_output")
        .value_or(true),
    getSettings<int>(json_settings, "HRU_Actor", "output_frequency")
        .value_or(OUTPUT_FREQUENCY));

  return SUCCESS;
}



std::optional<std::vector<std::string>> Settings::getSettingsArray(
		json settings, std::string key_1, std::string key_2) {
  std::vector<std::string> return_vector;
  // find first key
  try {
    if (settings.find(key_1) != settings.end()) {
      json key_1_settings = settings[key_1];

      // find value behind second key
      if (key_1_settings.find(key_2) != key_1_settings.end()) {
        for(auto& host : key_1_settings[key_2])
          return_vector.push_back(host["hostname"]);
      
        return return_vector;
      } 
      else 
        return {};

    } 
    else
      return {}; // return none in the optional (error value)
  } catch (json::exception& e) {
    std::cout << e.what() << "\n";
    std::cout << key_1 << "\n";
    std::cout << key_2 << "\n";
    return {};
  }
}


void Settings::printSettings() {
  std::cout << "************ DISTRIBUTED_SETTINGS ************\n"
            << distributed_settings_.toString() << "\n"
            << "************ SUMMA_ACTORS SETTINGS ************\n"
            << summa_actor_settings_.toString() << "\n"
            << "************ FILE_ACCESS_ACTOR SETTINGS ************\n"
            << fa_actor_settings_.toString() << "\n"
            << "************ JOB_ACTOR SETTINGS ************\n"
            << job_actor_settings_.toString() << "\n"
            << "************ HRU_ACTOR SETTINGS ************\n"
            << hru_actor_settings_.toString() << "\n"
            << "********************************************\n\n";
}

void Settings::generateConfigFile() {
    using json = nlohmann::ordered_json;
    json config_file; 
    config_file["Summa_Actor"] = {
        {"max_gru_per_job", GRU_PER_JOB},
        {"enable_logging", false},
        {"log_dir", ""}
    };
    config_file["File_Access_Actor"] = {
        {"num_partitions_in_output_buffer", NUM_PARTITIONS},
        {"num_timesteps_in_output_buffer", OUTPUT_TIMESTEPS}
    };
    config_file["Job_Actor"] = {
        {"file_manager_path", "/home/username/summa_file_manager"},
        {"max_run_attempts", 1},
        {"data_assimilation_mode", false},
        {"batch_size", MISSING_INT}
    };
    config_file["HRU_Actor"] = {
        {"print_output", true},
        {"output_frequency", OUTPUT_FREQUENCY},
        {"abs_tol", 1e1},
        {"rel_tol", 1e1},
        {"rel_tol_temp_cas", 1e1},
        {"rel_tol_temp_veg", 1e1},
        {"rel_tol_wat_veg", 1e1},
        {"rel_tol_temp_soil_snow", 1e1},
        {"rel_tol_wat_snow", 1e1},
        {"rel_tol_matric", 1e1},
        {"rel_tol_aquifr", 1e1},
        {"abs_tol_temp_cas", 1e1},
        {"abs_tol_temp_veg", 1e1},
        {"abs_tol_wat_veg", 1e1},
        {"abs_tol_temp_soil_snow", 1e1},
        {"abs_tol_wat_snow", 1e1},
        {"abs_tol_matric", 1e1},
        {"abs_tol_aquifr", 1e1},
        {"default_tol", true}
    };

    std::ofstream config_file_stream("config.json");
    config_file_stream << std::setw(4) << config_file.dump(2) << std::endl;
    config_file_stream.close();
}