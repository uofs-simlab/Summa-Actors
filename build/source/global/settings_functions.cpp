#include "settings_functions.hpp"

// Default Values
int default_partition_count = std::thread::hardware_concurrency() / 2;
int default_gru_per_job = 250;
int default_output_frequency = 1000;
int default_timesteps_output_buffer = 500;
int default_dt_init_factor = 1;
std::string log_dir = "";



int Settings::readSettings() {
  std::ifstream settings_file(json_file_);
  if (!settings_file.good()) {
    std::cerr << "Error: Could not open settings file: " << json_file_ << "\n";
    settings_file.close();
    return FAILURE;
  }
  settings_file >> settings_;
  settings_file.close();

  distributed_settings_ = DistributedSettings(
    getSettings<bool>("Distributed_Settings", "distributed_mode")
        .value_or(false),
    getSettingsArray("Distributed_Settings", "servers_list")
        .value_or(std::vector<std::string>()),
    getSettings<int>("Distributed_Settings", "port")
        .value_or(MISSING_INT),
    getSettings<int>("Distributed_Settings", "total_hru_count")
        .value_or(MISSING_INT),
    getSettings<int>("Distributed_Settings", "num_hru_per_batch")
        .value_or(MISSING_INT),
    getSettings<int>("Distributed_Settings", "num_nodes")
        .value_or(MISSING_INT),
    getSettings<bool>("Distributed_Settings", "load_balancing")
        .value_or(false)
  );

  summa_actor_settings_ = SummaActorSettings(
    getSettings<int>("Summa_Actor", "max_gru_per_job")
        .value_or(default_gru_per_job),
    getSettings<std::string>("Summa_Actor", "log_dir")
        .value_or("")
  );

  fa_actor_settings_ = FileAccessActorSettings(
    getSettings<int>("File_Access_Actor", "num_partitions_in_output_buffer")
        .value_or(default_partition_count),
    getSettings<int>("File_Access_Actor", "num_timesteps_in_output_buffer")
        .value_or(default_timesteps_output_buffer),
    getSettings<std::string>("File_Access_Actor", "output_file_suffix")
        .value_or("")
  );

  job_actor_settings_ = JobActorSettings(
    getSettings<std::string>("Job_Actor", "file_manager_path")
        .value_or(""),
    getSettings<int>("Job_Actor", "max_run_attempts")
        .value_or(1),
    getSettings<bool>("Job_Actor", "data_assimilation_mode")
        .value_or(false),
    getSettings<int>("Job_Actor", "batch_size")
        .value_or(10)
  );

  hru_actor_settings_ = HRUActorSettings(
    getSettings<bool>("HRU_Actor", "print_output").value_or(true),
    getSettings<int>("HRU_Actor", "output_frequency").value_or(100),
    getSettings<int>("HRU_Actor", "dt_init_factor").value_or(1),
    getSettings<double>("HRU_Actor", "rel_tol").value_or(MISSING_DOUBLE),
    getSettings<double>("HRU_Actor", "abs_tol").value_or(MISSING_DOUBLE)
  );

  return SUCCESS;
}



std::optional<std::vector<std::string>> Settings::getSettingsArray(
		std::string key_1, std::string key_2) {
  std::vector<std::string> return_vector;
  // find first key
  try {
    if (settings_.find(key_1) != settings_.end()) {
      json key_1_settings = settings_[key_1];

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
    config_file["Distributed_Settings"] = {
        {"distributed_mode", false},
        {"port", MISSING_INT},
        {"total_hru_count", MISSING_INT},
        {"num_hru_per_batch", MISSING_INT},
        {"load_balancing", false},
        {"num_nodes", MISSING_INT},
        {"servers_list", {
            {{"hostname", "host_1"}},
            {{"hostname", "host_2"}},
            {{"hostname", "host_3"}}
        }}
    };

    config_file["Summa_Actor"] = {
        {"max_gru_per_job", default_gru_per_job},
        {"log_dir", log_dir}
    };
    config_file["File_Access_Actor"] = {
        {"num_partitions_in_output_buffer", default_partition_count},
        {"num_timesteps_in_output_buffer", default_timesteps_output_buffer}
    };
    config_file["Job_Actor"] = {
        {"file_manager_path", "/home/username/summa_file_manager"},
        {"max_run_attempts", 1},
        {"data_assimilation_mode", false},
        {"batch_size", 10}
    };
    config_file["HRU_Actor"] = {
        {"print_output", true},
        {"output_frequency", default_output_frequency},
        {"dt_init_factor", 1},
        {"rel_tol", MISSING_DOUBLE},
        {"abs_tol", MISSING_DOUBLE}
    };

    std::ofstream config_file_stream("config.json");
    config_file_stream << std::setw(4) << config_file.dump(2) << std::endl;
    config_file_stream.close();
}