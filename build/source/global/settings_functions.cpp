#include "settings_functions.hpp"

// Default Values
int default_partition_count = std::thread::hardware_concurrency() / 2;
int missing_value = -9999;
int default_gru_per_job = 250;
int default_output_frequency = 1000;
int default_timesteps_output_buffer = 500;
int default_dt_init_factor = 1;
std::string log_dir = "";



int Settings::read_settings() {
  std::ifstream settings_file(json_file_);
  if (!settings_file.good()) {
    std::cerr << "Error: Could not open settings file: " << json_file_ << "\n";
    settings_file.close();
    return FAILURE;
  }
  settings_file >> settings_;
  settings_file.close();

  distributed_settings_ = DistributedSettings(
    get_settings<bool>("Distributed_Settings", "distributed_mode")
        .value_or(false),
    get_settings_array("Distributed_Settings", "servers_list")
        .value_or(std::vector<std::string>()),
    get_settings<int>("Distributed_Settings", "port")
        .value_or(MISSING_INT),
    get_settings<int>("Distributed_Settings", "total_hru_count")
        .value_or(MISSING_INT),
    get_settings<int>("Distributed_Settings", "num_hru_per_batch")
        .value_or(MISSING_INT),
    get_settings<int>("Distributed_Settings", "num_nodes")
        .value_or(MISSING_INT),
    get_settings<bool>("Distributed_Settings", "load_balancing")
        .value_or(false)
  );

  summa_actor_settings_ = SummaActorSettings(
    get_settings<int>("Summa_Actor", "max_gru_per_job")
        .value_or(default_gru_per_job),
    get_settings<std::string>("Summa_Actor", "log_dir")
        .value_or("")
  );

  fa_actor_settings_ = FileAccessActorSettings(
    get_settings<int>("File_Access_Actor", "num_partitions_in_output_buffer")
        .value_or(default_partition_count),
    get_settings<int>("File_Access_Actor", "num_timesteps_in_output_buffer")
        .value_or(default_timesteps_output_buffer),
    get_settings<std::string>("File_Access_Actor", "output_file_suffix")
        .value_or("")
  );

  job_actor_settings_ = JobActorSettings(
    get_settings<std::string>("Job_Actor", "file_manager_path")
        .value_or(""),
    get_settings<int>("Job_Actor", "max_run_attempts")
        .value_or(1),
    get_settings<bool>("Job_Actor", "data_assimilation_mode")
        .value_or(false),
    get_settings<int>("Job_Actor", "batch_size")
        .value_or(10)
  );

  hru_actor_settings_ = HRUActorSettings(
    get_settings<bool>("HRU_Actor", "print_output")
        .value_or(true),
    get_settings<int>("HRU_Actor", "output_frequency")
        .value_or(100),
    get_settings<int>("HRU_Actor", "dt_init_factor")
        .value_or(1)
  );

  return SUCCESS;
}



std::optional<std::vector<std::string>> Settings::get_settings_array(
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

// Distributed_Settings readDistributedSettings(std::string json_settings_file) {
//   Distributed_Settings distributed_settings;
//   std::string parent_key = "Distributed_Settings";

//   distributed_settings.distributed_mode = getSettings(json_settings_file, 
//       parent_key, "distributed_mode", 
//       distributed_settings.distributed_mode).value_or(false);

//   distributed_settings.servers_list = getSettingsArray(json_settings_file, 
//       parent_key, "servers_list").value_or(std::vector<std::string>());

//   distributed_settings.port = getSettings(json_settings_file, parent_key,
//       "port", distributed_settings.port).value_or(missing_value);

//   distributed_settings.total_hru_count = getSettings(json_settings_file, 
//       parent_key, "total_hru_count", 
//       distributed_settings.total_hru_count).value_or(missing_value);

//   distributed_settings.num_hru_per_batch = getSettings(json_settings_file, 
//     parent_key, "num_hru_per_batch", 
//     distributed_settings.num_hru_per_batch).value_or(missing_value);

//   distributed_settings.num_nodes = getSettings(json_settings_file, 
//       parent_key, "num_nodes", distributed_settings.num_nodes).value_or(1);

//   distributed_settings.load_balancing = getSettings(json_settings_file, 
//       parent_key, "load_balancing", 
//       distributed_settings.load_balancing).value_or(false);
  
//   return distributed_settings;
// }

// Summa_Actor_Settings readSummaActorSettings(std::string json_settings_file) {
//   Summa_Actor_Settings summa_actor_settings;
//   std::string parent_key = "Summa_Actor";
  
//   summa_actor_settings.max_gru_per_job = getSettings(json_settings_file, 
//       parent_key, "max_gru_per_job", 
//       summa_actor_settings.max_gru_per_job).value_or(250);
//   summa_actor_settings.log_dir = getSettings(json_settings_file, parent_key,
//       "log_dir", summa_actor_settings.log_dir).value_or("");

//   return summa_actor_settings;
// }

// File_Access_Actor_Settings readFileAccessActorSettings(std::string json_settings_file) {
//   // read file access actor settings
//   File_Access_Actor_Settings file_access_actor_settings;
//   std::string parent_key = "File_Access_Actor";
//   file_access_actor_settings.num_partitions_in_output_buffer = getSettings(
//       json_settings_file, parent_key, "num_partitions_in_output_buffer", 
//       file_access_actor_settings.num_partitions_in_output_buffer).value_or(
//       default_partition_count);

//   file_access_actor_settings.num_timesteps_in_output_buffer = getSettings(
//     json_settings_file, parent_key, "num_timesteps_in_output_buffer", 
//     file_access_actor_settings.num_timesteps_in_output_buffer).value_or(
//     default_timesteps_output_buffer);

//   return file_access_actor_settings;
// }

// Job_Actor_Settings readJobActorSettings(std::string json_settings_file) {
//   // read settings for job actor
//   Job_Actor_Settings job_actor_settings;
//   std::string parent_key = "Job_Actor";
//   job_actor_settings.file_manager_path = getSettings(json_settings_file, 
//       parent_key, "file_manager_path", 
//       job_actor_settings.file_manager_path).value_or("");
  
//   job_actor_settings.max_run_attempts = getSettings(json_settings_file, 
//       parent_key, "max_run_attempts", 
//       job_actor_settings.max_run_attempts).value_or(1);
  
//   job_actor_settings.data_assimilation_mode = getSettings(json_settings_file, 
//       parent_key, "data_assimilation_mode", 
//       job_actor_settings.data_assimilation_mode).value_or(false);

//   job_actor_settings.batch_size = getSettings(json_settings_file, parent_key,
//       "batch_size", job_actor_settings.batch_size).value_or(10);

//   return job_actor_settings;
// }


// HRU_Actor_Settings readHRUActorSettings(std::string json_settings_file) {
//     // read settings for HRU actor
//     HRU_Actor_Settings hru_actor_settings;
//     std::string parent_key = "HRU_Actor";
//     hru_actor_settings.print_output = getSettings(json_settings_file, parent_key, 
//         "print_output", hru_actor_settings.print_output).value_or(true);

//     hru_actor_settings.output_frequency = getSettings(json_settings_file, parent_key,
//         "output_frequency", hru_actor_settings.output_frequency).value_or(default_output_frequency);

//     hru_actor_settings.dt_init_factor = getSettings(json_settings_file, parent_key,
//         "dt_init_factor", hru_actor_settings.dt_init_factor).value_or(1);


//     /*
//     Set Tolerances
//     ---------------
//     We can use rel_tol and abs_tol to set the tolerances for all the state variables.
//     If we set rel_tol and abs_tol in the config.json file then we just don't include 
//     the other tolerance values and they will be set to the value of rtol and atol.
//     */
//     hru_actor_settings.rel_tol = getSettings(json_settings_file, parent_key,
//         "rel_tol", hru_actor_settings.rel_tol).value_or(missing_value);

//     hru_actor_settings.abs_tol = getSettings(json_settings_file, parent_key,
//         "abs_tol", hru_actor_settings.abs_tol).value_or(missing_value);

//     double local_rtol;
//     double local_atol;

//     if (hru_actor_settings.rel_tol > 0) {
//         local_rtol = hru_actor_settings.rel_tol;
//     } else {
//         local_rtol = 1e-6;
//     }

//     if (hru_actor_settings.abs_tol > 0) {
//         local_atol = hru_actor_settings.abs_tol;
//     } else {
//         local_atol = 1e-6;
//     }


//     return hru_actor_settings;
// }

void Settings::print_settings() {
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


// void check_settings_from_json(Distributed_Settings &distributed_settings, 
//     Summa_Actor_Settings &summa_actor_settings, 
//     File_Access_Actor_Settings &file_access_actor_settings, 
//     Job_Actor_Settings &job_actor_settings, 
//     HRU_Actor_Settings &hru_actor_settings) {
  
//   if (distributed_settings.distributed_mode) {
//     std::cout << "************ DISTRIBUTED_SETTINGS ************\n"
//               << distributed_settings.distributed_mode << "\n";
//     for (auto& host : distributed_settings.servers_list) {
//         std::cout << host << "\n";
//     }
//     std::cout << distributed_settings.port << "\n"
//               << distributed_settings.total_hru_count << "\n"
//               << distributed_settings.num_hru_per_batch << "\n";
//   }

//   std::cout << "************ SUMMA_ACTORS SETTINGS ************\n"
//             << "Max GRU per Job: "
//             << summa_actor_settings.max_gru_per_job << "\n"
//             << "Num Partitions in Output Buffer: "
//             << file_access_actor_settings.num_partitions_in_output_buffer << "\n"
//             << "Num Timesteps in Output Buffer: "
//             << file_access_actor_settings.num_timesteps_in_output_buffer << "\n"
//             << "File Manager Path: "
//             << job_actor_settings.file_manager_path << "\n"
//             << "Max Run Attempts Per GRU: "
//             << job_actor_settings.max_run_attempts << "\n"
//             << "Print GRU Timestep: "
//             << hru_actor_settings.print_output << "\n"
//             << "GRU Timestep Print Frequency: "
//             << hru_actor_settings.output_frequency << "\n"
//             << "GRU Initial Timestep Factor: "
//             << hru_actor_settings.dt_init_factor << "\n"
//             << "********************************************\n\n";

// }


void Settings::generate_config_file() {
    using json = nlohmann::ordered_json;
    json config_file; 
    config_file["Distributed_Settings"] = {
        {"distributed_mode", false},
        {"port", missing_value},
        {"total_hru_count", missing_value},
        {"num_hru_per_batch", missing_value},
        {"load_balancing", false},
        {"num_nodes", missing_value},
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
        {"rel_tol", missing_value},
        {"abs_tol", missing_value}
    };

    std::ofstream config_file_stream("config.json");
    config_file_stream << std::setw(4) << config_file.dump(2) << std::endl;
    config_file_stream.close();
}