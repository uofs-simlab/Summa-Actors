#include "fileManager.hpp"
#include <iostream>
#include <fstream>
#include <memory>

// Helper function to extract a string enclosed in single quotes
std::string extractEnclosed(const std::string& line) {
  std::size_t first_quote = line.find_first_of("'");
  std::size_t last_quote = line.find_last_of("'");
  if (first_quote != std::string::npos && last_quote != std::string::npos 
      && first_quote < last_quote) {
    return line.substr(first_quote + 1, last_quote - first_quote - 1);
  }
  return "";
}

fileManager::fileManager(const std::string& file_manager_path) {
  std::cout << "Rading File Manager\n";
  file_manager_path_ = file_manager_path;

  std::ifstream file(file_manager_path);
  if (!file.is_open()) {
    std::cerr << "Unable to open file: " << file_manager_path << std::endl;
    return;
  }

  std::string line;
  while (std::getline(file, line)) {
    if (line.compare(0, 14, "controlVersion") == 0) {
      control_vrs_ = extractEnclosed(line);
    } else if (line.compare(0, 12,"simStartTime") == 0) {
      sim_start_tm_ = extractEnclosed(line);
    } else if (line.compare(0, 10, "simEndTime") == 0) {
      sim_end_tm_ = extractEnclosed(line);
    } else if (line.compare(0, 10, "tmZoneInfo") == 0) {
      nc_time_zone_ = extractEnclosed(line);
    } else if (line.compare(0, 12, "settingsPath") == 0) {
      settings_path_ = extractEnclosed(line);
    } else if (line.compare(0, 11, "forcingPath") == 0) {
      forcing_path_ = extractEnclosed(line);
    } else if (line.compare(0, 10, "outputPath") == 0) {
      output_path_ = extractEnclosed(line);
    } else if (line.compare(0, 9, "statePath") == 0) {
      state_path_ = extractEnclosed(line);
    } else if (line.compare(0, 13, "decisionsFile") == 0) {
      m_decisions_ = extractEnclosed(line);
    } else if (line.compare(0, 17, "outputControlFile") == 0) {
      output_control_ = extractEnclosed(line);
    } else if (line.compare(0, 18, "globalHruParamFile") == 0) {
      localparam_info_ = extractEnclosed(line);
    } else if (line.compare(0, 18, "globalGruParamFile") == 0) {
      basinparam_info_ = extractEnclosed(line);
    } else if (line.compare(0, 13, "attributeFile") == 0) {
      local_attributes_ = extractEnclosed(line);
    } else if (line.compare(0, 14, "trialParamFile") == 0) {
      parameter_trial_ = extractEnclosed(line);
    } else if (line.compare(0, 12, "vegTableFile") == 0) {
      vegparm_ = extractEnclosed(line);
    } else if (line.compare(0, 13, "soilTableFile") == 0) {
      soilparm_ = extractEnclosed(line);
    } else if (line.compare(0, 16, "generalTableFile") == 0) {
      genparm_ = extractEnclosed(line);
    } else if (line.compare(0, 15, "noahmpTableFile") == 0) {
      mptable_ = extractEnclosed(line);
    } else if (line.compare(0, 15, "forcingListFile") == 0) {
      forcing_filelist_ = extractEnclosed(line);
    } else if (line.compare(0, 17, "initConditionFile") == 0) {
      model_initcond_ = extractEnclosed(line);
    } else if (line.compare(0, 13, "outFilePrefix") == 0) {
      output_prefix_ = extractEnclosed(line);
    } else {
      std::cerr << "Unrecognized line in file: " << line << std::endl;
    }
  }
  file.close();
}


std::string fileManager::setTimesDirsAndFiles() {
  int err = 0;
  std::unique_ptr<char[]> err_msg(new char[1024]);
  // Calls summa_SetTimesDirsAndFiles() 
  setTimesDirsAndFiles_fortran(file_manager_path_.c_str(), &err, &err_msg);
  return std::string(err_msg.get());
}

std::string fileManager::toString() {
  std::string str = "Control Version: " + control_vrs_ + "\n";
              str += "Simulation Start Time: " + sim_start_tm_ + "\n";
              str += "Simulation End Time: " + sim_end_tm_ + "\n";
              str += "Time Zone Info: " + nc_time_zone_ + "\n";
              str += "Settings Path: " + settings_path_ + "\n";
              str += "Forcing Path: " + forcing_path_ + "\n";
              str += "Output Path: " + output_path_ + "\n";
              str += "State Path: " + state_path_ + "\n";
              str += "Decisions File: " + m_decisions_ + "\n";
              str += "Output Control File: " + output_control_ + "\n";
              str += "Global HRU Param File: " + localparam_info_ + "\n";
              str += "Global GRU Param File: " + basinparam_info_ + "\n";
              str += "Attribute File: " + local_attributes_ + "\n";
              str += "Trial Param File: " + parameter_trial_ + "\n";
              str += "Veg Table File: " + vegparm_ + "\n";
              str += "Soil Table File: " + soilparm_ + "\n";
              str += "General Table File: " + genparm_ + "\n";
              str += "NoahMP Table File: " + mptable_ + "\n";
              str += "Forcing List File: " + forcing_filelist_ + "\n";
              str += "Init Condition File: " + model_initcond_ + "\n";
              str += "Output File Prefix: " + output_prefix_ + "\n";
  return str;
}