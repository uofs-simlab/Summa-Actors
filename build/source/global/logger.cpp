#include "logger.hpp"

// ############################
// Logger
// ############################
Logger::Logger(const std::string log_file_name) {
  log_file_ = log_file_name;
  std::ofstream file;
  file.open(log_file_, std::ios::out);
  file << "####### " << log_file_ << " Start #######\n\n";
  file.close();
}
Logger::~Logger() {}

void Logger::log(const std::string &message) {
  std::ofstream file;
  file.open(log_file_, std::ios::out | std::ios::app);
  file << message << "\n";
  file.close();
}


// ############################
// CsvLogger
// ############################

CsvLogger::CsvLogger(const std::string csv_file_name, 
                        const std::string header) {
  csv_file_ = csv_file_name;
  std::ofstream file;
  file.open(csv_file_, std::ios::out);
  file << header << "\n";
  file.close();
}

