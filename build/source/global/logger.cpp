#include "logger.hpp"

/*******************************************************************************
 * Logger
*******************************************************************************/
Logger::Logger(const std::string log_file_name) {
  log_file_ = log_file_name + ".log";
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


/*******************************************************************************
 * ErrorLogger
*******************************************************************************/
ErrorLogger::ErrorLogger(const std::string log_dir) {
  log_dir_ = log_dir;
  log_file_ = log_dir + "failures_attempt_" + std::to_string(attempt_)
              + ".csv";
  std::ofstream file;
  file.open(log_file_, std::ios::out);
  file << "ref_gru,indx_gru,timestep,rel_tol,abs_tol,err_code,err_msg\n";
  file.close();
}

void ErrorLogger::logError(int ref_gru, int indx_gru, int timestep, 
                           double rel_tol, double abs_tol, int err_code, 
                           const std::string &message) {
  std::ofstream file;
  file.open(log_file_, std::ios::out | std::ios::app);
  file << ref_gru << "," << indx_gru << "," << timestep << "," << rel_tol << 
          "," << abs_tol << "," << err_code << "," << message << "\n";
  file.close();
}

void ErrorLogger::nextAttempt() {
  attempt_++;
  log_file_ = log_dir_ + "failures_attempt_" + std::to_string(attempt_) 
              + ".csv";
  std::ofstream file;
  file.open(log_file_, std::ios::out);
    file << "ref_gru,indx_gru,timestep,rel_tol,abs_tol,err_code,err_msg\n";
  file.close();
}


/*******************************************************************************
 * SuccessLogger
*******************************************************************************/
SuccessLogger::SuccessLogger(const std::string log_dir) {
  log_dir_ = log_dir;
  log_file_ = log_dir + "successes_attempt_" + std::to_string(attempt_)
              + ".csv";
  std::ofstream file;
  file.open(log_file_, std::ios::out);
    file << "ref_gru,indx_gru,rel_tol,abs_tol\n";
  file.close();
}

void SuccessLogger::logSuccess(int ref_gru, int indx_gru, double rel_tol, 
                               double abs_tol) {
  std::ofstream file;
  file.open(log_file_, std::ios::out | std::ios::app);
    file << ref_gru << "," << indx_gru << "," << rel_tol << 
            "," << abs_tol << "\n";
  file.close();
}

void SuccessLogger::nextAttempt() {
  attempt_++;
  log_file_ = log_dir_ + "successes_attempt_" + std::to_string(attempt_) 
              + ".csv";
  std::ofstream file;
  file.open(log_file_, std::ios::out);
    file << "ref_gru,indx_gru,rel_tol,abs_tol\n";
  file.close();
}

