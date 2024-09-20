#include "logger.hpp"

/*******************************************************************************
 * Logger
*******************************************************************************/
Logger::Logger(const std::string log_file_name, bool create_file) {
  if (log_file_name.empty()) {
    log_file_ = "";
    enable_logging_ = false;
    std::cout << "Error: No log file name provided. Logging disabled.\n";
    return;
  }

  enable_logging_ = true;
  log_file_ = log_file_name + ".log";
  std::ofstream file;

  // Check if file exists
  std::ifstream f(log_file_);
  bool file_exists = f.good();
  f.close();

  if (!create_file || file_exists) return;

  file.open(log_file_, std::ios::out);
  file << "####### " << log_file_ << " Start #######\n\n";
  file.close();
  std::cout << "Logger created: " << log_file_ << "\n";
}

Logger::~Logger() {}

void Logger::log(const std::string &message) {
  if (!enable_logging_) return;
  std::ofstream file;
  file.open(log_file_, std::ios::out | std::ios::app);
  file << message << "\n";
  file.close();
}


/*******************************************************************************
 * ErrorLogger
*******************************************************************************/
ErrorLogger::ErrorLogger(const std::string log_dir) {
  if (log_dir.empty()) {
    log_file_ = "";
    enable_logging_ = false;
    return;
  }

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
  if (!enable_logging_) return;
  std::ofstream file;
  file.open(log_file_, std::ios::out | std::ios::app);
  file << ref_gru << "," << indx_gru << "," << timestep << "," << rel_tol << 
          "," << abs_tol << "," << err_code << "," << message << "\n";
  file.close();
}

void ErrorLogger::nextAttempt() {
  if (!enable_logging_) return;
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
  if (log_dir.empty()) {
    log_file_ = "";
    enable_logging_ = false;
    return;
  }

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
  if (!enable_logging_) return;
  std::ofstream file;
  file.open(log_file_, std::ios::out | std::ios::app);
    file << ref_gru << "," << indx_gru << "," << rel_tol << 
            "," << abs_tol << "\n";
  file.close();
}

void SuccessLogger::nextAttempt() {
  if (!enable_logging_) return;
  attempt_++;
  log_file_ = log_dir_ + "successes_attempt_" + std::to_string(attempt_) 
              + ".csv";
  std::ofstream file;
  file.open(log_file_, std::ios::out);
    file << "ref_gru,indx_gru,rel_tol,abs_tol\n";
  file.close();
}

/*******************************************************************************
 * BatchLogger
*******************************************************************************/

BatchLogger::BatchLogger(const std::string file_name, bool create_file) {
  if (file_name.empty()) {
    log_file_ = "";
    return;
  }

  log_file_ = file_name;

  // Check if file exists
  std::ifstream f(log_file_);
  bool file_exists = f.good();
  f.close();

  if (!create_file || file_exists) return;

  std::ofstream file;
  file.open(log_file_, std::ios::out);
  file << "s/r,name,batch_id,start_gru,num_gru,status\n";
  file.close();
}

void BatchLogger::logBatch(const std::string &s_or_r, const Batch &batch, 
                           const std::string &status) {
  if (log_file_.empty()) return;

  std::ofstream file;
  file.open(log_file_, std::ios::out | std::ios::app);
  file << s_or_r << "," 
       << batch.getName() << "," 
       << batch.getBatchID() << "," 
       << batch.getStartHRU() << "," 
       << batch.getNumHRU() << "," 
       << status << "\n";
  file.close();
}
  