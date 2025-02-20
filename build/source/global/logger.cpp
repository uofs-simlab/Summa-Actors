#include "logger.hpp"

/*******************************************************************************
 * Logger
*******************************************************************************/
Logger::Logger(const std::string log_file_name) {
  if (log_file_name.empty()) {
    log_file_ = "";
    enable_logging_ = false;
    return;
  }

  log_file_ = log_file_name + ".log";
  std::ofstream file;
  file.open(log_file_, std::ios::out);
  file << "####### " << log_file_ << " Start #######\n\n";
  file.close();
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
  file << "ref_gru,indx_gru,timestep,rel_tol,abs_tol,rel_tol_temp_cas,rel_tol_temp_veg,"
          "rel_tol_wat_veg,rel_tol_temp_soil_snow,rel_tol_wat_snow,rel_tol_matric,"
          "rel_tol_aquifr,abs_tol_temp_cas,abs_tol_temp_veg,abs_tol_wat_veg,"
          "abs_tol_temp_soil_snow,abs_tol_wat_snow,abs_tol_matric,abs_tol_aquifr,"
          "default_tol,err_code,err_msg\n";
  file.close();
}

void ErrorLogger::logError(int ref_gru, int indx_gru, int timestep, 
                           double rel_tol, double abs_tol, double rel_tol_temp_cas, 
                           double rel_tol_temp_veg, double rel_tol_wat_veg, 
                           double rel_tol_temp_soil_snow, double rel_tol_wat_snow, 
                           double rel_tol_matric, double rel_tol_aquifr, double abs_tol_temp_cas,
                           double abs_tol_temp_veg, double abs_tol_wat_veg, 
                           double abs_tol_temp_soil_snow, double abs_tol_wat_snow, 
                           double abs_tol_matric, double abs_tol_aquifr, bool default_tol,
                           int err_code, const std::string &message) {
  if (!enable_logging_) return;
  std::ofstream file;
  file.open(log_file_, std::ios::out | std::ios::app);
  file << ref_gru << "," << indx_gru << "," << timestep << "," << rel_tol << 
          "," << abs_tol << "," << rel_tol_temp_cas << "," << rel_tol_temp_veg <<
          "," << rel_tol_wat_veg<< "," << rel_tol_temp_soil_snow<< "," << 
          rel_tol_wat_snow<< "," << rel_tol_matric<< "," << rel_tol_aquifr<< "," 
          << abs_tol_temp_cas<< "," << abs_tol_temp_veg<< "," << abs_tol_wat_veg<< 
          "," << abs_tol_temp_soil_snow<< "," << abs_tol_wat_snow<< "," << 
          abs_tol_matric<< "," << abs_tol_aquifr<< "," << default_tol << ","
          << err_code << "," << message << "\n";
  file.close();
}

void ErrorLogger::nextAttempt() {
  if (!enable_logging_) return;
  attempt_++;
  log_file_ = log_dir_ + "failures_attempt_" + std::to_string(attempt_) 
              + ".csv";
  std::ofstream file;
  file.open(log_file_, std::ios::out);
  file << "ref_gru,indx_gru,timestep,rel_tol,abs_tol,rel_tol_temp_cas,rel_tol_temp_veg,"
          "rel_tol_wat_veg,rel_tol_temp_soil_snow,rel_tol_wat_snow,rel_tol_matric,"
          "rel_tol_aquifr,abs_tol_temp_cas,abs_tol_temp_veg,abs_tol_wat_veg,"
          "abs_tol_temp_soil_snow,abs_tol_wat_snow,abs_tol_matric,abs_tol_aquifr,"
          "default_tol,err_code,err_msg\n";
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
  file << "ref_gru,indx_gru,rel_tol,abs_tol,rel_tol_temp_cas,rel_tol_temp_veg,"
          "rel_tol_wat_veg,rel_tol_temp_soil_snow,rel_tol_wat_snow,rel_tol_matric,"
          "rel_tol_aquifr,abs_tol_temp_cas,abs_tol_temp_veg,abs_tol_wat_veg,"
          "abs_tol_temp_soil_snow,abs_tol_wat_snow,abs_tol_matric,abs_tol_aquifr,"
          "default_tol\n";
  file.close();
}

void SuccessLogger::logSuccess(int ref_gru, int indx_gru, double rel_tol, 
                               double abs_tol,double rel_tol_temp_cas, 
                               double rel_tol_temp_veg, double rel_tol_wat_veg, 
                               double rel_tol_temp_soil_snow, double rel_tol_wat_snow, 
                               double rel_tol_matric, double rel_tol_aquifr, 
                               double abs_tol_temp_cas, double abs_tol_temp_veg, 
                               double abs_tol_wat_veg, double abs_tol_temp_soil_snow, 
                               double abs_tol_wat_snow, double abs_tol_matric, 
                               double abs_tol_aquifr, bool default_tol) {
  if (!enable_logging_) return;
  std::ofstream file;
  file.open(log_file_, std::ios::out | std::ios::app);
    file << ref_gru << "," << indx_gru << "," << rel_tol << 
            "," << abs_tol << "," << rel_tol_temp_cas << "," <<
            rel_tol_temp_veg << "," << rel_tol_wat_veg<< "," <<
            rel_tol_temp_soil_snow<< "," << rel_tol_wat_snow<< "," <<
            rel_tol_matric<< "," << rel_tol_aquifr<< "," << abs_tol_temp_cas<< ","
            << abs_tol_temp_veg<< "," << abs_tol_wat_veg<< "," <<
            abs_tol_temp_soil_snow<< "," << abs_tol_wat_snow<< "," <<
            abs_tol_matric<< "," << abs_tol_aquifr<< "," << default_tol << "\n";

  file.close();
}

void SuccessLogger::nextAttempt() {
  if (!enable_logging_) return;
  attempt_++;
  log_file_ = log_dir_ + "successes_attempt_" + std::to_string(attempt_) 
              + ".csv";
  std::ofstream file;
  file.open(log_file_, std::ios::out);
    file << "ref_gru,indx_gru,rel_tol,abs_tol,rel_tol_temp_cas,rel_tol_temp_veg,"
          "rel_tol_wat_veg,rel_tol_temp_soil_snow,rel_tol_wat_snow,rel_tol_matric,"
          "rel_tol_aquifr,abs_tol_temp_cas,abs_tol_temp_veg,abs_tol_wat_veg,"
          "abs_tol_temp_soil_snow,abs_tol_wat_snow,abs_tol_matric,abs_tol_aquifr,"
          "default_tol\n";
  file.close();
}

