#pragma once
#include <string>
#include <fstream>

class Logger {
  private:
    std::string log_file_;
    bool enable_logging_;
  public:
    Logger(const std::string log_file_name = "");
    ~Logger();
    void log(const std::string &message);

    template <class Inspector>
    friend bool inspect(Inspector& inspector, Logger& logger) {
        return inspector.object(logger).fields(
              inspector.field("log_file", logger.log_file_));
    }

};

class ErrorLogger {
  private:
    std::string log_file_;
    std::string log_dir_;
    int attempt_ = 1;
    bool enable_logging_;         
  public:
    ErrorLogger(const std::string error_log_file_name = "");
    ~ErrorLogger() {};
    void logError(int ref_gru, int indx_gru, int timestep, double rel_tol, 
                  double abs_tol, double rel_tol_temp_cas, double rel_tol_temp_veg,
                  double rel_tol_wat_veg,double rel_tol_temp_soil_snow, 
                  double rel_tol_wat_snow, double rel_tol_matric, 
                  double rel_tol_aquifr, double abs_tol_temp_cas, 
                  double abs_tol_temp_veg, double abs_tol_wat_veg,
                  double abs_tol_temp_soil_snow, double abs_tol_wat_snow,
                  double abs_tol_matric, double abs_tol_aquifr,
                  bool default_tol, int err_code, 
                  const std::string &message);
    // void logError(int ref_gru, int indx_gru, int timestep, int be_steps, double rel_tol, 
    //               double abs_tolWat, double abs_tolNrg, int err_code, const std::string &message);
    void nextAttempt();
};


class SuccessLogger {
  private:
    std::string log_file_;
    std::string log_dir_;
    int attempt_ = 1;
    bool enable_logging_;
  public:
    SuccessLogger(const std::string success_log_file_name = "");
    ~SuccessLogger() {};
    void logSuccess(int ref_gru, int indx_gru, double rel_tol, double abs_tol,
                  double rel_tol_temp_cas, double rel_tol_temp_veg,
                  double rel_tol_wat_veg, double rel_tol_temp_soil_snow, 
                  double rel_tol_wat_snow, double rel_tol_matric, 
                  double rel_tol_aquifr, double abs_tol_temp_cas,
                  double abs_tol_temp_veg, double abs_tol_wat_veg,
                  double abs_tol_temp_soil_snow, double abs_tol_wat_snow,
                  double abs_tol_matric, double abs_tol_aquifr,
                  bool default_tol);
    // void logSuccess(int ref_gru, int indx_gru, int be_steps, double rel_tol, double abs_tolWat, double abs_tolNrg);
    void nextAttempt();
};