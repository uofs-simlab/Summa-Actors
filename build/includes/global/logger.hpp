#pragma once
#include <string>
#include <fstream>

class Logger {
  private:
    std::string log_file_;
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


class CsvLogger {
  private:
    std::string csv_file_;
  public:
    CsvLogger(const std::string csv_file_name = "", 
              const std::string header = "");
    ~CsvLogger();
    void log(const std::string &message);

    template <class Inspector>
    friend bool inspect(Inspector& inspector, CsvLogger& csv_logger) {
        return inspector.object(csv_logger).fields(
              inspector.field("csv_file", csv_logger.csv_file_));
    }
};
