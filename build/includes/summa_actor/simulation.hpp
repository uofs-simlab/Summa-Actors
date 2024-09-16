#pragma once

class Simulation {
  private:
    // Initial Configuration
    int id_;
    std::string name_;
    std::string file_manager_;
    int start_gru_;
    int num_gru_;
    
    // Batch Information
    BatchContainer batch_container_;

    // Statistics
    int num_success_;
    int num_failed_;
    double run_time_;
    double read_time_;
    double write_time_;

  public:
};