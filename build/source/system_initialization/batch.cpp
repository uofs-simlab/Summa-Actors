#include "batch.hpp"

Batch::Batch(std::string name, int batch_id, std::string file_manager, 
    int start_gru, int num_gru) :
  name_(name), batch_id_(batch_id), file_manager_(file_manager),
  start_gru_(start_gru), num_gru_(num_gru), run_time_(0.0), read_time_(0.0),
  write_time_(0.0), assigned_to_actor_(false), solved_(false) {};

std::string Batch::toString() const {
  std::string out_string = "batch_id: " + std::to_string(batch_id_) + "\n" +
      "\tname: " + name_ + "\n" +
      "\tstart_gru: " + std::to_string(start_gru_) + "\n" +
      "\tnum_gru: " + std::to_string(num_gru_) + "\n" +
      "\trun_time: " + std::to_string(run_time_) + "\n" + 
      "\tread_time: " + std::to_string(read_time_) + "\n" +
      "\twrite_time: " + std::to_string(write_time_) + "\n" +
      "\tnum_failed: " + std::to_string(num_failed_) + "\n" +
      "\tnum_solved: " + std::to_string(num_solved_) + "\n" +
      "\tsolved: " + std::to_string(solved_) + "\n";

  return out_string;
}

void Batch::writeBatchToFile(std::string file_name, std::string hostname) {
    std::ofstream output_file;
    output_file.open(file_name, std::ios_base::app);
    output_file <<
        batch_id_      << "," <<
        start_gru_     << "," << 
        num_gru_       << "," << 
        hostname       << "," <<   
        run_time_      << "," << 
        read_time_     << "," <<
        write_time_    << "\n";
    output_file.close();
}