#include "batch.hpp"

Batch::Batch(int batch_id, int start_hru, int num_hru){
  batch_id_ = batch_id;
  start_hru_ = start_hru;
  num_hru_ = num_hru;
  run_time_ = 0.0;
  read_time_ = 0.0;
  write_time_ = 0.0;
  assigned_to_actor_ = false;
  solved_ = false;

}

// Getters
int Batch::getBatchID() { return batch_id_; }
int Batch::getStartHRU() { return start_hru_; }
int Batch::getNumHRU() { return num_hru_; }
double Batch::getRunTime() { return run_time_; }
double Batch::getReadTime() { return read_time_; }
double Batch::getWriteTime() { return write_time_; }
bool Batch::isAssigned() { return assigned_to_actor_; }
bool Batch::isSolved() { return solved_; }

std::string Batch::getBatchInfoString() {
  std::string out_string = "batch_id=" + std::to_string(batch_id_) +
                           " -- start_hru=" + std::to_string(start_hru_) +
                           " -- num_hru: " + std::to_string(num_hru_) + "\n";
  return out_string;
}

// Setters
void Batch::updateRunTime(double run_time) { run_time_ = run_time; }
void Batch::updateReadTime(double read_time) { read_time_ = read_time; }
void Batch::updateWriteTime(double write_time) { write_time_ = write_time; }
void Batch::updateAssigned(bool boolean) { assigned_to_actor_ = boolean; }
void Batch::updateSolved(bool boolean) { solved_ = boolean; }

void Batch::printBatchInfo() {
  std::cout << "batch_id=" << batch_id_ << " -- start_hru=" << start_hru_ 
            << " -- num_hru: " << num_hru_ << "\n";
}



std::string Batch::toString() {
  std::stringstream out_string;
  out_string << "batch_id: " << batch_id_ << "\n" <<
                "start_hru: " << start_hru_ << "\n" <<
                "num_hru: " << num_hru_ << "\n" <<
                "run_time: " << run_time_ << "\n" << 
                "read_time: " << read_time_ << "\n" <<
                "write_time: " << write_time_ << "\n" <<
                // "assigned_to_actor: " << assigned_to_actor_ << "\n" <<
                "solved: " << solved_ << "\n";

  return out_string.str();
}

void Batch::writeBatchToFile(std::string file_name, std::string hostname) {
    std::ofstream output_file;
    output_file.open(file_name, std::ios_base::app);
    output_file <<
        batch_id_      << "," <<
        start_hru_     << "," << 
        num_hru_       << "," << 
        hostname             << "," <<   
        run_time_      << "," << 
        read_time_     << "," <<
        write_time_    << "\n";
    output_file.close();
}