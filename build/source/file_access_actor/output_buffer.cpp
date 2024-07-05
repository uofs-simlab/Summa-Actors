#include "output_buffer.hpp"

using chrono_time = std::chrono::time_point<std::chrono::high_resolution_clock>;


int OutputBuffer::defOutput(const std::string& actor_address) {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);

  int start_gru = num_gru_info_.start_gru_local;
  int num_gru = num_gru_info_.num_gru_local;
  int file_gru = num_gru_info_.file_gru;

  std::string output_extention = "";
  if (num_gru_info_.use_global_for_data_structures) {
    output_extention = actor_address;
  }

  if (fa_settings_.output_file_suffix_ != "") {
    output_extention = fa_settings_.output_file_suffix_;
    num_gru_info_.use_global_for_data_structures = true;
  }

  f_defOutput(handle_ncid_.get(), start_gru, num_gru, num_hru_, file_gru,
              num_gru_info_.use_global_for_data_structures, 
              output_extention.c_str(), err, &message);
  if (err != 0) {
    std::cout << "Error: File Access Actor -- f_defOutput: " 
              << message.get() << "\n";
  }
  
  return err;
}


int OutputBuffer::setChunkSize() {
  int chunk_size = std::ceil(static_cast<double>(num_gru_info_.num_gru_local) 
                             / fa_settings_.num_partitions_in_output_buffer_);
  f_setChunkSize(chunk_size);
  return chunk_size;
}


int OutputBuffer::allocateOutputBuffer(int num_timesteps) {
  chrono_time start_time = std::chrono::high_resolution_clock::now();
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  int num_gru = num_gru_info_.num_gru_local;

  if(num_timesteps < fa_settings_.num_timesteps_in_output_buffer_) {
    fa_settings_.num_timesteps_in_output_buffer_ = num_timesteps;
  }

  f_allocateOutputBuffer(fa_settings_.num_timesteps_in_output_buffer_,
                         num_gru, err, &message);
  if (err != 0) {
    std::cout << "Error: FileAccessActor -- f_allocateOutputBuffer: " 
              << message.get() << "\n";
  }
  chrono_time end_time = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed_seconds = end_time - start_time;
  std::cout << "Time taken for allocateOutputBuffer: " 
            << elapsed_seconds.count() << "s\n";
  return err;
}