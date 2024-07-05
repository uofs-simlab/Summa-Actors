#pragma once
#include "settings_functions.hpp"
#include "fortran_data_types.hpp"
#include "num_gru_info.hpp"
#include <cmath>

extern "C" {
  void f_defOutput(void *handle_ncid, int& start_gru, int& num_gru, int& num_hru,
                   int& file_gru, bool& use_extention,
                   char const* output_extention, int& err, void* message);
  void f_setChunkSize(int& chunk_size);
  void f_allocateOutputBuffer(int& max_steps, int& num_gru, int& err, 
                              void* message);
  void f_deallocateOutputBuffer();
}


struct OutputFileDeleter {
  void operator()(void* handle) const {
    delete_handle_var_i(handle);
  }
};

/**
 * A buffer that manages the output for Summa.
 * This structure simply tracks what is going on in fortran 
 */
class OutputBuffer {
  private:
    FileAccessActorSettings fa_settings_;
    std::unique_ptr<void, OutputFileDeleter> handle_ncid_;
    NumGRUInfo num_gru_info_;
    int num_hru_;


  public:
    OutputBuffer(FileAccessActorSettings fa_settings, NumGRUInfo num_gru_info,
                 int num_hru) : fa_settings_(fa_settings), 
                 num_gru_info_(num_gru_info), num_hru_(num_hru) {

      // Construct internal data structures            
      handle_ncid_ = std::unique_ptr<void, OutputFileDeleter>(
          new_handle_var_i(), OutputFileDeleter());
    };

    ~OutputBuffer() {
      f_deallocateOutputBuffer();
    };

    int defOutput(const std::string& actor_address);
    int setChunkSize();
    int allocateOutputBuffer(int num_timesteps);


};