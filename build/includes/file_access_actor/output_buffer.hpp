#pragma once
#include "settings_functions.hpp"
#include "fortran_data_types.hpp"
#include "num_gru_info.hpp"
#include "caf/all.hpp"

extern "C" {
  void f_defOutput(void *handle_ncid, int& start_gru, int& num_gru, 
                   int& num_hru, int& file_gru, bool& use_extention,
                   char const* output_extention, int& err, void* message);
  void f_setChunkSize(int& chunk_size);
  void f_allocateOutputBuffer(int& max_steps, int& num_gru, int& err, 
                              void* message);
  void f_deallocateOutputBuffer(void *handle_ncid_);

  void writeOutput_fortran(void* handle_ncid, int& num_steps, int& start_gru, 
                           int& max_gru, bool& writeParamFlag, int& err,
                           void* message);
}


struct WriteOutputReturn {
  int err;
  std::string message;
  std::vector<caf::actor> actor_to_update;
  int num_steps_update;
};


class OutputPartition {
  private:
    int start_gru_;
    int end_gru_;
    int num_gru_;
    int num_gru_active_;
    int num_steps_buffer_;
    int steps_remaining_;
    bool write_params_ = true;

    std::vector<caf::actor> ready_to_write_;
    WriteOutputReturn write_status_;

  public:
    OutputPartition(int start_gru, int num_gru, int num_steps_buffer, 
                    int num_timesteps) : start_gru_(start_gru), 
                    num_gru_(num_gru), num_steps_buffer_(num_steps_buffer), 
                    steps_remaining_(num_timesteps) {
      end_gru_ = start_gru_ + num_gru_ - 1;

      if (num_steps_buffer_ > steps_remaining_) {
        num_steps_buffer_ = steps_remaining_;
      }
    };

    inline const int getStartGru() { return start_gru_;};
    inline const int getEndGru() { return end_gru_;};

    inline const int getNumStepsBuffer() { return num_steps_buffer_;};

    const std::optional<WriteOutputReturn*> writeOutput(
        caf::actor gru, void* handle_ncid);

    bool isWriteParams();
};

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

    int num_gru_partition_;

    bool write_params_da_ = true;

    std::vector<std::unique_ptr<OutputPartition>> partitions_;
    bool rerunning_failed_grus_ = false;


  public:
    OutputBuffer(FileAccessActorSettings fa_settings, NumGRUInfo num_gru_info,
                 int num_hru, int num_timesteps) : fa_settings_(fa_settings), 
                 num_gru_info_(num_gru_info), num_hru_(num_hru) {

      // Construct internal data structures            
      handle_ncid_ = std::unique_ptr<void, OutputFileDeleter>(
          new_handle_var_i(), OutputFileDeleter());
      
      int num_buffer_steps = fa_settings_.num_timesteps_in_output_buffer_;
      int num_partitions = fa_settings_.num_partitions_in_output_buffer_;
      int num_gru = num_gru_info_.num_gru_local;
      
      // Construct the partitions
      if (num_partitions > num_gru) {
        num_partitions = num_gru;
      }

      int start_gru = 1;
      num_gru_partition_ = num_gru / num_partitions;
      int remainder = num_gru % num_partitions;

      for (int i = 0; i < num_partitions; i++) {
        int num_gru_container = num_gru_partition_ + (i < remainder ? 1 : 0);
        partitions_.push_back(std::make_unique<OutputPartition>(
            start_gru, num_gru_container, num_buffer_steps, num_timesteps));
        start_gru += num_gru_container;
      }
    };

    ~OutputBuffer() {
      f_deallocateOutputBuffer(handle_ncid_.get());
    };

    int getNumStepsBuffer(int gru_index);

    int defOutput(const std::string& actor_address);
    int setChunkSize();
    int allocateOutputBuffer(int num_timesteps);
    const std::optional<WriteOutputReturn*> writeOutput(
        int index_gru, caf::actor gru);
    const int writeOutputDA();


};