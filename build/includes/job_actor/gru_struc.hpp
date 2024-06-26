#pragma once
#include <string>
#include "GRU.hpp"

extern "C" {
  void read_dimension_fortran(int& start_gru, int& num_gru, int& num_hru, 
                              int& file_gru, int& file_hru, int& err, 
                              void* message);

  void read_icond_nlayers_fortran(int& num_gru, int& err, void* message);

  void get_num_hru_per_gru_fortran(int& arr_size, int& num_hru_per_gru_array);

  void deallocate_gru_struc_fortran();
}

class GruStruc {
  
  public:
    GruStruc(int start_gru, int num_gru, int num_retry_attempts);
    ~GruStruc(){deallocate_gru_struc_fortran();};
    int ReadDimension();
    int ReadIcondNlayers();
    inline int getStartGru() const { return start_gru_; }
    inline int getNumGrus() const { return num_gru_; }
    inline int get_file_gru() const { return file_gru_; }
    inline int getNumHrus() const { return num_hru_; }
    inline int get_gru_info_size() const { return gru_info_.size(); }
    inline int getNumGrusDone() const { return num_gru_done_; }
    inline int getNumGRUFailed() const { return num_gru_failed_; }

    inline void addGRU(std::unique_ptr<GRU> gru) {
      gru_info_[gru->getIndexJob() - 1] = std::move(gru);
    }

    inline void incrementNumGRUDone() { num_gru_done_++; }
    inline void incrementNumGRUFailed() { num_gru_failed_++; num_gru_done_++;}
    inline void decrementRetryAttempts() { num_retry_attempts_left_--; }
    inline void decrementNumGRUFailed() { num_gru_failed_--; num_gru_done_--;}
    inline GRU* getGRU(int index) { return gru_info_[index-1].get(); }

    inline bool isDone() { return num_gru_done_ >= num_gru_; }
    inline bool hasFailures() { return num_gru_failed_ > 0; }
    inline bool shouldRetry() { return num_retry_attempts_left_ > 0; }

    int getFailedIndex() {
      for (int i = 0; i < gru_info_.size(); i++) {
        if (gru_info_[i]->getStatus() == gru_state::failed) {
          return gru_info_[i]->getIndexJob();
        }
      }
      return -1;
    }

    void getNumHrusPerGru();
    inline int getNumHruPerGru(int index) { return num_hru_per_gru_[index]; }

  private:
    // Inital Information about the GRUs
    int start_gru_;
    int num_gru_;
    int num_hru_;
    int file_gru_;
    int file_hru_;
    
    // GRU specific Information
    std::vector<std::unique_ptr<GRU>> gru_info_;
    std::vector<int> num_hru_per_gru_;

    // Runtime status of the GRUs
    int num_gru_done_ = 0;
    int num_gru_failed_ = 0;
    int num_retry_attempts_left_ = 0;
    int attempt_ = 1;

};