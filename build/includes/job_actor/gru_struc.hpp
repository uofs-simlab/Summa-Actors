#pragma once
#include "caf/all.hpp"
#include <string>

extern "C" {
  void read_dimension_fortran(int& start_gru, int& num_gru, int& num_hru, 
                              int& file_gru, int& file_hru, int& err, 
                              void* message);

  void read_icond_nlayers_fortran(int& num_gru, int& err, void* message);

  void get_num_hru_per_gru_fortran(int& arr_size, int& num_hru_per_gru_array);

  void deallocate_gru_struc_fortran();
}

/** Determine the state of the GRU */
enum class gru_state { running, failed, succeeded };

/** Gru Information (meant to mimic gru_struc)*/
class GRU {
  private:
    int index_netcdf_;       // The index of the GRU in the netcdf file
    int index_job_;          // The index of the GRU within this job
    caf::actor actor_ref_;   // The actor for the GRU

    int num_hrus_;           // The number of HRUs in the GRU

    // Modifyable Parameters
    int dt_init_factor_;     // The initial dt for the GRU
    double rel_tol_;         // The relative tolerance for the GRU
    double abs_tol_;         // The absolute tolerance for the GRU

    // Status Information
    int attempts_left_;      // The number of attempts left for the GRU to succeed
    gru_state state_;        // The state of the GRU

    // Timing Information
    double run_time_ = 0.0;  // The total time to run the GRU

    
  public:
    // Constructor
    GRU(int index_netcdf, int index_job, caf::actor actor_ref, 
        int dt_init_factor, double rel_tol, double abs_tol, int max_attempts) 
        : index_netcdf_(index_netcdf), index_job_(index_job), 
          actor_ref_(actor_ref), dt_init_factor_(dt_init_factor),
          rel_tol_(rel_tol), abs_tol_(abs_tol), attempts_left_(max_attempts),
          state_(gru_state::running) {};

    // Deconstructor
    ~GRU() {};

    // Getters
    inline int getIndexNetcdf() const { return index_netcdf_; }
    inline int getIndexJob() const { return index_job_; }
    inline caf::actor getActorRef() const { return actor_ref_; }
    inline double getRunTime() const { return run_time_; }
    inline double getRelTol() const { return rel_tol_; }
    inline double getAbsTol() const { return abs_tol_; }
    inline int getAttemptsLeft() const { return attempts_left_; }
    inline gru_state getStatus() const { return state_; }

    // Setters
    inline void setRunTime(double run_time) { run_time_ = run_time; }
    inline void setRelTol(double rel_tol) { rel_tol_ = rel_tol; }
    inline void setAbsTol(double abs_tol) { abs_tol_ = abs_tol; }
    inline void setSuccess() { state_ = gru_state::succeeded; }
    inline void setFailed() { state_ = gru_state::failed; }
    inline void setRunning() { state_ = gru_state::running; }

    // Methods
    inline bool isFailed() const { return state_ == gru_state::failed; }
    inline void decrementAttemptsLeft() { attempts_left_--; }
    inline void setActorRef(caf::actor gru_actor) { actor_ref_ = gru_actor; }
};


class GruStruc {
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
  
  public:
    GruStruc(int start_gru, int num_gru, int num_retry_attempts);
    ~GruStruc(){deallocate_gru_struc_fortran();};
    int ReadDimension();
    int ReadIcondNlayers();
    inline std::vector<std::unique_ptr<GRU>>& getGruInfo() { return gru_info_; }
    inline int getStartGru() const { return start_gru_; }
    inline int getNumGru() const { return num_gru_; }
    inline int getFileGru() const { return file_gru_; }
    inline int getNumHru() const { return num_hru_; }
    inline int getGruInfoSize() const { return gru_info_.size(); }
    inline int getNumGruDone() const { return num_gru_done_; }
    inline int getNumGruFailed() const { return num_gru_failed_; }

    inline void addGRU(std::unique_ptr<GRU> gru) {
      gru_info_.push_back(std::move(gru));
      // gru_info_[gru->getIndexJob() - 1] = std::move(gru);
    }

    inline void incrementNumGruDone() { num_gru_done_++; }
    inline void incrementNumGruFailed() { num_gru_failed_++; num_gru_done_++;}
    inline void decrementRetryAttempts() { num_retry_attempts_left_--; }
    inline void decrementNumGruFailed() { num_gru_failed_--; num_gru_done_--;}
    inline GRU* getGRU(int index) { return gru_info_[index-1].get(); }

    inline bool isDone() { return num_gru_done_ >= num_gru_; }
    inline bool hasFailures() { return num_gru_failed_ > 0; }
    inline bool shouldRetry() { return num_retry_attempts_left_ > 0; }

    int getFailedIndex(); 
    void getNumHrusPerGru();
    inline int getNumHruPerGru(int index) { return num_hru_per_gru_[index]; }
};