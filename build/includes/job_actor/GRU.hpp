#pragma once
#include "caf/all.hpp"

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
        int dt_init_factor, double rel_tol, double abs_tol, int max_attempts);

    // Deconstructor
    ~GRU();

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
