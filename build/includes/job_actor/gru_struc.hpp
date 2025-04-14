#pragma once
#include "caf/all.hpp"
#include <string>
#include <iostream>

extern "C" {

  void f_readDimension(int& start_gru, int& num_gru, int& file_gru, 
                       int& file_hru, int& err, void* message);
  void f_setHruCount(int& iGRU, int& sGRU);
  void f_setIndexMap();
  void f_getNumHru(int& num_hru);

  void f_readIcondNlayers(int& num_gru, int& err, void* message);

  void f_getNumHruPerGru(int& arr_size, int& num_hru_per_gru_array);

  void f_deallocateGruStruc();
   bool f_get_default_tol();
  void f_set_default_tol(bool new_tol);
}

/** Determine the state of the GRU */
enum class gru_state { running, failed, succeeded , restarted};

/** Gru Information (meant to mimic gru_struc)*/
class GRU {
  private:
    int index_netcdf_;       // The index of the GRU in the netcdf file
    int index_job_;          // The index of the GRU within this job
    caf::actor actor_ref_;   // The actor for the GRU

    int num_hrus_;           // The number of HRUs in the GRU

    // Modifyable Parameters
    bool def_tol_;            // Default tol
    int dt_init_factor_;     // The initial dt for the GRU
    int be_steps_;           // The number of BE steps for the GRU
    // Relative Tolerances
    double rel_tol_;         // The relative tolerance for the GRU
    double rel_tol_temp_cas_; // The relative tolerance for the temperature of the cas
    double rel_tol_temp_veg_; // The relative tolerance for the temperature of the veg
    double rel_tol_wat_veg_;  // The relative tolerance for the water content of the veg
    double rel_tol_temp_soil_snow_; // The relative tolerance for the temperature of the soil snow
    double rel_tol_wat_snow_; // The relative tolerance for the water content of the snow
    double rel_tol_matric_;   // The relative tolerance for the matric potential
    double rel_tol_aquifr_;   // The relative tolerance for the aquifer
    // Absolute Tolerances
    double abs_tol_;         // The absolute tolerance for the GRU
    double abs_tolWat_;      // The absolute tolerance for the GRU water states
    double abs_tolNrg_;      // The absolute tolerance for the GRU energy states
    double abs_tol_temp_cas_; // The absolute tolerance for the temperature of the cas
    double abs_tol_temp_veg_; // The absolute tolerance for the temperature of the veg
    double abs_tol_wat_veg_;  // The absolute tolerance for the water content of the veg
    double abs_tol_temp_soil_snow_; // The absolute tolerance for the temperature of the soil snow
    double abs_tol_wat_snow_; // The absolute tolerance for the water content of the snow
    double abs_tol_matric_;   // The absolute tolerance for the matric potential
    double abs_tol_aquifr_;   // The absolute tolerance for the aquifer

    // Status Information
    int attempts_left_;      // The number of attempts left for the GRU to succeed
    gru_state state_;        // The state of the GRU

    // Timing Information
    double run_time_ = 0.0;  // The total time to run the GRU

    
  public:
    // Constructor
    GRU(int index_netcdf, int index_job, caf::actor actor_ref, 
        int dt_init_factor, int be_steps,
        // Relative Tolerances 
        double rel_tol, double rel_tol_temp_cas=0.0,
        double rel_tol_temp_veg=0.0, double rel_tol_wat_veg=0.0, 
        double rel_tol_temp_soil_snow=0.0, double rel_tol_wat_snow=0.0, 
        double rel_tol_matric=0.0, double rel_tol_aquifr=0.0,
        // Absolute Tolerances
        double abs_tol = 0.0, double abs_tolWat=0.0, double abs_tolNrg=0.0,
        double abs_tol_temp_cas=0.0, double abs_tol_temp_veg=0.0, 
        double abs_tol_wat_veg=0.0, double abs_tol_temp_soil_snow=0.0, 
        double abs_tol_wat_snow=0.0, double abs_tol_matric=0.0,
        double abs_tol_aquifr=0.0, 
        bool /*def_tol*/ = true, int max_attempts = 5)
        : index_netcdf_(index_netcdf), index_job_(index_job), 
          actor_ref_(actor_ref), dt_init_factor_(dt_init_factor),
          be_steps_(be_steps),
          // Relative Tolerances
          rel_tol_(rel_tol), rel_tol_temp_cas_(rel_tol_temp_cas),
          rel_tol_temp_veg_(rel_tol_temp_veg), 
          rel_tol_wat_veg_(rel_tol_wat_veg),
          rel_tol_temp_soil_snow_(rel_tol_temp_soil_snow), 
          rel_tol_wat_snow_(rel_tol_wat_snow),
          rel_tol_matric_(rel_tol_matric), rel_tol_aquifr_(rel_tol_aquifr),
          abs_tol_(abs_tol), abs_tolWat_(abs_tolWat), abs_tolNrg_(abs_tolNrg), 
          abs_tol_temp_cas_(abs_tol_temp_cas), 
          abs_tol_temp_veg_(abs_tol_temp_veg),
          abs_tol_wat_veg_(abs_tol_wat_veg), 
          abs_tol_temp_soil_snow_(abs_tol_temp_soil_snow),
          abs_tol_wat_snow_(abs_tol_wat_snow), abs_tol_matric_(abs_tol_matric),
          abs_tol_aquifr_(abs_tol_aquifr), 
          def_tol_(f_get_default_tol()),
          attempts_left_(max_attempts), state_(gru_state::running) {};

    // Deconstructor
    ~GRU() {};

    // Getters
    inline int getIndexNetcdf() const { return index_netcdf_; }
    inline int getIndexJob() const { return index_job_; }
    inline caf::actor getActorRef() const { return actor_ref_; }
    inline double getRunTime() const { return run_time_; }
    
    inline int getBeSteps() const { return be_steps_; }
    
    inline bool getDefTol() const { return def_tol_; }
    
    inline double getRelTol() const { return rel_tol_; }
    inline double getRelTolTempCas() const { return rel_tol_temp_cas_; }
    inline double getRelTolTempVeg() const { return rel_tol_temp_veg_;}
    inline double getRelTolWatVeg() const { return rel_tol_wat_veg_;}
    inline double getRelTolTempSoilSnow() const { return rel_tol_temp_soil_snow_;}
    inline double getRelTolWatSnow() const { return rel_tol_wat_snow_;}
    inline double getRelTolMatric() const { return rel_tol_matric_;}
    inline double getRelTolAquifr() const { return rel_tol_aquifr_;}
    
    inline double getAbsTol() const { return abs_tol_; }
    inline double getAbsTolWat() const { return abs_tolWat_; }
    inline double getAbsTolNrg() const { return abs_tolNrg_; }
    inline double getAbsTolTempCas() const { return abs_tol_temp_cas_;}
    inline double getAbsTolTempVeg() const { return abs_tol_temp_veg_;}
    inline double getAbsTolWatVeg() const { return abs_tol_wat_veg_;}
    inline double getAbsTolTempSoilSnow() const { return abs_tol_temp_soil_snow_;}
    inline double getAbsTolWatSnow() const { return abs_tol_wat_snow_;}
    inline double getAbsTolMatric() const { return abs_tol_matric_;}
    inline double getAbsTolAquifr() const { return abs_tol_aquifr_;}
    
    inline int getAttemptsLeft() const { return attempts_left_; }
    inline gru_state getStatus() const { return state_; }

    // Setters
    inline void setRunTime(double run_time) { run_time_ = run_time; }
    
    inline void setBeSteps(int be_steps) { be_steps_ = be_steps; }
    
    //Setting rel_tol will set all rel_tol_* to the same value
    inline void setRelTol(double rel_tol){
      rel_tol_ = rel_tol;
      rel_tol_temp_cas_ = rel_tol;
      rel_tol_temp_veg_ = rel_tol;
      rel_tol_temp_soil_snow_ = rel_tol;
      rel_tol_wat_veg_ = rel_tol;
      rel_tol_wat_snow_ = rel_tol;
      rel_tol_matric_ = rel_tol;
      rel_tol_aquifr_ = rel_tol;
    }
    inline void setRelTolTempCas(double tol){ rel_tol_temp_cas_ = tol;}
    inline void setRelTolTempVeg(double tol){ rel_tol_temp_veg_ = tol;}
    inline void setRelTolWatVeg(double tol){ rel_tol_wat_veg_ = tol;}
    inline void setRelTolTempSoilSnow(double tol){ rel_tol_temp_soil_snow_ = tol;}
    inline void setRelTolWatSnow(double tol){ rel_tol_wat_snow_ = tol;}
    inline void setRelTolMatric(double tol){ rel_tol_matric_ = tol;}
    inline void setRelTolAquifr(double tol){ rel_tol_aquifr_ = tol;}
    
    inline void setAbsTol(double abs_tol) { 
      abs_tol_ = abs_tol;
      abs_tol_temp_cas_ = abs_tol;
      abs_tol_temp_veg_ = abs_tol;
      abs_tol_temp_soil_snow_ = abs_tol;
      abs_tol_wat_veg_ = abs_tol;
      abs_tol_wat_snow_ = abs_tol;
      abs_tol_matric_ = abs_tol;
      abs_tol_aquifr_ = abs_tol;
    }
    inline void setAbsTolWat(double abs_tolWat) { abs_tolWat_ = abs_tolWat; }
    inline void setAbsTolNrg(double abs_tolNrg) { abs_tolNrg_ = abs_tolNrg; }    
    inline void setAbsTolTempCas(double tol){ abs_tol_temp_cas_ = tol;}
    inline void setAbsTolTempVeg(double tol){ abs_tol_temp_veg_ = tol;}
    inline void setAbsTolWatVeg(double tol){ abs_tol_wat_veg_ = tol;}
    inline void setAbsTolTempSoilSnow(double tol){ abs_tol_temp_soil_snow_ = tol;}
    inline void setAbsTolWatSnow(double tol){ abs_tol_wat_snow_ = tol;}
    inline void setAbsTolMatric(double tol){ abs_tol_matric_ = tol;}
    inline void setAbsTolAquifr(double tol){ abs_tol_aquifr_ = tol;}
    // TODO: Ashley's New Variables
    // inline void setRelTol(double rel_tol) { rel_tol_ = rel_tol; }
    
    inline void setSuccess() { state_ = gru_state::succeeded; }
    inline void setFailed() { state_ = gru_state::failed; }
    inline void setRunning() { state_ = gru_state::running; }
    inline void setRestarted() { state_ = gru_state::restarted; }

    // Methods
    inline bool isFailed() const { return state_ == gru_state::failed; }
    inline void decrementAttemptsLeft() { attempts_left_--; }
    inline void setActorRef(caf::actor gru_actor) { actor_ref_ = gru_actor; }
};


// Class to track Gru Information For Nodes in data assimilation
struct NodeGruInfo {
  int node_start_gru_;  // Start Gru for the node
  int start_gru_sim_;   // Start Gru provided by the user (global)
  int node_num_gru_;    // Number of GRUs for the node
  int num_gru_sim_;     // Number of GRUs provided by the user (global)
  int file_gru_;

  NodeGruInfo(int node_start_gru = -1, int start_gru_sim = -1, 
              int node_num_gru = -1, int num_gru_sim = -1, int file_gru = -1) 
              : node_start_gru_(node_start_gru), start_gru_sim_(start_gru_sim),
                node_num_gru_(node_num_gru), num_gru_sim_(num_gru_sim), 
                file_gru_(file_gru) {};
};
template <class Inspector>
bool inspect(Inspector& f, NodeGruInfo& x) {
  return f.object(x).fields(f.field("node_start_gru", x.node_start_gru_),
                            f.field("start_gru_sim", x.start_gru_sim_),
                            f.field("node_num_gru", x.node_num_gru_),
                            f.field("num_gru_sim", x.num_gru_sim_),
                            f.field("file_gru", x.file_gru_));
}

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
    std::vector<NodeGruInfo> node_gru_info_;

  
    // Runtime status of the GRUs
    int num_gru_done_ = 0;
    int num_gru_failed_ = 0;
    int num_retry_attempts_left_ = 0;
    int attempt_ = 1;

    // todo: check if this is necessary
    std::vector<int> num_hru_per_gru_;
  public:
    GruStruc(int start_gru, int num_gru, int num_retry_attempts);
    ~GruStruc(){f_deallocateGruStruc();};
    int readDimension();
    int readIcondNlayers();

    // Set the gru information for each node participating in data assimilation
    int setNodeGruInfo(int num_nodes);
    std::string getNodeGruInfoString();
    inline NodeGruInfo getNodeGruInfo(int index) {
      return node_gru_info_[index];
    }

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
    }

    inline void incrementNumGruDone() { num_gru_done_++; }
    inline void incrementNumGruFailed() { num_gru_failed_++; num_gru_done_++;}
    inline void decrementRetryAttempts() { num_retry_attempts_left_--; }
    inline int getRetryAttemptsLeft(){return num_retry_attempts_left_ ;}
    inline void decrementNumGruFailed() { num_gru_failed_--; num_gru_done_--;}
    inline GRU* getGRU(int index) { return gru_info_[index-1].get(); }

    inline bool isDone() { return num_gru_done_ >= num_gru_; }
    inline bool hasFailures() { return num_gru_failed_ > 0; }
    inline bool shouldRetry() { return num_retry_attempts_left_ > 0; }

    int getFailedIndex(); 
    void getNumHrusPerGru();

    // todo: check if this is necessary
    inline int getNumHruPerGru(int index) { return num_hru_per_gru_[index]; }
};