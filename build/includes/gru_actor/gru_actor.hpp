#pragma once
#include "caf/all.hpp"
#include "fortran_data_types.hpp"
#include "settings_functions.hpp"
#include "message_atoms.hpp"
// #include "hru_actor.hpp"
#include <vector>

extern "C" {
  void f_getNumHruInGru(int& index_gru, int& num_hru);
  void f_initGru(int& index_gru, void* gru_data, int& output_buffer_steps, 
      int& err, void* message);
  void setupGRU_fortran(int& index_gru, void* gru_data, int& err, 
      void* message);
  void readGRURestart_fortran(int& index_gru, void* gru_data, int& err, 
      void* message);
  void setTimeZoneOffsetGRU_fortran(int& iFile, void* gru_data, int& err, 
      void* message);
  void readGRUForcing_fortran(int& index_gru, int& iStep, int& iRead, 
      int& iFile, void* gru_data, int& err, void* message);
  void runGRU_fortran(int& index_gru, int& timestep, void* gru_data, 
      int& dt_init_factor, int& err, void* message);
  void writeGRUOutput_fortran(int& index_gru, int& timestep, int& output_step, 
      void* gru_data, int& err, void* message);
  void f_setGruTolerances(void* gru_data, double& rel_tol, double& abs_tol,
      double& rel_tol_temp_cas, double& rel_tol_temp_veg, double& rel_tol_wat_veg, 
      double& rel_tol_temp_soil_snow, double& rel_tol_wat_snow, double& rel_tol_matric, 
      double& rel_tol_aquifr, double& abs_tol_temp_cas, double& abs_tol_temp_veg, 
      double& abs_tol_wat_veg, double& abs_tol_temp_soil_snow, double& abs_tol_wat_snow, 
      double& abs_tol_matric, double& abs_tol_aquifr, bool& default_tol);
}

struct GruDeleter {
  void operator()(void* ptr) const {
    delete_handle_gru_type(ptr);
  }
};

class GruActor {
  caf::event_based_actor* self_;
  int netcdf_index_;
  int job_index_;
  HRUActorSettings hru_actor_settings_;
  int num_steps_output_buffer_;
  caf::actor file_access_actor_;
  caf::actor parent_;


  int num_hrus_;
  std::unique_ptr<void, GruDeleter> gru_data_;

  double dt_init_ = 0.0;
  int dt_init_factor_ = 1;
  int num_steps_until_write_;
  int num_steps_ = 0;                    // number of time steps
  int timestep_ = 1;	                   // Current Timestep of HRU simulation
  int iFile_ = 1;
  int stepsInCurrentFFile_;             // number of time steps in current forcing file
  int forcingStep_ = 1;                 // index of current time step in current forcing file
  int output_step_ = 1; // index of current time step in output structure

  bool data_assimilation_mode_ = false;             

  public:
    GruActor(caf::event_based_actor* self, int netcdf_index, int job_index, 
             int num_steps, HRUActorSettings hru_actor_settings, 
             bool data_assimilation_mode, int num_output_steps, 
             caf::actor file_access_actor, caf::actor parent) 
             : self_(self), netcdf_index_(netcdf_index), job_index_(job_index), 
               num_steps_(num_steps), hru_actor_settings_(hru_actor_settings),
               data_assimilation_mode_(data_assimilation_mode),
               num_steps_output_buffer_(num_output_steps),
               file_access_actor_(file_access_actor), parent_(parent) {};

    caf::behavior make_behavior();
    caf::behavior async_mode();
    caf::behavior data_assimilation_mode();

    std::vector<HRU> serializeGRU();
    void deserializeGRU(std::vector<HRU>& hrus);

    void handleErr(int err, std::unique_ptr<char[]>& message);
};
