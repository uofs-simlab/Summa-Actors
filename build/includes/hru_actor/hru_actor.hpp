#pragma once

#include "caf/all.hpp"
#include "fortran_data_types.hpp"
#include "auxilary.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include <string>
#include "message_atoms.hpp"

/*********************************************
 * HRU Actor Fortran Functions
 *********************************************/
extern "C" {
  // Initialize HRU data_structures
  void initHRU_fortran(int& indxGRU, int& indx_hru, int& num_steps, 
                       void* hru_data, int& err, void* message);
  
  void setupHRU_fortran(int& indxGRU, int& indxHRU, void* hru_data, int& err, 
                        void* message);
  
  // Setup summa_readRestart File if this option has been chosen 
  void readHRURestart_fortran(int& indxGRU, int& indxHRU, void* hru_data, 
                              int& err, void* message);
                         
  void readHRUForcing_fortran(int& index_gru, int& indx_hru, int& iStep, 
                              int& iRead, int& iFile, void* hru_data,  
                              int& err, void* message);
  // Run the model for one timestep
  void runHRU_fortran(int& indx_gru, int& indx_hru, int& timestep, void* hru_data, 
                      int& dt_int_factor, double& walltime_timestep, 
                      int& err, void* message);
  
  void writeHRUOutput_fortran(int& index_gru, int& index_hru, int& timestep, 
                              int& output_step, void* hru_data, int& y, int& m, 
                              int& d, int& h, int& err, void* message);
    
  int  hru_writeRestart(int* index_hru, int* index_gru, int* timestep, 
                        int* output_step, void* hru_data, int* err);

  void setTimeZoneOffset_fortran(int& iFile, void* hru_data, int& err, 
                                 void* message);

  // hru_writeOutput.f90
  void setFinalizeStatsFalse(int* indx_gru);

  void get_sundials_tolerances(void* hru_data, double* relTol, double* absTol);
  void set_sundials_tolerances(void* hru_data, double* relTol, double* absTol);

  void setIDATolerances(void* hru_data, double* relTolTempCas, 
                        double* absTolTempCas, double* relTolTempVeg, 
                        double* absTolTempVeg, double* relTolWatVeg, 
                        double* absTolWatVeg, double* relTolTempSoilSnow,
                        double* absTolTempSoilSnow, double* relTolWatSnow, 
                        double* absTolWatSnow, double* relTolMatric,
                        double* absTolMatric, double* relTolAquifr, 
                        double* absTolAquifr);
}



/*********************************************
 * HRU Actor state variables
 *********************************************/
struct Date {
  int y;
  int m;
  int d;
  int h;
};

struct hru_state {
  // Actor References
	caf::actor file_access_actor;
	caf::actor parent;

  int ref_gru;  // Index of GRU in netcdf file
  int indx_gru; // Index of GRU in current job
  int indx_hru; // Index of HRU in current job

	int stepsInCurrentFFile;        // number of time steps in current forcing file
  int num_steps_until_write;      // number of time steps until we pause for FA_Actor to write

  // HRU data structures (formerly summa_type)
  void *hru_data = new_handle_hru_type();

  hru hru_data_serialized;

  // Misc Variables
  int timestep = 1;	    // Current Timestep of HRU simulation
  int forcingStep = 1;    // index of current time step in current forcing file
  int num_steps = 0;      // number of time steps
  int iFile = 1;              // index of current forcing file from forcing file list
  int dt_init_factor = 1; // factor of dt_init (coupled_em)
  int output_structure_step_index = 1; // index of current time step in output structure
  std::string err_message;

  // Sundials variables
  double rtol = -9999; // -9999 uses default
  double atol = -9999; // -9999 uses default

  double walltime_timestep = 0.0; // walltime for the current timestep		

  // Checkpointing variables
  Date startDate = {0,0,0,0}; // will be initalized when hru finishes first timestep
  Date currentDate = {0,0,0,0}; // will be initalized when hru finishes first timestep
  int  checkpoint= 0;
  int daysInMonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
  int restartFrequency; // 0=never(default) 1=hour 2=day 3=week? 4=month 5=year 
        

  // Settings
  HRU_Actor_Settings hru_actor_settings;
    
  ~hru_state() {
      delete_handle_hru_type(hru_data);
  }
};

caf::behavior hru_actor(caf::stateful_actor<hru_state>* self, int ref_gru, 
                        int indx_gru, HRU_Actor_Settings hru_actor_settings, 
                        caf::actor file_access_actor, caf::actor parent);

/*********************************************
 * Functions for the HRU Actor
 *********************************************/

/** Function to initalize the HRU for running */
int initHRU(caf::stateful_actor<hru_state>* self);

/** Function runs all of the hru time_steps */
int runHRU(caf::stateful_actor<hru_state>* self);

/** Function checks if the HRU is at a restart checkpoint */
bool isCheckpoint(caf::stateful_actor<hru_state>* self);

void serializeHru(caf::stateful_actor<hru_state>* self, hru& serialized_state);
void deserializeHru(caf::stateful_actor<hru_state>* self, hru& new_state);
