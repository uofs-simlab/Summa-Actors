#pragma once

#include "caf/all.hpp"
#include "fortran_data_types.hpp"
#include "auxilary.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include <chrono>
#include <string>




namespace caf {
struct hru_state {
	// Actor References
	caf::actor file_access_actor;
	caf::actor parent;

    // Info about which HRU we are and our indexes
    // into global structures in Fortran
    int indxHRU; 	    // index for hru part of derived types in FORTRAN
    int indxGRU; 		// index for gru part of derived types in FORTRAN
    int refGRU;			// The actual ID of the GRU we are

    // Variables for forcing structures
	int stepsInCurrentFFile;        // number of time steps in current forcing file
    int num_steps_until_write;      // number of time steps until we pause for FA_Actor to write

    // HRU data structures (formerly summa_type)
    void *hru_data = new_handle_hru_type();

    // Misc Variables
	int 		timestep = 1;	    // Current Timestep of HRU simulation
    double      dt_init;            // used to initialize the length of the sub-step for each HRU
    double		upArea;             // area upslope of each HRU
    int         num_steps = 0;      // number of time steps
    int         forcingStep;        // index of current time step in current forcing file
    int         iFile;              // index of current forcing file from forcing file list
    int         dt_init_factor = 1; // factor of dt_init (coupled_em)
    bool        printOutput;
    int         outputFrequency;
    int         output_structure_step_index; // index of current time step in output structure


    // Settings
    HRU_Actor_Settings hru_actor_settings;
    // error control
    int         err = 0;			        
    
    ~hru_state() {
        delete_handle_hru_type(hru_data);
    }
};

behavior hru_actor(stateful_actor<hru_state>* self, int refGRU, int indxGRU,
    HRU_Actor_Settings hru_actor_settings, caf::actor file_access_actor, 
    caf::actor parent);

/**
 Function to initalize the HRU for running
 */
void Initialize_HRU(stateful_actor<hru_state>* self);

/**
 Function runs all of the hru time_steps
 */
int Run_HRU(stateful_actor<hru_state>* self);

bool check_HRU(stateful_actor<hru_state>* self, int err);

// Prints the timestep - the frequency of printing can be set by the user
void printOutput(stateful_actor<hru_state>* self);

// Get output from fortran into arrays
// Send the output to the file_access_actor
void getAndSendOutput(stateful_actor<hru_state>* self);

}