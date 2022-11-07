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

    // Variables for output/forcing structures
	int outputStep;
	int stepsInCurrentFFile;
	int forcingFileStep;
	int currentForcingFile = 1;


    // statistics structures
    void *handle_forcStat = new_handle_var_dlength();	// model forcing data	
    void *handle_progStat = new_handle_var_dlength();	// model prognostic (state) variables                   
    void *handle_diagStat = new_handle_var_dlength();	// model diagnostic variables                  
    void *handle_fluxStat = new_handle_var_dlength();	// model fluxes                 
    void *handle_indxStat = new_handle_var_dlength();	// model indices            
    void *handle_bvarStat = new_handle_var_dlength();	// basin-average variables  
    // primary data structures (scalars)
    void *handle_timeStruct = new_handle_var_i();       // model time data
    void *handle_forcStruct = new_handle_var_d();       // model forcing data
    void *handle_attrStruct = new_handle_var_d();       // local attributes for each HRU
    void *handle_typeStruct = new_handle_var_i();       // local classification of soil veg etc. for each HRU
    void *handle_idStruct   = new_handle_var_i8();		
    // primary data structures (variable length vectors)
    void *handle_indxStruct = new_handle_var_ilength();	// model indices
    void *handle_mparStruct = new_handle_var_dlength();	// model parameters
    void *handle_progStruct = new_handle_var_dlength();	// model prognostic (state) variables
    void *handle_diagStruct = new_handle_var_dlength();	// model diagnostic variables
    void *handle_fluxStruct = new_handle_var_dlength();	// model fluxes
	// basin-average structures
    void *handle_bparStruct = new_handle_var_d();		// basin-average parameters
    void *handle_bvarStruct = new_handle_var_dlength(); // basin-average variables
    // ancillary data structures
    void *handle_dparStruct = new_handle_var_d();		// default model parameters
    // sundials type
    void *handle_lookupStruct = new_handle_z_lookup();    
    // Counter variables
    void *handle_statCounter = new_handle_var_i();
    void *handle_outputTimeStep = new_handle_var_i();
    void *handle_resetStats = new_handle_flagVec();
    void *handle_finalizeStats = new_handle_flagVec();
    // Time variables
    void *handle_oldTime = new_handle_var_i();
    void *handle_refTime = new_handle_var_i();
    void *handle_finshTime = new_handle_var_i();
    void *handle_startTime = new_handle_var_i();
    // Misc Variables
	int 		timestep = 1;	    // Current Timestep of HRU simulation
    int         computeVegFlux;     // flag to indicate if we are computing fluxes over vegetation
    double      dt_init;            // used to initialize the length of the sub-step for each HRU
    double		upArea;             // area upslope of each HRU
    int         num_steps = 0;      // number of time steps
    int         forcingStep;    // index of current time step in current forcing file
    int         iFile;          // index of current forcing file from forcing file list
    int         dt_init_factor = 1; // factor of dt_init (coupled_em)
    bool        printOutput;
    int         outputFrequency;

    // Julian Day variables
    double      fracJulDay;
    double      tmZoneOffsetFracDay;
    int         yearLength;

    // Settings
    HRU_Actor_Settings hru_actor_settings;
    // error control
    int         err = 0;			        
    TimingInfo hru_timing;
    
    ~hru_state() {
        // statistics structures
        delete_handle_var_dlength(handle_forcStat);
        delete_handle_var_dlength(handle_progStat);
        delete_handle_var_dlength(handle_diagStat);
        delete_handle_var_dlength(handle_fluxStat);
        delete_handle_var_dlength(handle_indxStat);
        delete_handle_var_dlength(handle_bvarStat);
        // primary data structures (scalars)
        delete_handle_var_i(handle_timeStruct);
        delete_handle_var_d(handle_forcStruct);
        delete_handle_var_d(handle_attrStruct);
        delete_handle_var_i(handle_typeStruct);
        delete_handle_var_i8(handle_idStruct);
        // primary data structures (variable length vectors)
        delete_handle_var_ilength(handle_indxStruct);
        delete_handle_var_dlength(handle_mparStruct);
        delete_handle_var_dlength(handle_progStruct);
        delete_handle_var_dlength(handle_diagStruct);
        delete_handle_var_dlength(handle_fluxStruct);
        // basin-average structures
        delete_handle_var_d(handle_bparStruct);
        delete_handle_var_dlength(handle_bvarStruct);
        // ancillary data structures
        delete_handle_var_d(handle_dparStruct);
        // sundials type
        delete_handle_z_lookup(handle_lookupStruct);
        // counter variables
        delete_handle_var_i(handle_statCounter);
        delete_handle_var_i(handle_outputTimeStep);
        delete_handle_flagVec(handle_resetStats);
        delete_handle_flagVec(handle_finalizeStats);
        // time variables
        delete_handle_var_i(handle_oldTime);
        delete_handle_var_i(handle_refTime);
        delete_handle_var_i(handle_finshTime);
        delete_handle_var_i(handle_startTime);


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