#pragma once

#include "caf/all.hpp"
#include "fortran_data_types.hpp"
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
    int outputStrucSize;
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

    
    // Local hru data
    void *handle_ncid = new_handle_var_i();             // output file ids
    void *handle_statCounter = new_handle_var_i();
    void *handle_outputTimeStep = new_handle_var_i();
    void *handle_resetStats = new_handle_flagVec();
    void *handle_finalizeStats = new_handle_flagVec();
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


    int         err = 0;			            // error conotrol

    TimingInfo hru_timing;

};

behavior hru_actor(stateful_actor<hru_state>* self, int refGRU, int indxGRU,
    HRU_Actor_Settings hru_actor_settings, caf::actor file_access_actor, 
    int outputStrucSize, caf::actor parent);

/**
 Function to initalize the HRU for running
 */
void Initialize_HRU(stateful_actor<hru_state>* self);

/**
 Function runs all of the hru time_steps
 */
int Run_HRU(stateful_actor<hru_state>* self);

bool check_HRU(stateful_actor<hru_state>* self, int err);

void initalizeTimeVars(stateful_actor<hru_state>* self);

void finalizeTimeVars(stateful_actor<hru_state>* self);

void deallocateHRUStructures(stateful_actor<hru_state>* self);

void printOutput(stateful_actor<hru_state>* self);


}