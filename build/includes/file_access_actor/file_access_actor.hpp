#pragma once

#include "caf/all.hpp"
#include "output_manager.hpp"
#include "forcing_file_info.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include "fortran_data_types.hpp"
#include "auxilary.hpp"

namespace caf {


struct hru_output_handles {
    // Statistic Structures
    void* handle_forc_stat        = new_handle_var_dlength();
    void* handle_prog_stat        = new_handle_var_dlength();
    void* handle_diag_stat        = new_handle_var_dlength();
    void* handle_flux_stat        = new_handle_var_dlength();
    void* handle_indx_stat        = new_handle_var_dlength();
    void* handle_bvar_stat        = new_handle_var_dlength();
    // primary data structures (scalars)
    void* handle_time_struct      = new_handle_var_i();
    void* handle_forc_struct      = new_handle_var_d();
    void* handle_attr_struct      = new_handle_var_d();
    void* handle_type_struct      = new_handle_var_i();
    void* handle_id_struct        = new_handle_var_i8();
    // primary data structures (variable length vectors)
    void* handle_indx_struct      = new_handle_var_ilength();
    void* handle_mpar_struct      = new_handle_var_dlength();
    void* handle_prog_struct      = new_handle_var_dlength();
    void* handle_diag_struct      = new_handle_var_dlength();
    void* handle_flux_struct      = new_handle_var_dlength();
    // basin-average structures
    void* handle_bpar_struct      = new_handle_var_d();
    void* handle_bvar_struct      = new_handle_var_dlength();
    // ancillary data structures
    void* handle_dpar_struct      = new_handle_var_d();
    void* handle_finalize_stats   = new_handle_var_i();
    void* handle_output_timestep = new_handle_var_i();
};

struct file_access_state {
    // Variables set on Spwan
    caf::actor parent; 
    int start_gru;
    int num_gru;


    void *handle_forcing_file_info; // Handle for the forcing file information
    void *handle_ncid;              // output file ids
    OutputManager *output_manager;
    int num_vectors_in_output_manager;
    int num_steps;
    int stepsInCurrentFile;
    int numFiles;
    int filesLoaded;
    int err;

    File_Access_Actor_Settings file_access_actor_settings;

    std::vector<Forcing_File_Info> forcing_file_list; // list of steps in file
    std::vector<bool> outputFileInitHRU;

    // Variables for hanlding attributes file
    int attribute_ncid;
    int num_var_in_attributes_file;
    std::vector<void*> attr_structs_for_hrus;
    std::vector<void*> type_structs_for_hrus;
    std::vector<void*> id_structs_for_hrus;

    // Variables for handling parameters file
    std::vector<void*> mpar_structs_for_hrus;
    std::vector<void*> bpar_structs_for_hrus;
    std::vector<void*> dpar_structs_for_hrus;
    
    int dpar_array_size;
    int bpar_array_size;
    int type_array_size;
    bool param_file_exists;
    int num_var_in_param_file;
    int param_ncid;

    hru_output_handles output_handles;

     // Timing Variables
    TimingInfo file_access_timing;
};

behavior file_access_actor(stateful_actor<file_access_state>* self, int startGRU, int numGRU, 
   File_Access_Actor_Settings file_access_actor_settings, actor parent);

void initalizeFileAccessActor(stateful_actor<file_access_state>* self);
int writeOutput(stateful_actor<file_access_state>* self, int indxGRU, int indxHRU, int numStepsToWrite, int returnMessage, caf::actor actorRef);
int readForcing(stateful_actor<file_access_state>* self, int currentFile);
int write(stateful_actor<file_access_state>* self, int listIndex);

// Read in the attributes for all HRUs that are in the run-domain
void readAttributes(stateful_actor<file_access_state>* self); 

// read in the parameters for all HRUs that are in the run-domain
void readParameters(stateful_actor<file_access_state>* self);

} // end namespace