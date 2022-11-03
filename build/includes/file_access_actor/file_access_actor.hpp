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
    void* handle_output_timestep  = new_handle_var_i();

    // Destructor
    ~hru_output_handles(){
        // Statistic Structures
        delete_handle_var_dlength(handle_forc_stat);
        delete_handle_var_dlength(handle_prog_stat);
        delete_handle_var_dlength(handle_diag_stat);
        delete_handle_var_dlength(handle_flux_stat);
        delete_handle_var_dlength(handle_indx_stat);
        delete_handle_var_dlength(handle_bvar_stat);
        // primary data structures (scalars)
        delete_handle_var_i(handle_time_struct);
        delete_handle_var_d(handle_forc_struct);
        delete_handle_var_d(handle_attr_struct);
        delete_handle_var_i(handle_type_struct);
        delete_handle_var_i8(handle_id_struct);
        // primary data structures (variable length vectors)
        delete_handle_var_ilength(handle_indx_struct);
        delete_handle_var_dlength(handle_mpar_struct);
        delete_handle_var_dlength(handle_prog_struct);
        delete_handle_var_dlength(handle_diag_struct);
        delete_handle_var_dlength(handle_flux_struct);
        // basin-average structures
        delete_handle_var_d(handle_bpar_struct);
        delete_handle_var_dlength(handle_bvar_struct);
        // ancillary data structures
        delete_handle_var_d(handle_dpar_struct);
        delete_handle_var_i(handle_finalize_stats);
        delete_handle_var_i(handle_output_timestep);
    }

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
    std::vector<std::vector<double>> attr_structs_for_hrus;
    std::vector<std::vector<int>> type_structs_for_hrus;
    std::vector<std::vector<long int>> id_structs_for_hrus;

    // Variables for handling parameters file
    std::vector<std::vector<std::vector<double>>> mpar_structs_for_hrus;
    std::vector<std::vector<double>> bpar_structs_for_hrus;
    std::vector<std::vector<double>> dpar_structs_for_hrus;
    
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

// called to spawn a file_access_actor
behavior file_access_actor(stateful_actor<file_access_state>* self, int startGRU, int numGRU, 
   File_Access_Actor_Settings file_access_actor_settings, actor parent);

// Call Fortran functions that require file access and intialize the ffile_info structure
void initalizeFileAccessActor(stateful_actor<file_access_state>* self);

// Read in the attributes for all HRUs that are in the run-domain
void readAttributes(stateful_actor<file_access_state>* self); 

// read in the parameters for all HRUs that are in the run-domain
void readParameters(stateful_actor<file_access_state>* self);

// deallocate all dynamically allocated structures
void cleanup(stateful_actor<file_access_state>* self);

} // end namespace