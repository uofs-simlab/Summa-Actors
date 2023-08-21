#pragma once

#include "caf/actor.hpp"
#include "forcing_file_info.hpp"
#include "timing_info.hpp"
#include "output_container.hpp"
#include "settings_functions.hpp"
#include "fortran_data_types.hpp"
#include "auxilary.hpp"
#include "global.hpp"


// class Output_Container;

struct netcdf_gru_actor_info {
    int run_time_var_id;
    int init_duration_var_id;
    int forcing_duration_var_id;
    int run_physics_duration_var_id;
    int write_output_duration_var_id;
    
    int state_var_id; // The success of the GRU 1 = pass, 0 = fail
    int num_attempts_var_id;
    int rel_tol_var_id;
    int abs_tol_var_id;
};


namespace caf {


struct file_access_state {
    // Variables set on Spwan
    caf::actor parent; 
    int start_gru;
    int num_gru;

    netcdf_gru_actor_info gru_actor_stats;

    // std::vector<hru_output_handles> vector_of_output_handles;

    void *handle_forcing_file_info; // Handle for the forcing file information
    void *handle_ncid;              // output file ids
    int num_vectors_in_output_manager;
    int num_steps;
    int stepsInCurrentFile;
    int numFiles;
    int filesLoaded;
    int err;
    int num_output_steps;

    Output_Container* output_container;
    // std::vector<int> failed_gru_index_list;
    

    // Output_Container *output_container;
    std::vector<std::shared_ptr<output_partition>> output_partitions;
    std::vector<std::shared_ptr<output_partition>> output_partitions_for_reruns;


    File_Access_Actor_Settings file_access_actor_settings;

    std::vector<Forcing_File_Info> forcing_file_list; // list of steps in file

    // Variables for handling the inital conditions
    int init_cond_ncid;

    // Variables for hanlding attributes file
    int attribute_ncid;
    int num_var_in_attributes_file;
    std::vector<std::vector<double>> attr_structs_for_hrus;
    std::vector<std::vector<int>> type_structs_for_hrus;
    std::vector<std::vector<long int>> id_structs_for_hrus;

    // Variables for handling parameters file
    int param_ncid;
    std::vector<std::vector<std::vector<double>>> mpar_structs_for_hrus;
    std::vector<std::vector<double>> bpar_structs_for_hrus;
    std::vector<std::vector<double>> dpar_structs_for_hrus;
    
    int dpar_array_size;
    int bpar_array_size;
    int type_array_size;
    bool param_file_exists;
    int num_var_in_param_file;

    // hru_output_handles output_handles;

     // Timing Variables
    TimingInfo file_access_timing;
};

// called to spawn a file_access_actor
behavior file_access_actor(stateful_actor<file_access_state>* self, int startGRU, int numGRU, 
   File_Access_Actor_Settings file_access_actor_settings, actor parent);


void initalizeOutputHandles(stateful_actor<file_access_state>* self);

/* Setup and call the fortran routine that writes the output */
void writeOutput(stateful_actor<file_access_state>* self, Output_Partition* partition);

void deallocateOutputHandles(stateful_actor<file_access_state>* self);
 
} // end namespace