#pragma once

#include "caf/actor.hpp"
#include "forcing_file_info.hpp"
#include "timing_info.hpp"
#include "output_container.hpp"
#include "settings_functions.hpp"
#include "fortran_data_types.hpp"
#include "auxilary.hpp"


// class Output_Container;

namespace caf {


struct file_access_state {
    // Variables set on Spwan
    caf::actor parent; 
    int start_gru;
    int num_gru;

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

    // Output_Container *output_container;
    std::vector<std::shared_ptr<output_partition>> output_partitions;



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

// Call Fortran functions that require file access and intialize the ffile_info structure
void initalizeFileAccessActor(stateful_actor<file_access_state>* self);

// Read in the attributes for all HRUs that are in the run-domain
void readAttributes(stateful_actor<file_access_state>* self); 

// read in the parameters for all HRUs that are in the run-domain
void readParameters(stateful_actor<file_access_state>* self);

// Read in the inital conditions for all the HRUs that are in the run-domain
void readInitConditions(stateful_actor<file_access_state>* self);

void initalizeOutputHandles(stateful_actor<file_access_state>* self);

void deallocateOutputHandles(stateful_actor<file_access_state>* self);
 
} // end namespace