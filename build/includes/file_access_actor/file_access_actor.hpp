#pragma once

#include "caf/all.hpp"
#include "output_manager.hpp"
#include "forcing_file_info.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"

namespace caf {
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