#pragma once

#include "caf/all.hpp"
#include "fortran_data_types.hpp"
#include "timing_info.hpp"
#include "global.hpp"
#include <vector>
#include <array>
#include <chrono>
#include <string>
#include "var_lookup.hpp"

namespace caf{
struct gru_state {
    // GRU Initalization
    int ref_gru;
    int indx_gru;
    std::string config_path;
    caf::actor file_access_actor;
    int output_struc_size;
    caf::actor parent;
        
    // HRU information
    std::vector<caf::actor> hru_list;
    int num_hrus;
    int hrus_complete = 0;

    // Global Data
    int nTimeDelay = 2000; // number of hours in the time delay histogram (default: ~1 season = 24*365/4)
    struct iLookVarType var_type_lookup;
    
    // Data Structure local to the GRU
    int num_bpar_vars;                               // number of variables in the fortran structure for bpar_struct
    std::vector<double> bpar_struct;   
    std::vector<int> bpar_struct_var_type_list;  // The types to the variable at each element in bpar_struct

    int num_bvar_vars;                               // number of variables in the fortran structure for bvar_struct
    std::vector<std::vector<double>> bvar_struct;
    std::vector<int> bvar_struct_var_type_list;

    int num_var_types;
    std::vector<int> i_look_var_type_list; // The types to the variable at each element in bvar_struct
};

behavior gru_actor(stateful_actor<gru_state>* self, 
    int ref_gru, 
    int indx_gru, 
    std::string config_path, 
    caf::actor file_access_actor, 
    caf::actor parent);
}