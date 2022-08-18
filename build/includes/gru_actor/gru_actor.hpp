#pragma once

#include "caf/all.hpp"
#include "fortran_data_types.hpp"
#include "timing_info.hpp"
#include "global.hpp"
#include <vector>
#include <array>
#include <chrono>
#include <string>

namespace caf{
struct gru_state {
    caf::actor file_access_actor;
    caf::actor parent;
        
    std::vector<caf::actor> hru_list;

    int indx_gru; // index for gru part of derived types in FORTRAN
    int ref_gru; // The actual ID of the GRU we are
    int num_hrus;


    
    int num_bpar_vars;                               // number of variables in the fortran structure for bpar_struct
    std::vector<double> bpar_struct;   
    std::vector<int> bpar_struct_var_type_list;  // The types to the variable at each element in bpar_struct

    int num_bvar_vars;                               // number of variables in the fortran structure for bvar_struct
    std::vector<std::vector<double>> bvar_struct;
    std::vector<int> bvar_struct_var_type_list;

    int num_var_types;
    std::vector<int> i_look_var_type_list; // The types to the variable at each element in bvar_struct
};

behavior gru_actor(stateful_actor<gru_state>* self, int refGRU, int indxGRU,
    std::string configPath,
    int outputStrucSize, caf::actor parent);
}