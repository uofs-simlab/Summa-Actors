#pragma once

#include "caf/all.hpp"
#include "fortran_data_types.hpp"
#include "timing_info.hpp"
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

    int num_bpar_vars;                          // number of variables in the fortran structure for bpar_struct
    int num_bvar_vars;                          // number of variables in the fortran structure for bvar_struct
    int n_time_delay = 2000;                    // number of timesteps in the time delay historgram.
    std::vector<double> bpar_struct;             
    std::vector<std::vector<double>> bvar_struct; 
};

behavior gru_actor(stateful_actor<gru_state>* self, int refGRU, int indxGRU,
    std::string configPath,
    int outputStrucSize, caf::actor parent);
}