#pragma once

#include "caf/all.hpp"
#include "fortran_data_types.hpp"
#include "timing_info.hpp"

#include <chrono>
#include <string>

namespace caf{
struct gru_state {
    caf::actor file_access_actor;
    caf::actor parent;

    int indxGRU; // index for gru part of derived types in FORTRAN
    int refGRU; // The actual ID of the GRU we are
    int numHRUs;
};
}