#include "caf/all.hpp"
#include "message_atoms.hpp"

// enum class hru_error : uint8_t {
//     run_physics_unhandleable = 1,
//     run_physics_infeasible_state = 2,
// };

std::string to_string(hru_error err) {
    switch(err) {
        case hru_error::run_physics_unhandleable:
            return "run_physics_unhandleable";
        case hru_error::run_physics_infeasible_state:
            return "run_physics_infeasible_state";
        default:
            return "unknown";
    }
}

bool from_string(caf::string_view in, hru_error& out) {
    if (in == "run_physics_unhandleable") {
        out = hru_error::run_physics_unhandleable;
        return true;
    }
    if (in == "run_physics_infeasible_state") {
        out = hru_error::run_physics_infeasible_state;
        return true;
    }
    return false;
}

bool from_integer(uint8_t in, hru_error& out) {
    switch(in) {
        case 1:
            out = hru_error::run_physics_unhandleable;
            return true;
        case 2:
            out = hru_error::run_physics_infeasible_state;
            return true;
        default:
            return false;
    }
}

