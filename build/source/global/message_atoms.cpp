#include "caf/all.hpp"
#include "message_atoms.hpp"


// File Access Error
std::string to_string(file_access_error err) {
    switch(err) {
        case file_access_error::writing_error:
            return "writing_error";
        default:
            return "unknown";
    }
}

bool from_string(caf::string_view in, file_access_error& out) {
    if (in == "writing_error") {
        out = file_access_error::writing_error;
        return true;
    }
    return false;
}

bool from_integer(uint8_t in, file_access_error& out) {
    switch(in) {
        case 1:
            out = file_access_error::writing_error;
            return true;
        default:
            return false;
    }
}