#pragma once

// C prototypes for the opaque Fortran handle constructors/destructors used by the Summa-Actors
// C++ layer.  These are defined in build/source/global/cppwrap_datatypes.f90.
//
// This header once declared ~100 get_/set_data_* accessors for serializing per-HRU state to C++
// (the old per-HRU actor design).  That path was replaced by the per-GRU actor model and removed;
// only the handles below still have callers.

extern "C" {
    // var_i -- opaque handle for the per-output-file NetCDF id array (`ncid`),
    // used by the file-access actor and the output buffer.
    void* new_handle_var_i();
    void  delete_handle_var_i(void* handle);

    // gru_type -- opaque per-GRU data handle owned by the GRU actor (gru_actor.cpp).
    void* new_handle_gru_type(int& num_hru);
    void  delete_handle_gru_type(void* handle);
}
