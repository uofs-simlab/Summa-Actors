#pragma once

#include "caf/actor.hpp"
#include "fortran_data_types.hpp"
#include <vector>
#include <iostream>


struct hru_output_handles {
    caf::actor hru_actor;
    int index_hru;
    int index_gru;
    // Statistic Structures
    void* handle_forc_stat        = new_handle_var_dlength();
    void* handle_prog_stat        = new_handle_var_dlength();
    void* handle_diag_stat        = new_handle_var_dlength();
    void* handle_flux_stat        = new_handle_var_dlength();
    void* handle_indx_stat        = new_handle_var_dlength();
    void* handle_bvar_stat        = new_handle_var_dlength();
    // primary data structures (scalars)
    void* handle_time_struct      = new_handle_var_i();
    void* handle_forc_struct      = new_handle_var_d();
    void* handle_attr_struct      = new_handle_var_d();
    void* handle_type_struct      = new_handle_var_i();
    void* handle_id_struct        = new_handle_var_i8();
    // primary data structures (variable length vectors)
    void* handle_indx_struct      = new_handle_var_ilength();
    void* handle_mpar_struct      = new_handle_var_dlength();
    void* handle_prog_struct      = new_handle_var_dlength();
    void* handle_diag_struct      = new_handle_var_dlength();
    void* handle_flux_struct      = new_handle_var_dlength();
    // basin-average structures
    void* handle_bpar_struct      = new_handle_var_d();
    void* handle_bvar_struct      = new_handle_var_dlength();
    // ancillary data structures
    void* handle_dpar_struct      = new_handle_var_d();
    void* handle_finalize_stats   = new_handle_var_i();
    void* handle_output_timestep  = new_handle_var_i();

};

// This class holds the output for the HRUs as a buffer so 
// we can write more data at once
class Output_Container {
    private:
        // Matrix charactieristics
        int max_steps; // maximum number of steps we can hold for an HRU before writing
        int max_hrus; // maximum number of hrus we can hold for the structure

        std::vector<std::vector<hru_output_handles>> hru_output_handles_vector; // Pointers to HRU output data

    public:
        Output_Container(int max_hrus, int max_steps);
        ~Output_Container();

        // insertes output from an HRU into hru_output_handles
        void insertOutput(int hru_index, hru_output_handles hru_output);

        bool isFull(int hru_index);

        // returns the matrix of hru_outputs for writing
        std::vector<std::vector<hru_output_handles>> getAllHRUOutput();

        void clearAll();


};