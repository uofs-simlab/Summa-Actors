#pragma once
#include "fortran_data_types.hpp"

class HRU {

  private:
    void* hru_data_;
    int indx_hru_;
    int indx_gru_;
    int netcdf_index_;
    int timestep_;
    int forcing_step_;
    int num_steps_; // TODO: May not need this
    
};


