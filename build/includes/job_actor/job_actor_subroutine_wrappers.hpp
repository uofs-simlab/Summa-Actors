#pragma once

extern "C" {
    void job_init_fortran(char const* file_manager,
                          int* start_gru_index, 
                          int* num_gru, 
                          int* num_hru, 
                          int* err);

    void deallocateJobActor(int* err);
    
}