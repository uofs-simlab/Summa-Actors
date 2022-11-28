#pragma once

#include "file_access_actor.hpp"
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

        // returns the matrix of hru_outputs for writing
        std::vector<std::vector<hru_output_handles>> getAllHRUOuptut();


};