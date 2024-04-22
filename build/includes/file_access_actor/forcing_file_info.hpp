#pragma once

#include <string>
#include <vector>
#include <iostream>

/**
 * Same file_info from data_types.f90 
 * This is a C++ Representation of the file_info data type
*/
class fileInfo {
  public:
    std::string filenmData;     // name of data file
    int nVars;                  // number of variables in file
    int nTimeSteps;             // number of time steps in file
    std::vector<int> var_ix;    // index of each forcing data variable in the data structure
    std::vector<int> data_id;   // netcdf variable id for each forcing data variable
    std::vector<int> varName;   // netcdf variable name for each forcing data variable
    double firstJulDay;         // first julian day in forcing file
    double convTime2Days;       // conversion factor to convert time units to days
  
    fileInfo(void *handle_forcing_file_info); 
};


class Forcing_File_Info {
    private:
        int file_ID;
        int num_steps;
        bool is_loaded;
    
    public:
        Forcing_File_Info(int file_ID);

        int getNumSteps();

        bool isFileLoaded();

        void updateIsLoaded();

        void updateNumSteps(int num_steps);

};

struct Forcing_Info {
    int num_vars;
    int num_timesteps;
    std::vector<int> index_forc_var;
    std::vector<int> ncid_var;

};