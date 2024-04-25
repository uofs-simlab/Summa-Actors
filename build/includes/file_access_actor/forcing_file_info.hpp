#pragma once

#include <string>
#include <vector>
#include <iostream>
#include "timing_info.hpp"

extern "C" {
  // Create the fortran ffile_info and return the number of forcing_files
  void getNumFrocingFiles_fortran(int& num_forcing_files);
  // Creation and population of Fortran structures
  void getFileInfoSizes_fortran(int& iFile, int& var_ix_size, int& data_id_size, 
      int& varName_size);
  void getFileInfoCopy_fortran(int& iFile, void* file_name, int& nVars, 
      int& nTimeSteps, int& var_name_size, int& var_ix_size, int& data_id_size, 
      void* var_name_arr, void* var_ix_arr, void* data_id_arr, 
      double& firstJulDay, double& convTime2Days);

  // File Loading
  void read_forcingFile(int& iFile, int& start_gru, int& num_gru, 
      int& err, void* message);

  // Deallocate Fortran Structures associate with forcing files
  void freeForcingFiles_fortran();
}

/**
 * Same file_info from data_types.f90 
 * This is a C++ Representation of the file_info data type
*/
class fileInfo {
  public:
    /** Fortran Replication Part **/
    std::string filenmData;     // name of data file
    int nVars;                  // number of variables in file
    int nTimeSteps;             // number of time steps in file
    std::vector<int> var_ix;    // index of each forcing data variable in the data structure
    std::vector<int> data_id;   // netcdf variable id for each forcing data variable
    std::vector<std::string> varName;   // netcdf variable name for each forcing data variable
    double firstJulDay;         // first julian day in forcing file
    double convTime2Days;       // conversion factor to convert time units to days
    /** Fortran Replication Part **/

    /** C++ Part **/
    bool is_loaded = false;

    fileInfo(); 
};

class forcingFileContainer {
  public:
    std::vector<fileInfo> forcing_files_;
    forcingFileContainer();
    ~forcingFileContainer();

    int initForcingFiles();

    int loadForcingFile(int file_ID, int start_gru, int num_gru);
    bool isFileLoaded(int file_ID);
    inline bool allFilesLoaded() { 
      return files_loaded_ == forcing_files_.size(); 
    }
    inline int getNumSteps(int iFile) {
      return forcing_files_[iFile-1].nTimeSteps;
    }

    inline double getInitDuration() {
      return file_access_timing_.getDuration("init_duration").value_or(-1.0);
    }

    inline double getReadDuration() {
      return file_access_timing_.getDuration("read_duration").value_or(-1.0);
    }

  private:
    int files_loaded_ = 0;
    TimingInfo file_access_timing_ = TimingInfo();
};
