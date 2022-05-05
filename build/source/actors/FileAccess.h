#ifndef FILEACCESS_H_
#define FILEACCESS_H_

#include "../interface/fortran_dataTypes.h"
#include "../interface/file_access_actor/fileAccess_subroutine_wrappers.h"
#include "caf/all.hpp"
#include "messageAtoms.h"
#include "OutputManager.h"
#include <vector>
#include <chrono>
#include "global.h"

class forcingFile {
    private:
        int fileID; // which file are we relative the forcing file list saved in fortran
        int numSteps; // the number of steps in this forcing file
        bool isLoaded; // is this file actually loaded in to RAM yet.
    public:
        forcingFile(int fileID) {
            this->fileID = fileID;
            this->numSteps = 0;
            this->isLoaded = false;
        }

        int getNumSteps() {
            return this->numSteps;
        }

        bool isFileLoaded() {
            return this->isLoaded;
        }

        void updateIsLoaded() {
            this->isLoaded = true;
        }

        void updateNumSteps(int numSteps) {
            this->numSteps = numSteps;
            this->isLoaded = true;
        }
};

struct file_access_state {
    // Variables set on Spwan
    caf::actor parent; 
    int startGRU;
    int numGRU;


    void *handle_forcFileInfo = new_handle_file_info(); // Handle for the forcing file information
    void *handle_ncid = new_handle_var_i();               // output file ids
    OutputManager *output_manager;
    int num_vectors_in_output_manager = 5;
    int num_steps;
    int outputStrucSize;
    int stepsInCurrentFile;
    int numFiles;
    int filesLoaded;
    int err = 0;

    std::vector<forcingFile> forcFileList; // list of steps in file
    std::vector<bool> outputFileInitHRU;

    std::chrono::time_point<std::chrono::system_clock> readStart;
    std::chrono::time_point<std::chrono::system_clock> readEnd;
    double readDuration = 0.0;

    std::chrono::time_point<std::chrono::system_clock> writeStart;
    std::chrono::time_point<std::chrono::system_clock> writeEnd;
    double writeDuration = 0.0;


};








#endif