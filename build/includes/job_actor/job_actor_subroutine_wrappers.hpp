#pragma once

extern "C" {
    void initGlobals(char const*str1, int* totalGRUs, int* totalHRUs, 
        int* numGRUs, int* numHRUs, int* startGRUIndex, int* err);

    void setTimesDirsAndFiles(char const* file_manager, int* err);

    void defineGlobalData(int* start_gru_index, int* err);

    void cleanUpJobActor(int* err);
    
}