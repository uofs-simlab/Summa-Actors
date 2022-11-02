#pragma once

extern "C" {
    void setTimesDirsAndFiles(char const* file_manager, int* err);

    void defineGlobalData(int* start_gru_index, int* err);

    void readDimension(int* num_gru, int* num_hru, int* start_gru_index, int* err);

    void readIcondNLayers(int* num_gru, int* err);

    void allocateTimeStructure(int* err);

    void deallocateJobActor(int* err);
    
}