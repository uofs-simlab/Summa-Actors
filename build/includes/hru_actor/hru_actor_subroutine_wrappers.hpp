#pragma once

extern "C" {
  // Initialize HRU data_structures
    void initHRU(int* indxGRU, int* num_steps, void* hru_data, int* err);
    
    void setupHRUParam( int* indxGRU, int* indxHRU, void* hru_data, double* upArea, int* err);
    
    // Setup summa_readRestart File if this option has been chosen 
    void summa_readRestart(int* indxGRU, int* indxHRU, void* hru_data, double* dtInit, int* err);
    
    // Run the model for one timestep
    void RunPhysics(int* id, int* stepIndex, void* hru_data, double* dt, int* dt_int_factor, int* err);
    
    void hru_writeOutput(int* index_hru, int* index_gru, int* timestep, int* output_step, void* hru_data, int* err);
    
    void setTimeZoneOffset(int* iFile, void* hru_data, int* err);

    void HRU_readForcing(int* index_gru, int* iStep, int* iRead, int* iFile, void* hru_data,  int* err);

    void setIDATolerances(void* hru_data,
                        double* relTolTempCas,
                        double* absTolTempCas,
                        double* relTolTempVeg,
                        double* absTolTempVeg,
                        double* relTolWatVeg,
                        double* absTolWatVeg,
                        double* relTolTempSoilSnow,
                        double* absTolTempSoilSnow,
                        double* relTolWatSnow,
                        double* absTolWatSnow,
                        double* relTolMatric,
                        double* absTolMatric,
                        double* relTolAquifr,
                        double* absTolAquifr);
}