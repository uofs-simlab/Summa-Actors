#pragma once

extern "C" {
  // Initialize HRU data_structures
	void initHRU(int* indxGRU, int* num_steps, void* hru_data, int* err);

      // Initalize statistics flags
      void initStatisticsFlags(void* handle_statCounter, void* handle_outputTimeStep, void* handle_resetStats,
            void* handle_finalizeStats, int* err);
    
      void setupHRUParam( int* indxGRU, int* indxHRU, void* hru_data, double* upArea, int* err);

  
      // Setup summa_readRestart File if this option has been chosen 
	void summa_readRestart(int* indxGRU, int* indxHRU, void* hru_data, double* dtInit, int* err);


      // Run the model for one timestep
      void  RunPhysics(int* id, int* stepIndex, void* hru_data, double* fracJulDay, double* tmZoneOffsetFracDay, 
                       int* yearLength, int* flag, double* dt, int* dt_int_factor, int* err);

      void writeHRUToOutputStructure(int* index_hru, int* index_gru, int* output_step, void* hru_data, 
      void* handle_statCounter, void* handle_outputTimeStep, void* handle_resetStats, void* handle_finalizeStats,
      int* err);
      
      
      void setTimeZoneOffset(int* iFile, double* tmZoneOffsetFracDay, int* err);

      void getFirstTimestep(int* iFile, int* forcing_step, int* err);

      void readForcingHRU(int* index_gru, int* iStep, int* iRead, 
            void* hru_data, int* iFile, int* err);

      void computeTimeForcingHRU(void* hru_data, double* fracJulDay, int* yearLength, int* err);

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