#pragma once

extern "C" {
  // Initialize HRU data_structures
	void initHRU(
            int* indxGRU, int* num_steps, void* lookupStruct,
            // Statistics Structures
            void* forcStat, void* progStat, void* diagStat, void* fluxStat, void* indxStat, void* bvarStat,
            // Primary Data Structures (scalars) 
            void* timeStruct, void* forcStruct,
            // primary data structures (variable length vectors)
            void* indxStruct, void* progStruct, void* diagStruct, void* fluxStruct,
            // basin-average structures
            void* bvarStruct,
            // local HRU data 
            void* startTime, void* finshTime, void* refTime, void* oldTime, int* err);

      // Initalize statistics flags
      void initStatisticsFlags(void* handle_statCounter, void* handle_outputTimeStep, void* handle_resetStats,
            void* handle_finalizeStats, int* err);
    
      void setupHRUParam( int* indxGRU, int* indxHRU,
            // primary data structures (scalars)
            void* attrStruct, void* typeStruct, void* idStruct,
            // primary data structures (variable length vectors)
            void* mparStruct, void* bparStruct, void* bvarStruct, void* dparStruct,
            // lookup tables
            void* lookupStruct,
            // local HRU data
            void* startTime, void* oldTime,
            // miscellaneous
            double* upArea, int* err);

  
  // Setup summa_readRestart File if this option has been chosen 
	void summa_readRestart(
            int* indxGRU, int* indxHRU,
            // primary data structures (variable length vectors) 
            void* indxStruct, void* mparStruct, void* progStruct, void* diagStruct, void* fluxStruct, 
            // basin-average structures
            void* bvarStruct,
            // misc 
            double* dtInit, int* err);


      // Run the model for one timestep
      void  RunPhysics(
            int* id, int* stepIndex,
            // primary data structures (scalars)
            void* timeStruct, void* forcStruct, void* attrStruct, void* typeStruct,
            // primary data structures (variable length vectors) 
            void* indxStruct, void* mparStruct, void* progStruct, void* diagStruct, void* fluxStruct,
            // basin-average structures 
            void* bvarStruct,
            void* lookupStruct,
            double* fracJulDay, double* tmZoneOffsetFracDay, int* yearLength,
            // misc
            int* flag, double* dt, int* dt_int_factor, int* err);

      void writeHRUToOutputStructure(int* index_hru, int* index_gru, int* output_step, void* handle_forcStat,
            void* handle_progStat, void* handle_diagStat, void* handle_fluxStat, void* handle_indxStat,
            void* handle_bvarStat, void* handle_timeStruct, void* handle_forcStruct, void* handle_indxStruct,
            void* handle_mparStruct, void* handle_progStruct, void* handle_diagStruct, void* handle_fluxStruct,
            void* handle_bparStruct, void* handle_bvarStruct, void* handle_statCounter, void* handle_outputTimeStep,
            void* handle_resetStats, void* handle_finalizeStats, void* handle_finshTime, void* handle_oldTime, int* err);


      void prepareOutput(int* model_timestep, void* handle_force_stat, void* handle_prog_stat, 
            void* handle_diag_stat, void* handle_flux_stat, void* handle_indx_stat,
            void* handle_bvar_stat, void* handle_time_struct, void* handle_forc_struct,
            void* handle_attr_struct, void* handle_type_struct, void* handle_indx_struct,
            void* handle_mpar_struct, void* handle_prog_struct, void* handle_diag_struct,
            void* handle_flux_struct, void* handle_bpar_struct, void* handle_bvar_struct,
            void* stat_counter, void* output_timestep, void* reset_stats, void* finalize_stats,
            void* finish_time, void* old_time, int* err);
      
      void updateCounters(void* handle_timeStruct, void*  handle_statCounter, void* handle_outputTimeStep,
            void* handle_resetStats, void* handle_oldTime, void* handle_finalizeStats);

      
      void setTimeZoneOffset(int* iFile, double* tmZoneOffsetFracDay, int* err);

      void getFirstTimestep(int* iFile, int* forcing_step, int* err);

      void readForcingHRU(int* index_gru, int* iStep, int* iRead, void* handle_timeStruct, 
            void* handle_forcStruct, int* iFile, int* err);

      void computeTimeForcingHRU(void* handle_timeStruct, void* handle_forcStruct, double* fracJulDay, 
                              int* yearLength, int* err);

      void setIDATolerances(void* handle_mparStruct, 
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