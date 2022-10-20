#pragma once

extern "C" {
  // Initialize HRU data_structures
	void initHRU(
        int* indxGRU, int* num_steps, void* lookupStruct,
        // Statistics Structures
        void* forcStat, void* progStat, void* diagStat, void* fluxStat, void* indxStat, void* bvarStat,
        // Primary Data Structures (scalars) 
        void* timeStruct, void* forcStruct, void* attrStruct, void* typeStruct, void* idStruct, 
        // primary data structures (variable length vectors)
        void* indxStruct, void* mparStruct, void* progStruct, void* diagStruct, void* fluxStruct,
        // basin-average structures
        void* bparStruct, void* bvarStruct,
        // ancillary data structures
        void* dparStruct,
        // local HRU data 
        void* startTime, void* finshTime, void* refTime, void* oldTime, int* err);
    
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

  
  // Setup Restart File if this option has been chosen 
	void Restart(
        int* indxGRU, int* indxHRU,
        // primary data structures (variable length vectors) 
        void* indxStruct, void* mparStruct, void* progStruct, void* diagStruct, void* fluxStruct, 
        // basin-average structures
        void* bvarStruct,
        // misc 
        double* dtInit, int* err);
	
  // Read Forcing for HRU 
  void  Forcing( 
      int* indxGRU, int* stepIndex, 
      void* timeStruct, void* forcStruct, 
      int* iFile, int* forcingStep, 
      double* fracJulDay, double* tmZoneOffsetFracDay, int* yearLength,
      int* err);

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

  // Write output to the output structure
  void WriteOutput(
    int* indHRU, int* indxGRU, int* indexStep,
    // statistics structures
    void* forcStat, void* progStat, void* diagStat, void* fluxStat, void* indxStat, void* bvarStat,
    // primary data structures (scalars)
    void* timeStruct, void* forcStruct, void* attrStruct, void* typeStruct,
    // primary data structures (variable length vectors)
    void* indxStruct, void* mparStruct, void* progStruct, void* diagStruct, void* fluxStruct, 
    // basin-average structures
    void* bparStruct, void* bvarStruct, 
    // local vars
    void* statCounter, void* outputTimeStep, void* resetStats, void* finalizeStats,
    void* finshTime, void* oldTime, int* outputStep, int* err);

  void DeallocateStructures(
        void* handle_forcStat, void* handle_progStat, void* handle_diagStat, void* handle_fluxStat,
        void* handle_indxStat, void* handle_bvarStat, void* handle_timeStruct, void* handle_forcStruct,
        void* handle_attrStruct, void* handle_typeStruct, void* handle_idStruct, void* handle_indxStruct,
        void* handle_mparStruct, void* handle_progStruct, void* handle_diagStruct, void* handle_fluxStruct,
        void* handle_bparStruct, void* handle_bvarStruct, void* handle_dparStruct,
        void* handle_startTime, void* handle_finishTime,
        void* handle_refTime, void* handle_oldTime,
        void* handle_ncid,
        void* handle_statCounter,
        void* handle_outputTimeStep,
        void* handle_resetStats,
        void* handle_finalizeStats,
        int* err);
  
  void Write_Param_C(
        int* indxGRU, int* indxHRU,
        void* handle_attrStruct, void* handle_typeStruct, void* handle_mparStruct, void* handle_bparStruct,
        int* err);

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
}