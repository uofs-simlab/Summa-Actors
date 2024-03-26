#include "hru_actor.hpp"

bool hru_extra_logging = false;

namespace caf {

behavior hru_actor(stateful_actor<hru_state>* self, int refGRU, int indxGRU,
                   HRU_Actor_Settings hru_actor_settings, 
                   caf::actor file_access_actor, caf::actor parent) {
  
  // Actor References
  self->state.file_access_actor = file_access_actor;
  self->state.parent            = parent;
  // Indexes into global structures
  self->state.indxHRU           = 1;
  self->state.indxGRU           = indxGRU;
  self->state.refGRU            = refGRU;
  // Get the settings for the HRU
  self->state.hru_actor_settings = hru_actor_settings;
  self->state.dt_init_factor = hru_actor_settings.dt_init_factor;

  // Initialize HRU data and statistics structures
  initHRU(&self->state.indxGRU, &self->state.num_steps, self->state.hru_data, 
      &self->state.err);
  if (self->state.err != 0) {
      aout(self) << "Error: HRU_Actor - Initialize - HRU = " << self->state.indxHRU  
                  << " - indxGRU = " << self->state.indxGRU 
                  << " - refGRU = "<< self->state.refGRU
                  << "\nError Code = " << self->state.err << "\n";
      self->quit();
  }
  Initialize_HRU(self);


  return {
    /* Job actor telling us to run until completions
        We will interact with the file access actor */
    [=](update_hru_async) {
      self->request(self->state.file_access_actor, caf::infinite,
                    get_num_output_steps_v).await([=](int num_steps){
        self->state.num_steps_until_write = num_steps;
        self->send(self->state.file_access_actor, access_forcing_v, 
                    self->state.iFile, self);
      });
    },

    [=](num_steps_before_write, int num_steps) {
        self->state.num_steps_until_write = num_steps;
        self->state.output_structure_step_index = 1;
    },

    // Run HRU for a number of timesteps
    [=](run_hru) {
      int err = 0;

      while(self->state.num_steps_until_write > 0) {
        if (self->state.forcingStep > self->state.stepsInCurrentFFile) {
            self->send(self->state.file_access_actor, access_forcing_v, self->state.iFile+1, self);
            break;
        }

        self->state.num_steps_until_write--;
      if (self->state.timestep == 5) {
        self->send(self, serialize_hru_v);
      } 

        err = Run_HRU(self); // Simulate a Timestep
        if (err != 0) {
          #ifdef SUNDIALS_ACTIVE                        
            get_sundials_tolerances(self->state.hru_data, 
                &self->state.rtol, &self->state.atol);
            self->send(self->state.parent, err_atom_v, self, self->state.rtol,
                self->state.atol);
          #else                        
            self->send(self->state.parent, 
                hru_error::run_physics_unhandleable, self);
          #endif
          self->quit();
          return;
        }

        self->state.timestep++;
        self->state.forcingStep++;
        self->state.output_structure_step_index++;
        
        if (self->state.timestep > self->state.num_steps) {
            self->send(self, done_hru_v);
            break;
        }

      }
      // Our output structure is full
      if (self->state.num_steps_until_write <= 0) {
          self->send(self->state.file_access_actor, write_output_v, 
          self->state.indxGRU, self->state.indxHRU, self);
      }
    },


    [=](new_forcing_file, int num_forcing_steps_in_iFile, int iFile) {
        int err;
        self->state.iFile = iFile;
        self->state.stepsInCurrentFFile = num_forcing_steps_in_iFile;
        setTimeZoneOffset(&self->state.iFile, self->state.hru_data, &err);
        self->state.forcingStep = 1;
        self->send(self, run_hru_v);
    },

    
    [=](done_hru) {
        self->send(self->state.parent,done_hru_v,self->state.indxGRU);
        self->quit();
        return;
    },

    [=](dt_init_factor, int dt_init_factor) {
        aout(self) << "Recieved New dt_init_factor to attempt on next run \n";
    },

    [=](update_timeZoneOffset, int iFile) {
        if (hru_extra_logging)
            aout(self) << "Recieved New iFile-" << iFile 
                       << " to update timeZoneOffset \n";
        int err;
        self->state.iFile = iFile;
        setTimeZoneOffset(&iFile, self->state.hru_data, &err);
    },

    // BMI - Functions
    [=](update_hru, int timestep, int forcingstep) {
      if (hru_extra_logging) {
        aout(self) << "Computing Time Step: " << timestep 
                   << " Forcing Step: " << forcingstep << "\n";
      }

      self->state.output_structure_step_index = 1;
      self->state.timestep = timestep;
      self->state.forcingStep = forcingstep;

      int err = Run_HRU(self);
      if (err != 0) {
        self->send(self->state.parent, hru_error::run_physics_unhandleable, 
        self);
        self->quit();
        return;
      }
      self->send(self->state.parent, done_update_v);
    },

    // Get Fortran Data into C++
    [=](serialize_hru) {

      // std::vector<std::vector<std::vector<double>>> lookup_struct = 
          // get_lookup_struct(self->state.hru_data);

      self->state.hru_data_serialized.indx_hru = self->state.indxHRU;
      self->state.hru_data_serialized.indx_gru = self->state.indxGRU;
      self->state.hru_data_serialized.ref_gru = self->state.refGRU;
      self->state.hru_data_serialized.timestep = self->state.timestep;
      self->state.hru_data_serialized.forcing_step = self->state.forcingStep;
      self->state.hru_data_serialized.num_steps = self->state.num_steps;
      self->state.hru_data_serialized.iFile = self->state.iFile;
      self->state.hru_data_serialized.dt_init_factor = 
          self->state.dt_init_factor;
      self->state.hru_data_serialized.output_structure_step_index = 
          self->state.output_structure_step_index; 
      self->state.hru_data_serialized.dt_init = self->state.dt_init;
      self->state.hru_data_serialized.upArea = self->state.upArea;
      self->state.hru_data_serialized.rtol = self->state.rtol;
      self->state.hru_data_serialized.atol = self->state.atol;

      // Statistic Structures
      self->state.hru_data_serialized.forc_stat = 
          get_var_dlength_by_indx(self->state.hru_data, 1); 
      self->state.hru_data_serialized.prog_stat = 
          get_var_dlength_by_indx(self->state.hru_data, 2); 
      self->state.hru_data_serialized.diag_stat = 
          get_var_dlength_by_indx(self->state.hru_data, 3); 
      self->state.hru_data_serialized.flux_stat = 
          get_var_dlength_by_indx(self->state.hru_data, 4); 
      self->state.hru_data_serialized.indx_stat = 
          get_var_dlength_by_indx(self->state.hru_data, 5); 
      self->state.hru_data_serialized.bvar_stat = 
          get_var_dlength_by_indx(self->state.hru_data, 6);
      
      // Primary Data Structures (scalars)
      self->state.hru_data_serialized.time_struct = 
          get_var_i_by_indx(self->state.hru_data, 1);
      self->state.hru_data_serialized.forc_struct = 
          get_var_d_by_indx(self->state.hru_data, 1);
      self->state.hru_data_serialized.attr_struct = 
          get_var_d_by_indx(self->state.hru_data, 2);
      self->state.hru_data_serialized.type_struct = 
          get_var_i_by_indx(self->state.hru_data, 2);
      self->state.hru_data_serialized.id_struct = 
          get_var_i8_by_indx(self->state.hru_data, 1);
      
      // Primary Data Structures (variable length vectors)
      self->state.hru_data_serialized.indx_struct = 
          get_var_ilength_by_indx(self->state.hru_data, 1);
      self->state.hru_data_serialized.mpar_struct = 
          get_var_dlength_by_indx(self->state.hru_data, 7);      
      self->state.hru_data_serialized.prog_struct = 
          get_var_dlength_by_indx(self->state.hru_data, 8);
      self->state.hru_data_serialized.diag_struct = 
          get_var_dlength_by_indx(self->state.hru_data, 9);
      self->state.hru_data_serialized.flux_struct = 
          get_var_dlength_by_indx(self->state.hru_data, 10);

      // Basin-average structures
      self->state.hru_data_serialized.bpar_struct = 
          get_var_d_by_indx(self->state.hru_data, 3);
      self->state.hru_data_serialized.bvar_struct = 
          get_var_dlength_by_indx(self->state.hru_data, 11);
      self->state.hru_data_serialized.dpar_struct = 
          get_var_d_by_indx(self->state.hru_data, 4);

      // Local HRU data structures
      self->state.hru_data_serialized.start_time = 
          get_var_i_by_indx(self->state.hru_data, 3);
      self->state.hru_data_serialized.end_time = 
          get_var_i_by_indx(self->state.hru_data, 4);
      self->state.hru_data_serialized.ref_time = 
          get_var_i_by_indx(self->state.hru_data, 5);
      self->state.hru_data_serialized.old_time = 
          get_var_i_by_indx(self->state.hru_data, 6);

      // Statistic flags
      self->state.hru_data_serialized.stat_counter = 
          get_var_i_by_indx(self->state.hru_data, 7);
      self->state.hru_data_serialized.output_timestep = 
          get_var_i_by_indx(self->state.hru_data, 8);
      self->state.hru_data_serialized.reset_stats = 
          get_flagVec_by_indx(self->state.hru_data, 1);
      self->state.hru_data_serialized.finalize_stats = 
          get_flagVec_by_indx(self->state.hru_data, 2);
    },

  };
}



void Initialize_HRU(stateful_actor<hru_state>* self) {

  setupHRUParam(&self->state.indxGRU, &self->state.indxHRU, 
      self->state.hru_data, &self->state.upArea, &self->state.err);
  if (self->state.err != 0) {
    aout(self) << "Error: HRU_Actor - SetupHRUParam - HRU = " 
                << self->state.indxHRU
                << " - indxGRU = " << self->state.indxGRU 
                << " - refGRU = " << self->state.refGRU << "\n";
    self->quit();
    return;
  }
          
  summa_readRestart(&self->state.indxGRU, &self->state.indxHRU,
      self->state.hru_data, &self->state.dt_init, &self->state.err);
  if (self->state.err != 0) {
    aout(self) << "Error: HRU_Actor - summa_readRestart - HRU = " << self->state.indxHRU
                << " - indxGRU = " << self->state.indxGRU 
                << " - refGRU = " << self->state.refGRU << "\n";
    self->quit();
    return;
  }
  #ifdef SUNDIALS_ACTIVE
    if (self->state.hru_actor_settings.rel_tol > 0 && 
        self->state.hru_actor_settings.abs_tol > 0)
      set_sundials_tolerances(self->state.hru_data, 
          &self->state.hru_actor_settings.rel_tol, 
          &self->state.hru_actor_settings.abs_tol);
  #endif           
}

int Run_HRU(stateful_actor<hru_state>* self) {
  /**********************************************************************
  ** READ FORCING
  **********************************************************************/    
  HRU_readForcing(&self->state.indxGRU, &self->state.timestep, 
      &self->state.forcingStep, &self->state.iFile, self->state.hru_data, 
      &self->state.err);
  if (self->state.err != 0) {
    aout(self) << "Error---HRU_Actor: ReadForcingHRU\n" 
                << "\tIndxGRU = "               << self->state.indxGRU << "\n"
                << "\tRefGRU = "                << self->state.refGRU << "\n"
                << "\tForcing Step = "          << self->state.forcingStep << "\n"
                << "\tTimestep = "              << self->state.timestep << "\n"
                << "\tiFile = "                 << self->state.iFile << "\n"
                << "\tSteps in Forcing File = " << self->state.stepsInCurrentFFile << "\n";
    self->quit();
    return -1;
  }

  if (self->state.hru_actor_settings.print_output && self->state.timestep % 
      self->state.hru_actor_settings.output_frequency == 0) {
    aout(self) << self->state.refGRU << " - Timestep = " << self->state.timestep << "\n";
  }
    

    /**********************************************************************
    ** RUN_PHYSICS    
    **********************************************************************/    

  self->state.err = 0;
  RunPhysics(&self->state.indxHRU, &self->state.timestep, self->state.hru_data, 
      &self->state.dt_init,  &self->state.dt_init_factor, &self->state.err);
  if (self->state.err != 0) {
    aout(self) << "Error---RunPhysics:\n"
               << "\tIndxGRU = "  << self->state.indxGRU 
               << "\tRefGRU = "   << self->state.refGRU 
               << "\tTimestep = " << self->state.timestep <<  "\n";
    self->quit();
    return 20;
  }

  hru_writeOutput(&self->state.indxHRU, &self->state.indxGRU,
      &self->state.timestep, &self->state.output_structure_step_index,
      self->state.hru_data, &self->state.err);
  if (self->state.err != 0) {
    aout(self) << "Error: HRU_Actor - writeHRUToOutputStructure - HRU = " 
                << self->state.indxHRU << " - indxGRU = " 
                << self->state.indxGRU << " - refGRU = " << self->state.refGRU
                << "\nError = " << self->state.err  << "\n";
    self->quit();
    return 21;
  }

  return 0;      
}


}