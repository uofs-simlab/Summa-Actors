#include "hru_actor.hpp"

bool hru_extra_logging = false;

namespace caf {

behavior hru_actor(stateful_actor<hru_state>* self, int refGRU, int indxGRU,
    HRU_Actor_Settings hru_actor_settings, caf::actor file_access_actor, 
    caf::actor parent) {

  self->set_exit_handler([=](const exit_msg& msg) {
    aout(self) << "HRU Actor: Received Exit Message\n";
  });
  
  // Actor References
  self->state.file_access_actor = file_access_actor;
  self->state.parent            = parent;
  // Indexes into global structures
  self->state.indxHRU           = 1;
  self->state.indxGRU           = indxGRU;
  self->state.refGRU            = refGRU;
  if (hru_extra_logging) {
    aout(self) << "HRU Actor: indxHRU = " << self->state.indxHRU 
               << " indxGRU = " << self->state.indxGRU 
               << " refGRU = " << self->state.refGRU << "\n";
  }
  // Get the settings for the HRU
  self->state.hru_actor_settings = hru_actor_settings;
  self->state.dt_init_factor = hru_actor_settings.dt_init_factor;

  return {
    [=](init_hru) {
      Initialize_HRU(self);
    },


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
          self->send(self->state.file_access_actor, access_forcing_v, 
              self->state.iFile+1, self);
          break;
        }

        if (hru_extra_logging) 
            aout(self) << "HRU:" << self->state.indxGRU << " - Timestep: " 
                       << self->state.timestep << "\n";

        self->state.num_steps_until_write--;
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
        if (hru_extra_logging) {
            aout(self) << "Recieved New iFile-" << iFile 
                       << " with " << num_forcing_steps_in_iFile 
                       << " forcing steps\n";
        }
        int err;
        self->state.iFile = iFile;
        self->state.stepsInCurrentFFile = num_forcing_steps_in_iFile;
        setTimeZoneOffset(&self->state.iFile, self->state.hru_data, &err);
        if (err != 0) {
            aout(self) << "Error: HRU_Actor - setTimeZoneOffset - HRU = " 
                       << self->state.indxHRU << " - indxGRU = " 
                       << self->state.indxGRU << " - refGRU = " 
                       << self->state.refGRU << "\n";
            self->quit();
            return;
        }
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
      
      self->send(self->state.parent, done_update_v, 
                 self->state.walltime_timestep, self->state.indxGRU);
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

      get_scalar_data(self->state.hru_data,
          self->state.hru_data_serialized.frac_jul_day,
          self->state.hru_data_serialized.tm_zone_offset_frac_day,
          self->state.hru_data_serialized.year_length,
          self->state.hru_data_serialized.compute_veg_flux);
       
      self->send(self->state.parent, self->state.hru_data_serialized);
    },

    [=](reinit_hru, hru hru_data) {

      // Set output structure finalize stats to false for all timesteps of 
      // the old GRU index
      setFinalizeStatsFalse(&self->state.indxGRU);
      // deallocate the old hru_data
      delete_handle_hru_type(self->state.hru_data);
      self->state.hru_data = new_handle_hru_type();


      self->state.indxHRU = hru_data.indx_hru;
      self->state.indxGRU = hru_data.indx_gru;
      self->state.refGRU = hru_data.ref_gru;
      self->state.timestep = hru_data.timestep;
      self->state.forcingStep = hru_data.forcing_step;
      self->state.num_steps = hru_data.num_steps;
      self->state.iFile = hru_data.iFile;
      self->state.dt_init_factor = hru_data.dt_init_factor;
      self->state.output_structure_step_index = 
          hru_data.output_structure_step_index;
      self->state.dt_init = hru_data.dt_init;
      self->state.upArea = hru_data.upArea;
      self->state.rtol = hru_data.rtol;
      self->state.atol = hru_data.atol;

      // Statistic Structures
      set_var_dlength_by_indx(self->state.hru_data, hru_data.forc_stat, 1);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.prog_stat, 2);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.diag_stat, 3);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.flux_stat, 4);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.indx_stat, 5);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.bvar_stat, 6);

      // Primary Data Structures (scalars)
      set_var_i_by_indx(self->state.hru_data, hru_data.time_struct, 1);
      set_var_d_by_indx(self->state.hru_data, hru_data.forc_struct, 1);
      set_var_d_by_indx(self->state.hru_data, hru_data.attr_struct, 2);
      set_var_i_by_indx(self->state.hru_data, hru_data.type_struct, 2);
      set_var_i8_by_indx(self->state.hru_data, hru_data.id_struct, 1);

      // Primary Data Structures (variable length vectors)
      set_var_ilength_by_indx(self->state.hru_data, hru_data.indx_struct, 1);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.mpar_struct, 7);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.prog_struct, 8);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.diag_struct, 9);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.flux_struct, 10);

      // Basin-average structures
      set_var_d_by_indx(self->state.hru_data, hru_data.bpar_struct, 3);
      set_var_dlength_by_indx(self->state.hru_data, hru_data.bvar_struct, 11);
      set_var_d_by_indx(self->state.hru_data, hru_data.dpar_struct, 4);

      // Local HRU data structures
      set_var_i_by_indx(self->state.hru_data, hru_data.start_time, 3);
      set_var_i_by_indx(self->state.hru_data, hru_data.end_time, 4);
      set_var_i_by_indx(self->state.hru_data, hru_data.ref_time, 5);
      set_var_i_by_indx(self->state.hru_data, hru_data.old_time, 6);

      // statistic flags
      set_var_i_by_indx(self->state.hru_data, hru_data.stat_counter, 7);
      set_var_i_by_indx(self->state.hru_data, hru_data.output_timestep, 8);
      set_flagVec_by_indx(self->state.hru_data, hru_data.reset_stats, 1);
      set_flagVec_by_indx(self->state.hru_data, hru_data.finalize_stats, 2);

      // scalar data
      set_scalar_data(self->state.hru_data, hru_data.frac_jul_day,
          hru_data.tm_zone_offset_frac_day, hru_data.year_length,
          hru_data.compute_veg_flux);

      self->send(self->state.parent, reinit_hru_v);
    },

  };
}



void Initialize_HRU(stateful_actor<hru_state>* self) {
  initHRU(&self->state.indxGRU, &self->state.num_steps, self->state.hru_data, 
      &self->state.err);
  if (self->state.err != 0) {
    aout(self) << "Error: HRU_Actor - Initialize - HRU = " 
               << self->state.indxHRU  
               << " - indxGRU = " << self->state.indxGRU 
               << " - refGRU = "<< self->state.refGRU
               << "\nError Code = " << self->state.err << "\n";
    self->quit();
  }

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
    aout(self) << "Error: HRU_Actor - summa_readRestart - HRU = " 
               << self->state.indxHRU
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
               << "\tIndxGRU = " << self->state.indxGRU << "\n"
               << "\tRefGRU = " << self->state.refGRU << "\n"
               << "\tForcing Step = " << self->state.forcingStep << "\n"
               << "\tTimestep = " << self->state.timestep << "\n"
               << "\tiFile = " << self->state.iFile << "\n"
               << "\tSteps in Forcing File = " 
               << self->state.stepsInCurrentFFile << "\n";
    self->quit();
    return -1;
  }

  if (self->state.hru_actor_settings.print_output && self->state.timestep % 
      self->state.hru_actor_settings.output_frequency == 0) {
    aout(self) << self->state.refGRU << " - Timestep = " 
               << self->state.timestep << "\n";
  }
    

    /**********************************************************************
    ** RUN_PHYSICS    
    **********************************************************************/    

  self->state.err = 0;
  RunPhysics(&self->state.indxHRU, &self->state.timestep, self->state.hru_data, 
      &self->state.dt_init,  &self->state.dt_init_factor, 
      &self->state.walltime_timestep, &self->state.err);
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