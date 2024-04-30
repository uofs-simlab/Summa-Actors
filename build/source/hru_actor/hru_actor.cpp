#include "hru_actor.hpp"

bool hru_extra_logging = false;

using namespace caf;
behavior hru_actor(stateful_actor<hru_state>* self, int ref_gru, int indx_gru,
                   HRU_Actor_Settings hru_actor_settings, 
                   actor file_access_actor, actor parent) {
            
  self->set_exit_handler([=](const caf::exit_msg& msg) {
    aout(self) << "HRU Actor: Received Exit Message\n";
  });
  
  // Actor References
  self->state.file_access_actor = file_access_actor;
  self->state.parent            = parent;
  // Indexes into global structures
  self->state.ref_gru           = ref_gru;
  self->state.indxGRU           = indx_gru;
  self->state.indxHRU           = 1;
  if (hru_extra_logging) {
    aout(self) << "HRU Actor: indxHRU = " << self->state.indxHRU 
               << " indxGRU = " << self->state.indxGRU 
               << " ref_gru = " << self->state.ref_gru << "\n";
  }
  // Get the settings for the HRU
  self->state.hru_actor_settings = hru_actor_settings;
  self->state.dt_init_factor = hru_actor_settings.dt_init_factor;

  return {
    [=](init_hru) {
      Initialize_HRU(self);
    },

    /* Run until completion -- only interact with the file access actor */
    [=](update_hru_async) {
      self->request(self->state.file_access_actor, caf::infinite,
                    get_num_output_steps_v).await([=](int num_steps) {
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

        self->state.num_steps_until_write--;
        err = Run_HRU(self); // Simulate a Timestep
        if (err != 0) {
            self->send(self->state.parent, err_atom_v, err, 
                       self->state.indxGRU);
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
                   << self->state.ref_gru << "\n";
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
                     << " Updating timeZoneOffset\n";
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
        self->send(self->state.parent, err_atom_v, err, self->state.indxGRU);
        self->quit();
        return;
      }
      
      self->send(self->state.parent, done_update_v, 
                 self->state.walltime_timestep, self->state.indxGRU);
    },

    // Get Fortran Data into C++
    [=](serialize_hru) {
      hru serialized_state;
      serializeHru(self, serialized_state);
      self->send(self->state.parent, serialized_state);
    },

    [=](reinit_hru, hru hru_data) {
      deserializeHru(self, hru_data);
      self->send(self->state.parent, reinit_hru_v);
    },
  };
}



void Initialize_HRU(stateful_actor<hru_state>* self) {
  int err = 0;
  initHRU(&self->state.indxGRU, &self->state.num_steps, self->state.hru_data, 
          &err);
  if (err != 0) {
    aout(self) << "Error: HRU_Actor - Initialize - HRU = " 
               << self->state.indxHRU  
               << " - indxGRU = " << self->state.indxGRU 
               << " - refGRU = "<< self->state.ref_gru
               << "\nError Code = " << err << "\n";
    self->quit();
  }

  setupHRUParam(&self->state.indxGRU, &self->state.indxHRU, 
                self->state.hru_data, &self->state.upArea, &err);
  if (err != 0) {
    aout(self) << "Error: HRU_Actor - SetupHRUParam - HRU = " 
                << self->state.indxHRU
                << " - indxGRU = " << self->state.indxGRU 
                << " - refGRU = " << self->state.ref_gru << "\n";
    self->quit();
    return;
  }
          
  summa_readRestart(&self->state.indxGRU, &self->state.indxHRU,
                    self->state.hru_data, &self->state.dt_init, &err);
  if (err != 0) {
    aout(self) << "Error: HRU_Actor - summa_readRestart - HRU = " 
               << self->state.indxHRU
               << " - indxGRU = " << self->state.indxGRU 
               << " - refGRU = " << self->state.ref_gru << "\n";
    self->quit();
    return;
  }
  #ifdef SUNDIALS_ACTIVE
    if (self->state.hru_actor_settings.rel_tol > 0 && 
        self->state.hru_actor_settings.abs_tol > 0) {
      set_sundials_tolerances(self->state.hru_data, 
                              &self->state.hru_actor_settings.rel_tol, 
                              &self->state.hru_actor_settings.abs_tol);
    }
  #endif           
}

int Run_HRU(stateful_actor<hru_state>* self) {
  int err = 0;
  HRU_readForcing(&self->state.indxGRU, &self->state.timestep, 
                  &self->state.forcingStep, &self->state.iFile, 
                  self->state.hru_data, &err);
  if (err != 0) {
    aout(self) << "Error---HRU_Actor: ReadForcingHRU\n" 
               << "\tIndxGRU = " << self->state.indxGRU << "\n"
               << "\tRefGRU = " << self->state.ref_gru << "\n"
               << "\tForcing Step = " << self->state.forcingStep << "\n"
               << "\tTimestep = " << self->state.timestep << "\n"
               << "\tiFile = " << self->state.iFile << "\n"
               << "\tSteps in Forcing File = " 
               << self->state.stepsInCurrentFFile << "\n";
    self->quit();
    return err;
  }

  auto& settings = self->state.hru_actor_settings;
  if (settings.print_output && self->state.timestep % 
      settings.output_frequency == 0) {
    aout(self) << self->state.ref_gru << " - Timestep = " 
               << self->state.timestep << "\n";
  }
    
  RunPhysics(&self->state.indxHRU, &self->state.timestep, self->state.hru_data, 
             &self->state.dt_init,  &self->state.dt_init_factor, 
             &self->state.walltime_timestep, &err);
  if (err != 0) {
    aout(self) << "Error---RunPhysics:\n"
               << "\tIndxGRU = "  << self->state.indxGRU 
               << "\tRefGRU = "   << self->state.ref_gru 
               << "\tTimestep = " << self->state.timestep <<  "\n";
    self->quit();
    return err;
  }

  hru_writeOutput(&self->state.indxHRU, &self->state.indxGRU,
                  &self->state.timestep, 
                  &self->state.output_structure_step_index, 
                  self->state.hru_data, &err);
  if (err != 0) {
    aout(self) << "Error: HRU_Actor - writeHRUToOutputStructure - HRU = " 
                << self->state.indxHRU << " - indxGRU = " 
                << self->state.indxGRU << " - refGRU = " << self->state.ref_gru
                << "\nError = " << err  << "\n";
    self->quit();
    return err;
  }

  return 0;      
}