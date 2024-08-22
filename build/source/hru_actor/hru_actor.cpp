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
  self->state.parent = parent;
  self->state.ref_gru = ref_gru;
  self->state.indx_gru = indx_gru;
  self->state.indx_hru = 1;
  if (hru_extra_logging) {
    aout(self) << "HRU Actor: indxHRU = " << self->state.indx_hru 
               << " indx_gru = " << self->state.indx_gru 
               << " ref_gru = " << self->state.ref_gru << "\n";
  }
  // Get the settings for the HRU
  self->state.hru_actor_settings = hru_actor_settings;
  self->state.dt_init_factor = hru_actor_settings.dt_init_factor;

  // Set the restart frequency
  self->state.restartFrequency = 1; // TODO: obtain this value from command line arg


  return {
    [=](init_hru) {
      int err = initHRU(self);
      if (err != 0) {
        self->send(self->state.parent, err_atom_v, self->state.indx_gru, 
                   self->state.timestep, err, self->state.err_message);
        self->quit(); return;
      }
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
        err = runHRU(self); // Simulate a Timestep
        if (err != 0) {
          self->send(self->state.parent, err_atom_v, self->state.indx_gru, 
                     self->state.timestep, err, self->state.err_message);
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
      // Output Structure Full -- Need to request write
      if (self->state.num_steps_until_write <= 0) {
          self->send(self->state.file_access_actor, write_output_v, 
                     self->state.indx_gru, self->state.indx_hru, self);
      }
    },


    [=](new_forcing_file, int num_forcing_steps_in_iFile, int iFile) {
      int err;
      self->state.iFile = iFile;
      self->state.stepsInCurrentFFile = num_forcing_steps_in_iFile;
      std::unique_ptr<char[]> message(new char[256]);
      setTimeZoneOffset_fortran(iFile, self->state.hru_data, err, &message);
      if (err != 0) {
        aout(self) << "HRU_Actor: Error setTimeZoneOffset\n" 
                   << "\tindx_gru = " << self->state.indx_gru << "\n"
                   << "\tref_gru = "  << self->state.ref_gru  << "\n"
                   << "\tTimestep = " << self->state.timestep << "\n"
                   << "\tMessage = "  << message.get() << "\n";
        self->send(self->state.parent, err_atom_v, self->state.indx_gru, 
                   self->state.timestep, err, message.get());
        self->quit(); 
        return;
      }
      self->state.forcingStep = 1;
      self->send(self, run_hru_v);
    },

    
    [=](done_hru) {
      self->send(self->state.parent,done_hru_v,self->state.indx_gru);
      self->quit();
      return;
    },

    [=](dt_init_factor, int dt_init_factor) {
      aout(self) << "Received New dt_init_factor to attempt on next run \n";
    },

    [=](update_timeZoneOffset, int iFile) {
      if (hru_extra_logging)
          aout(self) << "Recieved New iFile-" << iFile 
                     << " Updating timeZoneOffset\n";
      int err;
      self->state.iFile = iFile;
      std::unique_ptr<char[]> message(new char[256]);
      setTimeZoneOffset_fortran(iFile, self->state.hru_data, err, &message);
      if (err !=0) {
        aout(self) << "HRU_Actor: Error setTimeZoneOffset\n" 
                   << "\tindx_gru = " << self->state.indx_gru << "\n"
                   << "\tref_gru = "  << self->state.ref_gru  << "\n"
                   << "\tTimestep = " << self->state.timestep << "\n"
                   << "\tMessage = "  << message.get() << "\n";
        self->send(self->state.parent, err_atom_v, self->state.indx_gru, 
                   self->state.timestep, err, message.get());
        self->quit(); 
        return;
      }
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

      int err = runHRU(self);
      if (err != 0) {
        self->send(self->state.parent, err_atom_v, self->state.indx_gru, 
                   self->state.timestep, err, self->state.err_message);
        self->quit();
        return;
      }
      
      self->send(self->state.parent, done_update_v, 
                 self->state.walltime_timestep, self->state.indx_gru);
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



int initHRU(stateful_actor<hru_state>* self) {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  initHRU_fortran(self->state.indx_gru, self->state.indx_hru, 
                  self->state.num_steps, self->state.hru_data, err, &message);
  if (err != 0) {
    aout(self) << "HRU_Actor: Error initHRU\n" 
               << "\tindx_gru = " << self->state.indx_gru << "\n"
               << "\tref_gru = "  << self->state.ref_gru  << "\n"
               << "\tTimestep = " << self->state.timestep << "\n"
               << "\tMessage = "  << message.get() << "\n";
    self->state.err_message = message.get();
    return err;
  }
  
  std::fill(message.get(), message.get() + 256, '\0'); // Clear message
  setupHRU_fortran(self->state.indx_gru, self->state.indx_hru, 
                   self->state.hru_data, err, &message);
  if (err != 0) {
    aout(self) << "HRU_Actor: Error setupHRU\n" 
               << "\tindx_gru = " << self->state.indx_gru << "\n"
               << "\tref_gru = "  << self->state.ref_gru  << "\n"
               << "\tTimestep = " << self->state.timestep << "\n"
               << "\tMessage = "  << message.get() << "\n";
    self->state.err_message = message.get();
    return err;
  }

  std::fill(message.get(), message.get() + 256, '\0'); // Clear message
  readHRURestart_fortran(self->state.indx_gru, self->state.indx_hru,
                         self->state.hru_data, err, &message);
  if (err != 0) {
    aout(self) << "HRU_Actor: Error readHRURestart\n" 
               << "\tindx_gru = " << self->state.indx_gru << "\n"
               << "\tref_gru = "  << self->state.ref_gru  << "\n"
               << "\tTimestep = " << self->state.timestep << "\n"
               << "\tMessage = "  << message.get() << "\n";
    self->state.err_message = message.get();
    return err;
  }
  #ifdef SUNDIALS_ACTIVE
    if (self->state.hru_actor_settings.rel_tol > 0 && 
        self->state.hru_actor_settings.abs_tol > 0) {
      set_sundials_tolerances(self->state.hru_data, 
                              &self->state.hru_actor_settings.rel_tol, 
                              &self->state.hru_actor_settings.abs_tol);
    }
  #endif     
  
  return 0;      
}

int runHRU(stateful_actor<hru_state>* self) {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  readHRUForcing_fortran(self->state.indx_gru, self->state.indx_hru, 
                         self->state.timestep, self->state.forcingStep, 
                         self->state.iFile, self->state.hru_data, err,
                         &message);
  if (err != 0) {
    aout(self) << "HRU_Actor: Error readHRUForcing\n" 
               << "\tindx_gru = "     << self->state.indx_gru    << "\n"
               << "\tRefGRU = "       << self->state.ref_gru     << "\n"
               << "\tForcing Step = " << self->state.forcingStep << "\n"
               << "\tTimestep = "     << self->state.timestep    << "\n"
               << "\tiFile = "        << self->state.iFile       << "\n"
               << "\tSteps in Forcing File = " 
               << self->state.stepsInCurrentFFile << "\n";
    self->state.err_message = message.get();
    return err;
  }

  auto& settings = self->state.hru_actor_settings;
  if (settings.print_output && self->state.timestep % 
      settings.output_frequency == 0) {
    aout(self) << self->state.ref_gru << " - Timestep = " 
               << self->state.timestep << "\n";
  }

  std::fill(message.get(), message.get() + 256, '\0'); // Clear message
  runHRU_fortran(self->state.indx_gru, self->state.indx_hru, self->state.timestep, 
                 self->state.hru_data, self->state.dt_init_factor, 
                 self->state.walltime_timestep, err, &message);
  if (err != 0) {
    aout(self) << "HRU_Actor: Error RunPhysics:\n"
               << "\tindx_gru = " << self->state.indx_gru << "\n"
               << "\tref_gru = "   << self->state.ref_gru  << "\n"
               << "\tTimestep = " << self->state.timestep << "\n"
               << "\tMessage = "  << message.get() << "\n";
    self->state.err_message = message.get();
    return err;
  }

  if (self->state.timestep != 0){
    // write variables for output to summa_struct, extract y,m,d,h from 
    // fortran side ald save it to the hru's state
    int y,m,d,h;
    std::fill(message.get(), message.get() + 256, '\0'); // Clear message
    writeHRUOutput_fortran(self->state.indx_gru, self->state.indx_hru,
                           self->state.timestep, 
                           self->state.output_structure_step_index,
                           self->state.hru_data, y, m, d, h, err, &message);

    if (err != 0) {
      aout(self) << "HRU_Actor: Error writeHRUToOutputStructure" 
                 << "\tindx_gru = " << self->state.indx_gru << "\n"
                 << "\tref_gru = "   << self->state.ref_gru  << "\n"
                 << "\tTimestep = " << self->state.timestep << "\n"
                 << "\tMessage = "  << message.get() << "\n";
      self->state.err_message = message.get();
      return err;
    }

    self->state.currentDate.y = y;
    self->state.currentDate.m = m;
    self->state.currentDate.d = d;
    self->state.currentDate.h = h;

    // collect date infomation on first timestep to find starting date
    if (self->state.timestep == 1 ){
        self->state.startDate = self->state.currentDate;
    }

    // check if hru reached a checkpoint, if so it will write restart data to outputsructure and
    // send an update to the FAA
    if (isCheckpoint(self)){
      self->state.checkpoint++;

      hru_writeRestart(&self->state.indx_hru, &self->state.indx_gru,
                       &self->state.output_structure_step_index,
                       &self->state.output_structure_step_index, //unused
                       self->state.hru_data, &err);
     
      self->send(self->state.file_access_actor, write_restart_v,
                 self->state.ref_gru, self->state.timestep,
                 self->state.checkpoint,
                 self->state.output_structure_step_index,
                 self->state.currentDate.y, self->state.currentDate.m,
                 self->state.currentDate.d, self->state.currentDate.h);
    }
  }

  return 0;      
}

// given a hru_actor with a state, compared current date with starting date
// to deterimine if hru is on a checkpoint
bool isCheckpoint(stateful_actor<hru_state>* self){
  switch(self->state.restartFrequency){
    case 0: // restart not enabled
      break;
    case 1: // every timestep
      return true;
    case 2: // daily
      if (self->state.startDate.h == self->state.currentDate.h){
          return true;
      }
      break;
    case 3: // weekly not supported
      break;
    case 4: // monthly
      if (self->state.startDate.d == self->state.currentDate.d &&
          self->state.startDate.h == self->state.currentDate.h){
        return true;
      }    
      break;   
    case 5: // yearly
      if (self->state.startDate.m == self->state.currentDate.m &&
          self->state.startDate.d == self->state.currentDate.d &&
          self->state.startDate.h == self->state.currentDate.h){
        return true;
      }
      break;
  }
  return false;
}

