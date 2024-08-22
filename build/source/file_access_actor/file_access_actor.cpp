#include "file_access_actor.hpp"

using json = nlohmann::json;
using namespace caf;

const int NOTIFY_ERR = -1;  // Error code for notification but not quitting

behavior file_access_actor(stateful_actor<file_access_state>* self, 
                           NumGRUInfo num_gru_info,
                           File_Access_Actor_Settings file_access_actor_settings, 
                           actor parent) {
  aout(self) << "\n----------File_Access_Actor Started----------\n";

  self->set_exit_handler([=](const caf::exit_msg& em) {
    aout(self) << "File Access Actor: Received Exit Message\n";
  });
    
  // Set Up timing Info we wish to track
  self->state.file_access_timing = TimingInfo();
  self->state.file_access_timing.addTimePoint("write_duration");
  // Save the parameters passed from job_actor
  self->state.file_access_actor_settings = file_access_actor_settings;
  auto& fa_settings = self->state.file_access_actor_settings;
  self->state.parent = parent;
  self->state.num_gru_info = num_gru_info;

  if (self->state.num_gru_info.use_global_for_data_structures) {
    self->state.start_gru = self->state.num_gru_info.start_gru_global;
    self->state.num_gru = self->state.num_gru_info.num_gru_global;
  } else {
    self->state.start_gru = self->state.num_gru_info.start_gru_local;
    self->state.num_gru = self->state.num_gru_info.num_gru_local;
  }
  self->state.num_output_steps = fa_settings.num_timesteps_in_output_buffer;
  
  return {
    [=](init_file_access_actor, int file_gru, int num_hru) {
      aout(self) << "File Access Actor: Initializing\n";
      auto& fa_settings = self->state.file_access_actor_settings;
      self->state.num_hru = num_hru;
      
      // Get the information about the forcing files
      self->state.forcing_files = std::make_unique<forcingFileContainer>();
      if (self->state.forcing_files->initForcingFiles() != 0) return -1;

      int err = 0;
      std::unique_ptr<char[]> message(new char[256]);
      fileAccessActor_init_fortran(&self->state.num_steps,
                                   &fa_settings.num_timesteps_in_output_buffer, 
                                   &self->state.num_gru, &err, &message);
      if (err != 0) {
        aout(self) << "\n\nFile Access Actor: Error fileAccessActor_init\n" 
                   << "\tMessage = " << message.get() << "\n\n";
        return -1;
      }
   
      // Ensure output buffer size is less than the number of simulation timesteps
      if (self->state.num_steps < fa_settings.num_timesteps_in_output_buffer) {
        self->state.num_output_steps = self->state.num_steps;
        fa_settings.num_timesteps_in_output_buffer = self->state.num_steps;
      }

      // Set up the output container
      if (!self->state.num_gru_info.use_global_for_data_structures) {
        self->state.output_container = new Output_Container(
            fa_settings.num_partitions_in_output_buffer, self->state.num_gru,
            fa_settings.num_timesteps_in_output_buffer, self->state.num_steps);
      }

      aout(self) << "Creating Output File\n";
      std::string actor_address = "";  
      if (self->state.num_gru_info.use_global_for_data_structures) {
        actor_address = "_" + to_string(self->address());
      }

      if (fa_settings.output_file_suffix != "") {
        actor_address = "_" + fa_settings.output_file_suffix;
        self->state.num_gru_info.use_global_for_data_structures = true;
      }

      defOutputFortran(self->state.handle_ncid, &self->state.start_gru, 
                       &self->state.num_gru, &self->state.num_hru, &file_gru, 
                       &self->state.num_gru_info.use_global_for_data_structures,
                       actor_address.c_str(), &err);
      if (err != 0) return -1;

      self->state.file_access_timing.updateEndPoint("init_duration");
      return self->state.num_steps;
    },

    [=](openwq_initialize, caf::actor openwq) {
      self->state.openwq = openwq;
      self->state.openwq_registered = true;
    },

    // Message from the HRU actor to get the forcing file that is loaded
    [=](access_forcing, int iFile, caf::actor refToRespondTo) {
      if (self->state.forcing_files->allFilesLoaded()) {
        self->send(refToRespondTo, new_forcing_file_v, 
          self->state.forcing_files->getNumSteps(iFile), iFile);
        return;
      }
      auto err = self->state.forcing_files->
          loadForcingFile(iFile, self->state.start_gru, self->state.num_gru);
      if (err != 0) {
        aout(self) << "File_Access_Actor: Error loadForcingFile\n"
                   << "\tMessage = Can't load forcing file\n";
        self->send(self->state.parent, err_atom_v, 0, 0, err, 
                   "Can't load forcing file\n");
        self->quit();
        return;
      }
      
      // Load files behind the scenes
      self->send(self, access_forcing_internal_v, iFile + 1);
      self->send(refToRespondTo, new_forcing_file_v, 
                 self->state.forcing_files->getNumSteps(iFile), iFile);
    },

    // Internal message to load the forcing files in the background    
    [=](access_forcing_internal, int iFile) {
      if (self->state.forcing_files->allFilesLoaded()) {
        return;
      }
      auto err = self->state.forcing_files->loadForcingFile(iFile, 
          self->state.start_gru, self->state.num_gru);
      if (err != 0) {
        aout(self) << "File_Access_Actor: Error loadForcingFile\n"
                   << "\tMessage = Can't load forcing file\n";
        self->send(self->state.parent, err_atom_v, 0, 0, err, 
                   "Can't load forcing file\n");
        self->quit();
        return;
      }
      self->send(self, access_forcing_internal_v, iFile + 1);
    },
        
    // Number of steps an HRU can compute before needing to write
    [=] (get_num_output_steps) { 
      return self->state.num_output_steps; 
    },

    [=](write_output, int index_gru, int index_hru, caf::actor hru_actor) {
      self->state.file_access_timing.updateStartPoint("write_duration");

      Output_Partition *output_partition = 
          self->state.output_container->getOutputPartition(index_gru);

      output_partition->setGRUReadyToWrite(hru_actor);
        
      if (output_partition->isReadyToWrite()) {
        writeOutput(self, output_partition);
      }

      self->state.file_access_timing.updateEndPoint("write_duration");
    },

    [=](write_restart, int gru, int gru_timestep, int gru_checkpoint, 
         int output_stucture_index, int year, int month, int day, int hour){
      // update hru progress vecs 
      int gru_index = abs(self->state.start_gru - gru); 
      if (self->state.openwq_registered) {
        self->send(self->state.openwq, space_step_openwq_v, gru, gru_checkpoint, output_stucture_index);
      }
      /*self->state.hru_timesteps[gru_index] = gru_timestep;
      self->state.hru_checkpoints[gru_index] = gru_checkpoint;

      // find slowest time step of all hrus in job, stored in self->state.hru_timesteps
      int slowest_timestep = *std::min_element(
          self->state.hru_timesteps.begin(), 
          self->state.hru_timesteps.end());  
      int slowest_checkpoint = *std::min_element(
          self->state.hru_checkpoints.begin(), 
          self->state.hru_checkpoints.end());  

      // if the slowest hru is past the ith checkpoint (current threshold)            
      if ( slowest_checkpoint >= (self->state.completed_checkpoints)){// temp for dubuging
        Output_Partition *output_partition = 
            self->state.output_container->getOutputPartition(gru-1);
        writeRestart(self, output_partition, self->state.start_gru, 
                     self->state.hru_timesteps.size(), output_stucture_index,
                     year, month, day, hour);
        // update checkpint counter
        self->state.completed_checkpoints++;*/
      
    },

    // Write message from the job actor TODO: This could be async
    [=](write_output, int steps_to_write, int start_gru, int max_gru) {
      self->state.file_access_timing.updateStartPoint("write_duration");
      std::unique_ptr<char[]> err_msg(new char[256]);
      int err = 0;
      writeOutput_fortran(self->state.handle_ncid, &steps_to_write, &start_gru, 
                          &max_gru, &self->state.write_params_flag, &err,
                          &err_msg);
      if (err != 0) {
        aout(self) << "File Access Actor: Error writeOutput from job\n"
                   << "\tMessage = " << err_msg.get() << "\n";
        self->send(self->state.parent, err_atom_v, 0, 0, err, err_msg.get());
      }

      if (self->state.write_params_flag) self->state.write_params_flag = false;
      self->state.file_access_timing.updateEndPoint("write_duration");
      return err;
    },

    [=](restart_failures) {
      self->state.output_container->reconstruct();
    },

    [=](run_failure, int local_gru_index) {
      self->state.file_access_timing.updateStartPoint("write_duration");
      Output_Partition *output_partition = 
          self->state.output_container->getOutputPartition(local_gru_index);
        
      output_partition->addFailedGRUIndex(local_gru_index);

      if (output_partition->isReadyToWrite()) {
        writeOutput(self, output_partition);
      }
      self->state.file_access_timing.updateEndPoint("write_duration");
    },

    [=](finalize) {
        
      aout(self) << "File Access Actor: Deallocating Structures\n";
      // TODO: output container can be wrapped in a smart pointer
      if (!self->state.num_gru_info.use_global_for_data_structures) {
        delete self->state.output_container;
      }
      FileAccessActor_DeallocateStructures(self->state.handle_ncid);
      aout(self) << "\n________________" 
                 << "FILE_ACCESS_ACTOR TIMING INFO RESULTS________________\n"
                 << "Total Read Duration = "
                 << self->state.forcing_files->getReadDuration() << " Seconds\n"
                 << "Total Write Duration = "
                 << self->state.file_access_timing.getDuration("write_duration")
                     .value_or(-1.0) << " Seconds\n"
                 << "\n__________________________________________________\n"; 
      self->quit();
      return std::make_tuple(self->state.forcing_files->getReadDuration(),
                             self->state.file_access_timing
                             .getDuration("write_duration").value_or(-1.0));
    },
  };
}


void writeOutput(stateful_actor<file_access_state>* self, 
                 Output_Partition* partition) {              
  int num_timesteps_to_write = partition->getNumStoredTimesteps();
  int start_gru = partition->getStartGRUIndex();
  int max_gru = partition->getMaxGRUIndex();
  if (start_gru > max_gru) {
    aout(self) << "File Access Actor: Error writeOutput\n"
               << "\tMessage = start_gru > max_gru\n";
    self->send(self->state.parent, err_atom_v, 0, 0, NOTIFY_ERR, 
               "start_gru > max_gru");
    return;
  }
  bool write_param_flag = partition->isWriteParams();
  int err = 0;
  std::unique_ptr<char[]> err_msg(new char[256]);
  writeOutput_fortran(self->state.handle_ncid, &num_timesteps_to_write,
                      &start_gru, &max_gru, &write_param_flag, &err, &err_msg);
  if (err != 0) {
    aout(self) << "File Access Actor: Error writeOutput\n"
               << "\tMessage = " << err_msg.get() << "\n";
    self->send(self->state.parent, err_atom_v, 0, 0, NOTIFY_ERR, err_msg.get());
  }

  partition->updateTimeSteps();

  int num_steps_before_next_write = partition->getNumStoredTimesteps();

  std::vector<caf::actor> hrus_to_update = partition->getReadyToWriteList();
  for (int i = 0; i < hrus_to_update.size(); i++) {
    self->send(hrus_to_update[i], num_steps_before_write_v, 
               num_steps_before_next_write);
    self->send(hrus_to_update[i], run_hru_v);
  }

  partition->resetReadyToWriteList();
}

void writeRestart(stateful_actor<file_access_state>* self , 
                  Output_Partition* partition, int start_gru, int num_gru, 
                  int timestep, int year, int month, int day, int hour){  
  
  writeRestart_fortran(self->state.handle_ncid, &start_gru, &num_gru, &timestep, 
                       &year, &month, &day, &hour, &self->state.err);
}


