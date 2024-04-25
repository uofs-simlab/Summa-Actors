#include "file_access_actor.hpp"

using json = nlohmann::json;


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
    [=](init_file_access_actor, int file_gru) {
      aout(self) << "File Access Actor: Intializing\n";
      auto& fa_settings = self->state.file_access_actor_settings;
      int num_hru = self->state.num_gru;
      int err = 0;

      // Call ffile_info
      self->state.forcing_files = std::make_unique<forcingFileContainer>();
      err = self->state.forcing_files->initForcingFiles(self->state.num_gru);
      if (err != 0) return -1;

      std::unique_ptr<char[]> message(new char[256]);
      fileAccessActor_init_fortran(&self->state.num_steps,
          &fa_settings.num_timesteps_in_output_buffer, self->state.handle_ncid,
          &self->state.start_gru, &self->state.num_gru, &num_hru, &err,
          &message);
      if (err != 0) {
        aout(self) << "\n\nERROR: fileAccessActor_init_fortran() - " 
                   << message.get() << "\n\n";
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
      defOutputFortran(self->state.handle_ncid, 
                       &self->state.start_gru, 
                       &self->state.num_gru, 
                       &num_hru, 
                       &file_gru, 
                       &self->state.num_gru_info.use_global_for_data_structures,
                       actor_address.c_str(), 
                       &err);
      if (err != 0) return -1;

      self->state.file_access_timing.updateEndPoint("init_duration");
      return self->state.num_steps;
    },

    // Message from the HRU actor to get the forcing file that is loaded
    [=](access_forcing, int iFile, caf::actor refToRespondTo) {
      if (self->state.forcing_files->allFilesLoaded()) {
        self->send(refToRespondTo, new_forcing_file_v, 
          self->state.forcing_files->getNumSteps(iFile), iFile);
        return;
      }
      auto err = self->state.forcing_files->loadForcingFile(iFile, 
          self->state.start_gru, self->state.num_gru);
      if (err != 0) {
        aout(self) << "ERROR: Reading Forcing" << std::endl;
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
        aout(self) << "ERROR: Reading Forcing" << std::endl;
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

    // Write message from the job actor TODO: This could be async
    [=](write_output, int steps_to_write, int start_gru, int max_gru) {
      self->state.file_access_timing.updateStartPoint("write_duration");
      int err = 0;
      writeOutput_fortran(self->state.handle_ncid, &steps_to_write,
          &start_gru, &max_gru, &self->state.write_params_flag, &err);
      if (self->state.write_params_flag) 
        self->state.write_params_flag = false;
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
        self->state.output_container->~Output_Container();
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
  bool write_param_flag = partition->isWriteParams();
  int err = 0;
  writeOutput_fortran(self->state.handle_ncid, &num_timesteps_to_write,
      &start_gru, &max_gru, &write_param_flag, &err);
  
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
