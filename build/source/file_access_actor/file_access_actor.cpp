#include "file_access_actor.hpp"
#include "forcing_file_info.hpp"
#include "fortran_data_types.hpp"
#include "message_atoms.hpp"
#include "json.hpp"
#include "auxilary.hpp"

using json = nlohmann::json;

namespace caf {

behavior file_access_actor(stateful_actor<file_access_state>* self, 
                          int start_gru, int num_gru, 
                          File_Access_Actor_Settings file_access_actor_settings, 
                          actor parent) {
  aout(self) << "\n----------File_Access_Actor Started----------\n";
    
  // Set Up timing Info we wish to track
  self->state.file_access_timing = TimingInfo();
  self->state.file_access_timing.addTimePoint("read_duration");
  self->state.file_access_timing.addTimePoint("write_duration");
  // Save the parameters passed from job_actor
  self->state.file_access_actor_settings = file_access_actor_settings;
  auto& fa_settings = self->state.file_access_actor_settings;
  self->state.parent = parent;
  self->state.num_gru = num_gru;
  self->state.start_gru = start_gru;
  self->state.err = 0;

  self->state.num_output_steps = fa_settings.num_timesteps_in_output_buffer;

  int num_hru = self->state.num_gru; // Filler for num_hrus
  fileAccessActor_init_fortran(self->state.handle_forcing_file_info, 
      &self->state.numFiles, &self->state.num_steps,
      &fa_settings.num_timesteps_in_output_buffer, self->state.handle_ncid,
      &self->state.start_gru, &self->state.num_gru, &num_hru, &self->state.err);
  if (self->state.err != 0) {
    aout(self) << "ERROR: File Access Actor - File_Access_init_Fortran\n";
    if (self->state.err == 100)
      self->send(self->state.parent, file_access_error::mDecisions_error, 
          self);
    else
      self->send(self->state.parent, file_access_error::unhandleable_error, 
          self);
    return {};  
  }

  // Notify job actor that initial files have been loaded
  self->send(self->state.parent, init_file_access_actor_v, 
      self->state.num_steps);

  // Initalize the forcingFile array
  self->state.filesLoaded = 0;
  for (int i = 1; i <= self->state.numFiles; i++) {
    self->state.forcing_file_list.push_back(Forcing_File_Info(i));
  }
  
  // Ensure output buffer size is less than the number of simulation timesteps
  if (self->state.num_steps < fa_settings.num_timesteps_in_output_buffer) {
    self->state.num_output_steps = self->state.num_steps;
    fa_settings.num_timesteps_in_output_buffer = self->state.num_steps;
  }

  // Set up the output container
  self->state.output_container = new Output_Container(
      fa_settings.num_partitions_in_output_buffer, self->state.num_gru,
      fa_settings.num_timesteps_in_output_buffer, self->state.num_steps); 

  return {
    [=](def_output, int file_gru) {
      aout(self) << "Creating Output File\n";
      int num_hru = self->state.num_gru; // Filler for num_hrus
      int err = 0;
      defOutputFortran(self->state.handle_ncid, &self->state.start_gru, 
          &self->state.num_gru, &num_hru, &file_gru, &err);
      if (self->state.err != 0) {
        aout(self) << "ERROR: Defining Output\n";
        self->quit();
      }
    },

    // Message from the HRU actor to get the forcing file that is loaded
    [=](access_forcing, int currentFile, caf::actor refToRespondTo) {
      if (currentFile <= self->state.numFiles) {
        // Note: C++ starts at 0 and Fortran starts at 1
        if(!self->state.forcing_file_list[currentFile - 1].isFileLoaded()) {
          self->state.file_access_timing.updateStartPoint("read_duration");
          read_forcingFile(self->state.handle_forcing_file_info, &currentFile,
              &self->state.stepsInCurrentFile, &self->state.start_gru, 
              &self->state.num_gru, &self->state.err);
          if (self->state.err != 0) {
            aout(self) << "ERROR: Reading Forcing" << std::endl;
          }
          self->state.filesLoaded += 1;
          self->state.forcing_file_list[currentFile - 1]
              .updateNumSteps(self->state.stepsInCurrentFile);

          self->state.file_access_timing.updateEndPoint("read_duration");
          // Check if all files have been loaded
          if(self->state.filesLoaded <= self->state.numFiles) {
            self->send(self, access_forcing_internal_v, currentFile + 1);
          }
        }
        
        self->send(refToRespondTo, new_forcing_file_v, 
            self->state.forcing_file_list[currentFile - 1].getNumSteps(),
            currentFile);
        } else {
          aout(self) << currentFile << " exceeds the number of forcing files\n"; 
        }
    },
        
    [=](access_forcing_internal, int currentFile) {
      if (self->state.filesLoaded <= self->state.numFiles &&
          currentFile <= self->state.numFiles) {
        
        if (self->state.forcing_file_list[currentFile - 1].isFileLoaded()) {
          aout(self) << "Unexpected File Loaded!!\n";
        }
      
        self->state.file_access_timing.updateStartPoint("read_duration");
        read_forcingFile(self->state.handle_forcing_file_info, &currentFile,
            &self->state.stepsInCurrentFile, &self->state.start_gru, 
            &self->state.num_gru, &self->state.err);
        if (self->state.err != 0) {
          aout(self) << "ERROR: Reading Forcing" << std::endl;
        }
        
        self->state.filesLoaded += 1;
        self->state.forcing_file_list[currentFile - 1]
            .updateNumSteps(self->state.stepsInCurrentFile);
          
        self->state.file_access_timing.updateEndPoint("read_duration");
        self->send(self, access_forcing_internal_v, currentFile + 1);
      } else {
        aout(self) << "All Forcing Files Loaded \n";
      }
    },
        
    // Number of steps an HRU can compute before needing to write
    [=] (get_num_output_steps) { return self->state.num_output_steps; },

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
      
      writeOutput_fortran(self->state.handle_ncid, &steps_to_write,
          &start_gru, &max_gru, &self->state.write_params_flag, 
          &self->state.err);
      if (self->state.write_params_flag) 
        self->state.write_params_flag = false;
      self->state.file_access_timing.updateEndPoint("write_duration");
      return self->state.err;
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
      self->state.output_container->~Output_Container();
      FileAccessActor_DeallocateStructures(self->state.handle_forcing_file_info, 
          self->state.handle_ncid);

      aout(self) << "\n________________" 
                 << "FILE_ACCESS_ACTOR TIMING INFO RESULTS________________\n"
                 << "Total Read Duration = "
                 << self->state.file_access_timing.getDuration("read_duration")
                     .value_or(-1.0) << " Seconds\n"
                 << "Total Write Duration = "
                 << self->state.file_access_timing.getDuration("write_duration")
                     .value_or(-1.0) << " Seconds\n";
                  
        
      self->quit();
      return std::make_tuple(self->state.file_access_timing
          .getDuration("read_duration").value_or(-1.0), 
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
  
  writeOutput_fortran(self->state.handle_ncid, &num_timesteps_to_write,
      &start_gru, &max_gru, &write_param_flag, &self->state.err);
  
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

} // end namespace