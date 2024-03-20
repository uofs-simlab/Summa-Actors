#include "job_actor.hpp"
#include "summa_actor.hpp"
#include "node_actor.hpp"

namespace caf {

behavior distributed_job_actor(stateful_actor<distributed_job_state>* self,
    int start_gru, int num_gru, Distributed_Settings distributed_settings,
    File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, 
    HRU_Actor_Settings hru_actor_settings) {
  aout(self) << "Starting Distributed Job Actor\n";
  self->state.job_timing = TimingInfo();
  self->state.job_timing.addTimePoint("total_duration");
  self->state.job_timing.updateStartPoint("total_duration");
  
  self->set_down_handler([=](const down_msg& dm){
    aout(self) << "Received Down Message\n";
  });
  
  self->state.start_gru = start_gru;
  self->state.num_gru = num_gru;
  
  self->state.distributed_settings = distributed_settings;
  self->state.file_access_actor_settings = file_access_actor_settings;
  self->state.job_actor_settings = job_actor_settings;
  self->state.hru_actor_settings = hru_actor_settings;

  self->state.file_gru = getNumGRUInFile(job_actor_settings.file_manager_path);
  if (self->state.file_gru  == -1) 
    aout(self) << "***WARNING***: UNABLE TO VERIFY NUMBER OF GRUS" 
               << " - Job Actor MAY CRASH\n";

  // Fix the number of GRUs if it exceeds the number of GRUs in the file
  if (self->state.file_gru > 0) { 
    if (self->state.start_gru + self->state.num_gru > self->state.file_gru) {
      self->state.num_gru = self->state.file_gru - self->state.start_gru + 1;
    }
  }

  aout(self) << "Number of Nodes = " << distributed_settings.num_nodes << "\n";

  // Set up the node ranges
  int gru_per_node = (self->state.num_gru + distributed_settings.num_nodes - 1) 
                        / distributed_settings.num_nodes;
  int remaining = self->state.num_gru;
  for (int i = 0; i < distributed_settings.num_nodes; i++) {
    int start_gru = i * gru_per_node + self->state.start_gru;
    int num_gru = gru_per_node;
    if (i == distributed_settings.num_nodes - 1) {
      num_gru = remaining;
    }
    remaining -= num_gru;
    self->state.node_gru_ranges.push_back(std::make_tuple(start_gru, num_gru));
  }

  // Print the node ranges
  for (int i = 0; i < distributed_settings.num_nodes; i++) {
    aout(self) << "Node " << i << " GRU Range: " 
               << std::get<0>(self->state.node_gru_ranges[i]) << " - " 
               << std::get<1>(self->state.node_gru_ranges[i]) << "\n";
  }

  auto is_published = self->system().middleman().publish(self, 
      distributed_settings.port);
  if (!is_published) {
    aout(self) << "Unable to publish actor\n";
    self->quit();
    return {};
  }

  aout(self) << "Distributed Job Actor Has Been Published\n";

  // Spawn the local node actor
  auto node = self->spawn(node_actor, "", self, distributed_settings, 
      file_access_actor_settings, job_actor_settings, hru_actor_settings);
  
  return {

    // Message from nodes when they are connected
    [=](connect_to_server, actor client_actor, std::string hostname) {
      aout(self) << "Received a connect request from: " << hostname << "\n";
      
      self->monitor(client_actor);

      self->state.connected_nodes.push_back(client_actor);

      if (self->state.connected_nodes.size() == 
          distributed_settings.num_nodes) {

        aout(self) << "All Nodes Connected\n";

        for (int i = 0; i < distributed_settings.num_nodes; i++) {
          self->send(self->state.connected_nodes[i], start_job_v, 
                     std::get<0>(self->state.node_gru_ranges[i]), 
                     std::get<1>(self->state.node_gru_ranges[i]));
        }
      }
    },
    
    // Get the next forcing file in the simulation
    [=](new_forcing_file, int num_steps_in_iFile, int nextFile, 
        int num_timesteps) {
      self->state.messages_returned++;
      self->state.iFile = nextFile;
      self->state.stepsInCurrentFFile = num_steps_in_iFile;
      self->state.forcingStep = 1;
      if (self->state.num_steps == 0) {
        self->state.num_steps = num_timesteps;
        aout(self) << "Num Steps: " << self->state.num_steps << "\n";
      }

      if (self->state.messages_returned >= distributed_settings.num_nodes) {
        self->state.messages_returned = 0;
        self->send(self, update_hru_v);
      }
    },
    
    // Run the simulation for one timestep
    [=](update_hru) {
      for(auto node : self->state.connected_nodes) {
        self->send(node, update_hru_v);
      }
    },

    // Message from nodes when they have finished a timestep
    [=](done_update) {
      self->state.messages_returned++;
      if (self->state.messages_returned >= distributed_settings.num_nodes) {
        int steps_to_write = 1;

        for (auto node : self->state.connected_nodes) {
          self->send(node, write_output_v, steps_to_write);
        }

        self->state.messages_returned = 0;
      }
    },

    // Message from nodes when they have finished writing output
    [=](write_output) {
      self->state.messages_returned++;
      if (self->state.messages_returned >= distributed_settings.num_nodes) {
        aout(self) << "Distributed Job_Actor: Done timestep:" 
                   << self->state.timestep << "\n";
        
        self->state.timestep++;
        self->state.forcingStep++;
        self->state.messages_returned = 0;

        if (self->state.timestep > self->state.num_steps) {
          aout(self) << "Distributed Job_Actor: Done Simulation\n";
          for(auto node : self->state.connected_nodes) {
            self->send(node, finalize_v);
          }
          self->send(self, finalize_v);
        
        } else if(self->state.forcingStep > self->state.stepsInCurrentFFile) {
          aout(self) << "Distributed Job_Actor: Done Forcing File\n";
          for(auto node : self->state.connected_nodes) {
            self->send(node, access_forcing_v, self->state.iFile + 1);
          }        
        } else {
          self->send(self, update_hru_v);
        }
      }

    },

    [=](int err) {
      aout(self) << "Error in write_output\n";
    },

    [=](finalize) {
      aout(self) << "Simulation Finished\n";
      self->state.job_timing.updateEndPoint("total_duration");
      double total_dur_sec = self->state.job_timing.getDuration(
          "total_duration").value_or(-1.0);
      double total_dur_min = total_dur_sec / 60;
      double total_dur_hr = total_dur_min / 60;
      aout(self) << "Total Duration = " << total_dur_sec << " Seconds\n"
                 << "Total Duration = " << total_dur_min << " Minutes\n"
                 << "Total Duration = " << total_dur_hr << " Hours\n"
                 << "___________________Program Finished__________________\n";

      std::exit(0);
    }




  };
}



} // namespace caf