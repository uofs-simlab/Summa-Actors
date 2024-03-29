#include "job_actor.hpp"
#include "summa_actor.hpp"
#include "node_actor.hpp"

namespace caf {

behavior distributed_job_actor(stateful_actor<distributed_job_state>* self,
    int start_gru_global, int num_gru_global, 
    Distributed_Settings distributed_settings,
    File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, 
    HRU_Actor_Settings hru_actor_settings) {
      
  aout(self) << "Starting Distributed Job Actor\n";
  self->state.job_timing = TimingInfo();
  
  self->set_down_handler([=](const down_msg& dm){
    aout(self) << "Received Down Message\n.\n.\n.\n.\nExiting\n"; 
    exit(0);
  });
  
  self->state.start_gru = start_gru_global;
  self->state.num_gru = num_gru_global;
  
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
  aout(self) << "File GRU = " << self->state.file_gru << "\n";

  // Set up the node ranges
  int gru_per_node = (self->state.num_gru + distributed_settings.num_nodes - 1) 
      / distributed_settings.num_nodes;
  int remaining = self->state.num_gru;
  for (int i = 0; i < distributed_settings.num_nodes; i++) {
    int start_gru_local = i * gru_per_node + self->state.start_gru;
    int num_gru_local = gru_per_node;
    if (i == distributed_settings.num_nodes - 1) {
      num_gru_local = remaining;
    }
    remaining -= num_gru_local;

    bool use_global_for_data_structures = true;
    self->state.node_num_gru_info.push_back(
      NumGRUInfo(start_gru_local, self->state.start_gru, num_gru_local, 
          self->state.num_gru, self->state.file_gru,
          use_global_for_data_structures)
    );
  }

  // Print the node ranges
  for (int i = 0; i < distributed_settings.num_nodes; i++) {
    aout(self) << "Node " << i << " GRU Range: "
               << self->state.node_num_gru_info[i].start_gru_local << " - "
                << self->state.node_num_gru_info[i].num_gru_local << "\n"; 
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
        self->state.job_timing.addTimePoint("total_duration");
        self->state.job_timing.updateStartPoint("total_duration");

        for (int i = 0; i < distributed_settings.num_nodes; i++) {
          self->send(self->state.connected_nodes[i], start_job_v, 
              self->state.node_num_gru_info[i]);
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

    [=](load_balance) {

      self->state.load_balance_start_time = std::chrono::system_clock::now();

      // Find the node with the highest walltime from the map
      auto max_node = std::max_element(
          self->state.node_walltimes_map.begin(), 
          self->state.node_walltimes_map.end(),
          [](const std::pair<caf::actor, double>& p1, 
              const std::pair<caf::actor, double>& p2) {
            return p1.second < p2.second;
          });

      // Find the node with the lowest walltime from the map
      auto min_node = std::min_element(
          self->state.node_walltimes_map.begin(), 
          self->state.node_walltimes_map.end(),
          [](const std::pair<caf::actor, double>& p1, 
              const std::pair<caf::actor, double>& p2) {
            return p1.second < p2.second;
          });

      auto max_node_hru_times = 
          self->state.node_to_hru_map[max_node->first];
      auto min_node_hru_times =
          self->state.node_to_hru_map[min_node->first];

      // Find the 5 HRUs with the highest walltimes from the 
      // max_node_hru_times map
      std::vector<std::pair<caf::actor, double>> max_hru_times(
          max_node_hru_times.begin(), max_node_hru_times.end());
      std::sort(max_hru_times.begin(), max_hru_times.end(),
          [](const std::pair<caf::actor, double>& p1, 
              const std::pair<caf::actor, double>& p2) {
            return p1.second > p2.second;
          });
      
      std::vector<std::pair<caf::actor, double>> min_hru_times(
          min_node_hru_times.begin(), min_node_hru_times.end());
      std::sort(min_hru_times.begin(), min_hru_times.end(),
          [](const std::pair<caf::actor, double>& p1, 
              const std::pair<caf::actor, double>& p2) {
            return p1.second < p2.second;
          });

      // Get the 5 HRUs with the highest walltimes states from the 
      // max node
      for (int i = 0; i < 5; i++) {
        self->send(max_hru_times[i].first, serialize_hru_v);
      }

      // Get the 5 HRUs with the lowest walltimes states from the
      // min node
      for (int i = 0; i < 5; i++) {
        self->send(min_hru_times[i].first, serialize_hru_v);
      }

      self->state.num_serialize_messages_sent = 10;
    },

    [=] (caf::actor actor_ref, hru hru_data) {
      self->state.num_serialize_messages_received++;

      self->state.hrus_to_balance.push_back(
          std::make_pair(actor_ref, hru_data));

      if (self->state.num_serialize_messages_received >= 
          self->state.num_serialize_messages_sent) {

        // Redistribute the HRU data
        while(!self->state.hrus_to_balance.empty()) {
          // Find two HRUs that have different node owners
          auto hru1 = self->state.hrus_to_balance.back();
          self->state.hrus_to_balance.pop_back();
          auto node_1 = self->state.hru_to_node_map[hru1.first];
          for (auto hru2 : self->state.hrus_to_balance) {
            auto node_2 = self->state.hru_to_node_map[hru2.first];
            if (node_1 != node_2) {

              self->send(node_1, reinit_hru_v, hru1.first, hru2.second);
              self->send(node_2, reinit_hru_v, hru2.first, hru1.second);

              self->state.hrus_to_balance.erase(
                  std::remove_if(self->state.hrus_to_balance.begin(), 
                      self->state.hrus_to_balance.end(),
                      [&hru2](const auto& pair) { 
                          return pair.first == hru2.first; }
                  ),
                  self->state.hrus_to_balance.end()
              );
              break;
            }
          }
        }

        self->state.num_serialize_messages_received = 0;
      }
    },

    [=](reinit_hru) {
      self->state.num_serialize_messages_received++;

      if (self->state.num_serialize_messages_received >= 
          self->state.num_serialize_messages_sent) {
        
        self->state.num_serialize_messages_received = 0;
        self->state.num_serialize_messages_sent = 0;
        self->state.num_times_load_balanced++;
        self->state.load_balance_end_time = std::chrono::system_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
            self->state.load_balance_end_time - 
            self->state.load_balance_start_time);
        self->state.load_balance_time += duration.count();
        self->state.node_walltimes.clear();
        self->send(self, update_hru_v);
      }
    },

    // Message from nodes when they have finished a timestep
    [=](done_update, double node_walltime, 
        std::unordered_map<caf::actor, double> hru_walltimes) {
      self->state.messages_returned++;

      caf::actor sender = actor_cast<caf::actor>(self->current_sender());
      self->state.node_walltimes_map[sender] = node_walltime;
      self->state.node_to_hru_map[sender] = hru_walltimes;

      // self->state.node_walltimes.push_back(node_walltime);

      for (auto& hru_walltime : hru_walltimes) {
        self->state.hru_walltimes[hru_walltime.first] = hru_walltime.second;
      }

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

        bool load_balance = false;
        if (self->state.distributed_settings.load_balancing) {
          // Calculate the standard deviation of the node_wall_times_map
          for (auto& node_walltime : self->state.node_walltimes_map) {
            self->state.node_walltimes.push_back(node_walltime.second);
          }

          // Calculate the standard Deviation of the walltimes
          double sum = std::accumulate(self->state.node_walltimes.begin(), 
              self->state.node_walltimes.end(), 0.0);
          double mean = sum / self->state.node_walltimes.size();
          double sq_sum = std::inner_product(self->state.node_walltimes.begin(), 
              self->state.node_walltimes.end(), 
              self->state.node_walltimes.begin(), 0.0);
          double variance = 
              sq_sum / self->state.node_walltimes.size() - mean * mean;
          double stdev = std::sqrt(variance);
          
          double load_balancing_threshold = 0.005;
          if (stdev > load_balancing_threshold) {
            load_balance = true;
          }
        }
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
        } else if (load_balance) {
          self->send(self, load_balance_v);
        } else {
          self->send(self, update_hru_v);
        }
      }

    },

    [=](std::vector<caf::actor> hrus_from_node) {
      actor sender = actor_cast<actor>(self->current_sender());
      for (auto hru : hrus_from_node) {
        self->state.hru_to_node_map[hru] = sender;
      }
      self->state.hru_batch_maps_received++;
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
                 << "Number of Load Balances = " 
                 << self->state.num_times_load_balanced
                 << "\nTotal Load Balance Time = " 
                 << self->state.load_balance_time / 1000 << " Seconds"
                 << "\n___________________Program Finished__________________\n";
    }




  };
}



} // namespace caf