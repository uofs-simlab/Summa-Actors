#include "node_actor.hpp"

namespace caf {

behavior node_actor(stateful_actor<node_state>* self, std::string host, 
    actor parent, Distributed_Settings distributed_settings,
    File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, 
    HRU_Actor_Settings hru_actor_settings) {

  aout(self) << "Starting Node Actor\n";
  self->state.node_timing = TimingInfo();
  self->state.node_timing.addTimePoint("total_duration");
  self->state.node_timing.updateStartPoint("total_duration");

  self->set_down_handler([=](const down_msg& dm){
    aout(self) << "Received Down Message\n.\n.\n.\n.\nExiting\n"; 
    exit(0);
  });

  self->state.max_run_attempts = job_actor_settings.max_run_attempts;


  self->state.distributed_settings = distributed_settings;
  self->state.file_access_actor_settings = file_access_actor_settings;
  self->state.job_actor_settings = job_actor_settings;
  self->state.hru_actor_settings = hru_actor_settings;

  if (!host.empty()) {
    auto server = self->system().middleman().remote_actor(host, 
                                           distributed_settings.port);
    if (!server) {
      aout(self) << "Failed To Connect To Server\n"; 
      return {};
    }

    self->state.current_server = *server;
    self->monitor(self->state.current_server);

  } else {
    self->state.current_server = parent;
  }

  char hostname[HOST_NAME_MAX];
  gethostname(hostname, HOST_NAME_MAX);
  self->state.hostname = hostname;
  self->send(self->state.current_server, connect_to_server_v, self, 
      self->state.hostname);


  
  return {
    [=](start_job, NumGRUInfo num_gru_info) {
      self->state.num_gru_info = num_gru_info;
      aout(self) << "Recieved Start Job Message\n"
                 << "Start GRU Local: " 
                 << self->state.num_gru_info.start_gru_local
                 << " -- Start GRU Global:"
                 << self->state.num_gru_info.start_gru_global
                 << " -- Num GRU Local: "
                 << self->state.num_gru_info.num_gru_local
                 << " -- Num GRU Global: "
                 << self->state.num_gru_info.num_gru_global
                 << " -- File GRU: " << self->state.num_gru_info.file_gru 
                 << "\n";
                 
                      
      // self->state.start_gru = start_gru;
      // self->state.num_gru_local = num_gru_local;
      // self->state.num_gru_global = num_gru_global;
      self->state.gru_container.num_gru_in_run_domain 
          = self->state.num_gru_info.num_gru_local;
      

      int start_gru, num_gru, num_hru;
      if (self->state.num_gru_info.use_global_for_data_structures) {
        start_gru = self->state.num_gru_info.start_gru_global;
        num_gru = self->state.num_gru_info.num_gru_global;
        num_hru = self->state.num_gru_info.num_gru_global;
      } else {
        start_gru = self->state.num_gru_info.start_gru_local;
        num_gru = self->state.num_gru_info.num_gru_local;
        num_hru = self->state.num_gru_info.num_gru_local;
      }


      int err, file_gru_to_remove;
      job_init_fortran(self->state.job_actor_settings.file_manager_path.c_str(),
          &start_gru, &num_gru, &num_hru, &file_gru_to_remove, &err);
      if (err != 0) { 
        aout(self) << "\nERROR: Job_Actor - job_init_fortran\n"; 
        self->quit();
        return;
      }
      // Spawn the file_access_actor.
      self->state.file_access_actor = self->spawn(file_access_actor, 
          self->state.num_gru_info, self->state.file_access_actor_settings, 
          self);
      self->monitor(self->state.file_access_actor);
      self->send(self->state.file_access_actor, def_output_v, 
          self->state.num_gru_info.file_gru);
    },

    [=](init_file_access_actor, int num_timesteps) {
      aout(self) << "Num Steps: " << num_timesteps << "\n";
      self->state.num_steps = num_timesteps;
      auto& gru_container = self->state.gru_container;
        
      // Spawn HRUs in batches or individually
      if (self->state.job_actor_settings.batch_size > 1)
        spawnHRUBatches(self);
      else
        spawnHRUActors(self, false);
      

      aout(self) << "GRUs Initialized\n";
      self->send(self->state.file_access_actor, access_forcing_v, 
                  self->state.iFile, self);
    },

    [=](access_forcing, int new_iFile) {
      self->send(self->state.file_access_actor, access_forcing_v, new_iFile, 
                 self);
    },

    [=](new_forcing_file, int num_steps_in_iFile, int nextFile) {
      aout(self) << "Received New Forcing File\n";
      self->state.iFile = nextFile;
      self->state.stepsInCurrentFFile = num_steps_in_iFile;
      self->state.forcingStep = 1;
      for(auto gru : self->state.gru_container.gru_list) {
        self->send(gru->getGRUActor(), update_timeZoneOffset_v, 
            self->state.iFile);
      }
      self->send(self->state.current_server, new_forcing_file_v, 
                 self->state.stepsInCurrentFFile, self->state.iFile, 
                 self->state.num_steps);
    },

    [=](update_hru) {
      for (auto gru : self->state.gru_container.gru_list) {
        self->send(gru->getGRUActor(), update_hru_v,
                   self->state.timestep, self->state.forcingStep);
      }
    },

    [=](done_update) {
      self->state.num_gru_done_timestep++;
      if (self->state.num_gru_done_timestep >= 
          self->state.gru_container.gru_list.size()) {

        aout(self) << "Node Actor: Done Update for timestep:" 
                   << self->state.timestep << "\n";

        self->state.timestep++;
        self->state.forcingStep++;
        self->state.num_gru_done_timestep = 0;

        self->send(self->state.current_server, done_update_v);

      }
    },

    [=](write_output, int steps_to_write) {
      
      int num_gru_to_write;
      if (self->state.num_gru_info.use_global_for_data_structures) {
        num_gru_to_write = self->state.num_gru_info.num_gru_global;
      } else {
        num_gru_to_write = self->state.num_gru_info.num_gru_local;
      }

      self->request(self->state.file_access_actor, infinite, write_output_v, 
          steps_to_write, 1, num_gru_to_write).await(
          [=](int err) {
            if (err != 0) {
              aout(self) << "Error Writing Output\n";
              self->send_exit(self->state.file_access_actor, 
                  exit_reason::user_shutdown);
              self->send(self->state.current_server, err);
              // self->quit();
            }
          });

      self->send(self->state.current_server, write_output_v);
    },

    [=](finalize) {
      aout(self) << "Done Simulation\n";
      self->state.node_timing.updateEndPoint("total_duration");
      double total_duration = self->state.node_timing.getDuration(
          "total_duration").value_or(-1.0);
      double total_dur_min = total_duration / 60;
      double total_dur_hr = total_dur_min / 60;
      aout(self) << "Total Duration: " << total_duration << " seconds\n"
                 << "Total Duration: " << total_dur_min << " minutes\n"
                 << "Total Duration: " << total_dur_hr << " hours\n"
                 << "___________________Node Finished__________________\n";
    
      std::exit(0);
    }
  };
}

void spawnHRUActors(stateful_actor<node_state>* self, bool normal_mode) {
  auto& gru_container = self->state.gru_container;
  gru_container.gru_start_time = std::chrono::high_resolution_clock::now();
  gru_container.run_attempts_left = self->state.max_run_attempts;
  gru_container.run_attempts_left--;

  for (int i = 0; i < gru_container.num_gru_in_run_domain; i++) {
    auto global_gru_index = gru_container.gru_list.size() 
                            + self->state.start_gru;
    auto local_gru_index = gru_container.gru_list.size() + 1;                                

    auto gru = self->spawn(hru_actor, global_gru_index, local_gru_index,               
                           self->state.hru_actor_settings,                                
                           self->state.file_access_actor, self);

    // Create the GRU object (Job uses this to keep track of GRU status)
    gru_container.gru_list.push_back(new GRU(global_gru_index, 
                                     local_gru_index, gru, 
                                     self->state.dt_init_start_factor, 
                                     self->state.hru_actor_settings.rel_tol,
                                     self->state.hru_actor_settings.abs_tol,
                                     self->state.max_run_attempts));  
    
    if (normal_mode) self->send(gru, update_hru_async_v);
  }                        
          
}

void spawnHRUBatches(stateful_actor<node_state>* self) {
  int batch_size;

  auto& gru_container = self->state.gru_container;
  gru_container.gru_start_time = std::chrono::high_resolution_clock::now();
  gru_container.run_attempts_left = self->state.max_run_attempts;
  gru_container.run_attempts_left--;

  if (self->state.job_actor_settings.batch_size == 9999) {
    batch_size = std::ceil(gru_container.num_gru_in_run_domain / 
                           (std::thread::hardware_concurrency() * 2));
  } else {
    batch_size = self->state.job_actor_settings.batch_size;
  }

  int remaining_hru_to_batch = gru_container.num_gru_in_run_domain;
  int start_hru_global = self->state.start_gru;
  int start_hru_local = 1;

  while (remaining_hru_to_batch > 0) {
    int current_batch_size = std::min(batch_size, remaining_hru_to_batch);
    auto gru_batch = self->spawn(hru_batch_actor, start_hru_local,
                                 start_hru_global, current_batch_size,
                                  self->state.hru_actor_settings,
                                 self->state.file_access_actor, self);

    gru_container.gru_list.push_back(new GRU(start_hru_global, 
                                    start_hru_local, gru_batch, 
                                    self->state.dt_init_start_factor, 
                                    self->state.hru_actor_settings.rel_tol,
                                    self->state.hru_actor_settings.abs_tol,
                                    self->state.max_run_attempts));  

    remaining_hru_to_batch -= current_batch_size;
    start_hru_local += current_batch_size;
    start_hru_global += current_batch_size;
  }
  aout(self) << "Number of HRU_Batch_Actors: " 
             << gru_container.gru_list.size() << "\n";
}

} // namespace caf