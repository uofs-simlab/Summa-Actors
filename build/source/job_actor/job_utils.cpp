#include "job_actor.hpp"

namespace caf {

void spawnHRUActors(stateful_actor<job_state>* self) {
  auto& gru_container = self->state.gru_container;
  gru_container.gru_start_time = std::chrono::high_resolution_clock::now();
  gru_container.run_attempts_left = self->state.max_run_attempts;
  gru_container.run_attempts_left--;

  for (int i = 0; i < gru_container.num_gru_in_run_domain; i++) {
    auto global_gru_index = gru_container.gru_list.size() + 
        self->state.start_gru;
    auto local_gru_index = gru_container.gru_list.size() + 1;                                

    auto gru = self->spawn(hru_actor, global_gru_index, local_gru_index,               
        self->state.hru_actor_settings, self->state.file_access_actor, self);

    // Create the GRU object (Job uses this to keep track of GRU status)
    gru_container.gru_list.push_back(new GRU(global_gru_index, 
        local_gru_index, gru, self->state.dt_init_start_factor, 
        self->state.hru_actor_settings.rel_tol,
        self->state.hru_actor_settings.abs_tol, self->state.max_run_attempts));  
    
    // if (normal_mode) self->send(gru, update_hru_async_v);
  }                        
          
}

void spawnHRUBatches(stateful_actor<job_state>* self) {
  aout(self) << "Job_Actor: Spawning HRU Batches\n";
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

  // Correct when number of batches is greater than number of HRUs
  if (batch_size == 0) {
    batch_size = 1; 
  }

  // Correct if number of GRUs is less than the desired batch size
  aout(self) << "Job_Actor: Batch Size=" << batch_size << "\n";

  int remaining_hru_to_batch = gru_container.num_gru_in_run_domain;
  int start_hru_global = self->state.start_gru;
  int start_hru_local = 1;

  while (remaining_hru_to_batch > 0) {
    int current_batch_size = std::min(batch_size, remaining_hru_to_batch);
    auto gru_batch = self->spawn(hru_batch_actor, start_hru_local,
        start_hru_global, current_batch_size, self->state.hru_actor_settings,
        self->state.file_access_actor, self);

    gru_container.gru_list.push_back(new GRU(start_hru_global, 
        start_hru_local, gru_batch, self->state.dt_init_start_factor, 
        self->state.hru_actor_settings.rel_tol,
        self->state.hru_actor_settings.abs_tol, self->state.max_run_attempts));  

    remaining_hru_to_batch -= current_batch_size;
    start_hru_local += current_batch_size;
    start_hru_global += current_batch_size;
  }
  aout(self) << "Number of HRU_Batch_Actors: " 
             << gru_container.gru_list.size() << "\n";
}


void finalizeJob(stateful_actor<job_state>* self) {
  std::vector<serializable_netcdf_gru_actor_info> netcdf_gru_info = 
      getGruNetcdfInfo(
          self->state.max_run_attempts,self->state.gru_container.gru_list);
  
  self->state.num_gru_failed = std::count_if(netcdf_gru_info.begin(), 
      netcdf_gru_info.end(), [](auto& gru_info) {
    return !gru_info.successful;
  });

  self->request(self->state.file_access_actor, infinite, finalize_v).await(
    [=](std::tuple<double, double> read_write_duration) {
      int err = 0;
      for (auto GRU : self->state.gru_container.gru_list) 
        delete GRU;
      self->state.gru_container.gru_list.clear();
      self->state.job_timing.updateEndPoint("total_duration");
      aout(self) << "\n________________" 
                  << "PRINTING JOB_ACTOR TIMING INFO RESULTS"
                  << "________________\n"
                  << "Total Duration = "
                  << self->state.job_timing.getDuration("total_duration")
                      .value_or(-1.0) << " Seconds\n"
                  << "Total Duration = " 
                  << self->state.job_timing.getDuration("total_duration")
                      .value_or(-1.0) / 60 << " Minutes\n"
                  << "Total Duration = " 
                  << (self->state.job_timing.getDuration("total_duration")
                    .value_or(-1.0) / 60) / 60 << " Hours\n"
                  << "Job Init Duration = " 
                  << self->state.job_timing.getDuration("init_duration")
                      .value_or(-1.0) << " Seconds\n"
                  << "_________________________________" 
                  << "_______________________________________\n\n";
      
      deallocateJobActor(&err);
      
      // Tell Parent we are done
      self->send(self->state.parent, done_job_v, self->state.num_gru_failed, 
          self->state.job_timing.getDuration("total_duration").value_or(-1.0),
          std::get<0>(read_write_duration), 
          std::get<1>(read_write_duration));
      self->quit();
    });
}

void handleFinishedGRU(stateful_actor<job_state>* self, int local_gru_index) {
  using namespace std::chrono;
  auto& gru_container = self->state.gru_container;
  chrono_time end_point = high_resolution_clock::now();
  double total_duration = duration_cast<seconds>(end_point - 
      gru_container.gru_start_time).count();
  gru_container.num_gru_done++;

  aout(self) << "GRU Finished: " << gru_container.num_gru_done << "/" 
             << gru_container.num_gru_in_run_domain << " -- GlobalGRU=" 
             << gru_container.gru_list[local_gru_index-1]->getGlobalGRUIndex()
             << " -- LocalGRU=" << local_gru_index << "\n";

  gru_container.gru_list[local_gru_index-1]->setRunTime(total_duration);
  gru_container.gru_list[local_gru_index-1]->setInitDuration(-1);
  gru_container.gru_list[local_gru_index-1]->setForcingDuration(-1);
  gru_container.gru_list[local_gru_index-1]->setRunPhysicsDuration(-1);
  gru_container.gru_list[local_gru_index-1]->setWriteOutputDuration(-1);
  gru_container.gru_list[local_gru_index-1]->setSuccess();


  // Check if all GRUs are done
  if (gru_container.num_gru_done >= gru_container.num_gru_in_run_domain) {
    if(gru_container.num_gru_failed == 0 || self->state.max_run_attempts == 1)
      self->send(self, finalize_v);
    else
      self->send(self, restart_failures_v);
  }
}
} // End of Namespace caf