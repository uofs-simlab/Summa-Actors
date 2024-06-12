#include "job_actor.hpp"

using namespace caf;

void spawnHRUBatches(stateful_actor<job_state>* self) {
  aout(self) << "Job_Actor: Spawning HRU Batches\n";
  int batch_size;

  auto& gru_container = self->state.gru_container;
  gru_container.gru_start_time = std::chrono::high_resolution_clock::now();
  gru_container.run_attempts_left = self->state.job_actor_settings.max_run_attempts;
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
  int start_hru_global = self->state.batch.getStartHRU();
  int start_hru_local = 1;

  while (remaining_hru_to_batch > 0) {
    int current_batch_size = std::min(batch_size, remaining_hru_to_batch);
    auto gru_batch = self->spawn(hru_batch_actor, start_hru_local,
                                 start_hru_global, current_batch_size, 
                                 self->state.hru_actor_settings,
                                 self->state.file_access_actor, self);
    auto& job_settings = self->state.job_actor_settings;
    gru_container.gru_list.push_back(new GRU(start_hru_global, 
                                     start_hru_local, gru_batch, 
                                     self->state.dt_init_start_factor, 
                                     self->state.hru_actor_settings.rel_tol,
                                     self->state.hru_actor_settings.abs_tol, 
                                     job_settings.max_run_attempts));  

    remaining_hru_to_batch -= current_batch_size;
    start_hru_local += current_batch_size;
    start_hru_global += current_batch_size;
  }
  aout(self) << "Number of HRU_Batch_Actors: " 
             << gru_container.gru_list.size() << "\n";
}

