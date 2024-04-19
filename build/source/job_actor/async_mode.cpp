#include "job_actor.hpp"
namespace caf {

behavior async_mode(stateful_actor<job_state>* self) {
  aout(self) << "Async Mode Started\n";

  return {
    /*** From file access actor after it spawns ***/
    [=](init_file_access_actor, int num_timesteps) {
      aout(self) << "Async Mode: init_file_access_actor\n";
      spawnHRUActors(self, true);
      for(auto& gru : self->state.gru_container.gru_list) {
        self->send(gru->getGRUActor(), init_hru_v);
        self->send(gru->getGRUActor(), update_hru_async_v);
      }
    },

    [=](done_hru, int local_gru_index) {
      aout(self) << "HRU Done: " << local_gru_index << "\n";
      handleFinishedGRU(self, local_gru_index);
    },

    [=](finalize) { finalizeJob(self); },
    
  };
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




} // End of Namespace