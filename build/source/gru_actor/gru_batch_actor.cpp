#include "gru_batch_actor.hpp"
#include "gru_actor.hpp"

using namespace caf;

behavior GruBatchActor::make_behavior() {
  bool data_assimilation_mode = true;
  for (int i = 0; i < num_gru_; i++) {
    gru_actors_.push_back(self_->spawn(actor_from_state<GruActor>, 
        netcdf_start_index_ + i, job_start_index_ + i, 
        num_steps_, hru_actor_settings_, data_assimilation_mode, 
        num_steps_output_buffer_, file_access_actor_, self_, restart_, tolerance_settings_));  
  }
  return {
    [this](update_timeZoneOffset, int iFile) {
      for (auto& gru_actor : gru_actors_) {
        self_->mail(update_timeZoneOffset_v, iFile).send(gru_actor);
      }
    },
    [this](update_hru, int time_step, int forcing_step, int output_step) {
      for (auto& gru_actor : gru_actors_) {
        self_->mail(update_hru_v, time_step, forcing_step, output_step)
            .send(gru_actor);
      }
    },
    [this](done_update) {
      num_gru_done_++;
      if (num_gru_done_ >= gru_actors_.size()) {
        self_->mail(done_update_v).send(parent_);
        num_gru_done_ = 0;
      }
    },
  };
}

