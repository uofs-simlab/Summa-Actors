#include "gru_batch_actor.hpp"
#include "gru_actor.hpp"

using namespace caf;

behavior GruBatchActor::make_behavior() {
  bool data_assimilation_mode = true;
  for (int i = 0; i < num_gru_; i++) {
    gru_actors_.push_back(self_->spawn(actor_from_state<GruActor>, 
                                       netcdf_start_index_ + i, 
                                       job_start_index_ + i, 
                                       num_steps_, hru_actor_settings_, 
                                       data_assimilation_mode, 
                                       num_steps_output_buffer_,
                                       file_access_actor_, self_));  
  }
  return {
    [this](update_timeZoneOffset, int iFile) {
      for (auto& gru_actor : gru_actors_) {
        self_->mail(update_timeZoneOffset_v, iFile).send(gru_actor);
      }
    },
    [this](update_hru, int timestep, int forcing_step) {
      for (auto& gru_actor : gru_actors_) {
        self_->mail(update_hru_v, timestep, forcing_step).send(gru_actor);
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

// namespace caf {





//     [=](serialize_hru, int indx_gru) {
//       self->send(self->state.hru_actors[indx_gru], serialize_hru_v);
//     },

//     [=](serialize_hru, caf::actor actor_ref) {
//       self->send(actor_ref, serialize_hru_v);
//     },

//     [=](hru hru_data) {
//       auto sender = actor_cast<actor>(self->current_sender());
//       self->send(self->state.parent, sender, hru_data);
//     },

//     [=](reinit_hru, hru hru_data) {
//       self->state.hru_actors.push_back(
//         self->spawn(hru_actor, hru_data.ref_gru, hru_data.indx_gru,
//             self->state.hru_actor_settings, self->state.file_access_actor, 
//             self)
//       );
//       self->send(self->state.hru_actors.back(), reinit_hru_v, hru_data);
//     },
 
//     [=](reinit_hru, caf::actor target_actor, hru hru_data) {
//       self->send(target_actor, reinit_hru_v, hru_data);
//     },

//     [=](reinit_hru) {
//       self->send(self->state.parent, reinit_hru_v);
//     }
//   };

// }
                            


// }