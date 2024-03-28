#include "hru_batch_actor.hpp"

namespace caf {

behavior hru_batch_actor(stateful_actor<hru_batch_state>* self,
    int start_gru_local, int start_gru_global, int num_gru,
    HRU_Actor_Settings hru_actor_settings, caf::actor file_access_actor, 
    caf::actor parent) {
  
  self->state.file_access_actor = file_access_actor;
  self->state.parent = parent;
  self->state.hru_actor_settings = hru_actor_settings;

  self->state.hru_walltimes.resize(num_gru);
  
  for (int i = 0; i < num_gru; i++) {
    self->state.hru_actors.push_back(
      self->spawn(hru_actor, start_gru_global + i, start_gru_local + i, 
          hru_actor_settings, file_access_actor, self)
    );
    self->send(self->state.hru_actors.back(), init_hru_v);
  }

  self->send(self->state.parent, self->state.hru_actors);

  return {
    [=](update_timeZoneOffset, int iFile) {
      for (auto& hru_actor : self->state.hru_actors) {
        self->send(hru_actor, update_timeZoneOffset_v, iFile);
      }
    },

    [=](update_hru, int timestep, int forcingstep) {
      for (auto& hru_actor : self->state.hru_actors) {
        self->send(hru_actor, update_hru_v, timestep, forcingstep);
      }
    },

    [=](done_update, double walltime_timestep, int indx_gru) {
      // int indx = indx_gru % self->state.hru_walltimes.size();
      // self->state.hru_walltimes[indx] = walltime_timestep;
      
      caf::actor sender = actor_cast<caf::actor>(self->current_sender());
      self->state.hru_actor_walltimes[sender] = walltime_timestep;


      self->state.num_done++;
      if (self->state.num_done == self->state.hru_actors.size()) {
        self->send(self->state.parent, done_update_v, 
            self->state.hru_actor_walltimes);
        self->state.num_done = 0;
      }
    },

    [=](serialize_hru, int indx_gru) {
      self->send(self->state.hru_actors[indx_gru], serialize_hru_v);
    },

    [=](serialize_hru, caf::actor actor_ref) {
      self->send(actor_ref, serialize_hru_v);
    },

    [=](hru hru_data) {
      aout(self) << "HRU_Batch_Actor: Recieved HRU Data\n";
      auto sender = actor_cast<actor>(self->current_sender());
      // self->send_exit(sender, exit_reason::user_shutdown);
      // self->state.hru_actors.erase(
      //   std::remove(self->state.hru_actors.begin(), 
      //       self->state.hru_actors.end(), sender),
      //   self->state.hru_actors.end()
      // );


      self->send(self->state.parent, sender, hru_data);
    },

    [=](reinit_hru, hru hru_data) {
      aout(self) << "HRU_Batch_Actor: Re-initializing HRU\n";
      self->state.hru_actors.push_back(
        self->spawn(hru_actor, hru_data.ref_gru, hru_data.indx_gru,
            self->state.hru_actor_settings, self->state.file_access_actor, 
            self)
      );
      self->send(self->state.hru_actors.back(), reinit_hru_v, hru_data);
    },
 
    [=](reinit_hru, caf::actor target_actor, hru hru_data) {
      self->send(target_actor, reinit_hru_v, hru_data);
    },

    [=](reinit_hru) {
      aout(self) << "HRU_Batch_Actor: HRU Re-Initialized\n";
      self->send(self->state.parent, reinit_hru_v);
    }
  };

}
                            


}