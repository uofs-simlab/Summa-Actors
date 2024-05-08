#include "job_actor.hpp"

using namespace caf;

behavior data_assimilation_mode(stateful_actor<job_state>* self) {
  aout(self) << "Data Assimilation Mode: Started\n";

  return {
    [=](file_access_actor_ready, int num_timesteps) {
      aout(self) << "Data Assimilation Mode: init_file_access_actor\n";
      self->state.num_steps = num_timesteps;
      self->state.job_actor_settings.batch_size > 0 ? 
          spawnHRUBatches(self) : spawnHRUActors(self);
      aout(self) << "Data Assimilation Mode: GRUs Initalized\n";
      self->send(self->state.file_access_actor, access_forcing_v,
          self->state.iFile, self);
    },

    [=](new_forcing_file, int num_steps_in_iFile, int nextFile) {
      aout(self) << "Data Assimilation Mode: New Forcing File\n";
      self->state.iFile = nextFile;
      self->state.stepsInCurrentFFile = num_steps_in_iFile;
      self->state.forcingStep = 1;
      for(auto gru : self->state.gru_container.gru_list) {
        self->send(gru->getActorRef(), update_timeZoneOffset_v, 
            self->state.iFile);
      }

      self->send(self, update_hru_v);
    },

    [=](update_hru){
      for(auto gru : self->state.gru_container.gru_list) {
        self->send(gru->getActorRef(), update_hru_v, self->state.timestep, 
            self->state.forcingStep);
      }      
    },

    [=](done_update, std::unordered_map<caf::actor, double> walltimes){
      self->state.num_gru_done_timestep++;
      
      if (self->state.num_gru_done_timestep >= 
          self->state.gru_container.gru_list.size()) {
        aout(self) << "Job_Actor: Done Update for timestep:" 
                   << self->state.timestep << "\n";

        // write the output
        int steps_to_write = 1;
        int start_gru = 1;
        self->request(self->state.file_access_actor, caf::infinite,
          write_output_v, steps_to_write, start_gru, self->state.num_gru).await(
          [=](int err) {
            if (err != 0) {
              aout(self) << "Job_Actor: Error Writing Output\n";
              for (auto GRU : self->state.gru_container.gru_list)
                self->send(GRU->getActorRef(), exit_msg_v);
              
              self->send_exit(self->state.file_access_actor, 
                              exit_reason::user_shutdown);
              self->quit();
            } 
          });

        self->state.timestep++;
        self->state.forcingStep++;

        // Check if we are done the simulation
        if (self->state.timestep > self->state.num_steps) {
          aout(self) << "Job_Actor: Done Job\n";
          for (auto GRU : self->state.gru_container.gru_list) {
            self->send_exit(GRU->getActorRef(), exit_reason::user_shutdown);
            GRU->setSuccess();
          }
          self->send(self, finalize_v);
        } else if (self->state.forcingStep > self->state.stepsInCurrentFFile) {
        // Check if we need another forcing file
          aout(self) << "Job_Actor: Done Forcing Step\n";
          self->send(self->state.file_access_actor, access_forcing_v, 
              self->state.iFile+1, self);
        } else {
          // otherwise update all HRUs
          self->send(self, update_hru_v);
        }
        self->state.num_gru_done_timestep = 0;
      }
    },

    [=](std::vector<actor> hru_actors) {},

    [=](serialize_hru, hru hru_data) {
      aout(self) << "Job_Actor: Recieved HRU Data\n";
      auto sender = actor_cast<actor>(self->current_sender());

      self->send(sender, reinit_hru_v, hru_data);
    },


    [=](finalize) { finalizeJob(self); },
  

  };
}