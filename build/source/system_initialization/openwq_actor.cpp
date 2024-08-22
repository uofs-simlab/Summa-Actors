#include "openwq_actor.hpp"

namespace caf {

behavior openwq_actor(stateful_actor<openwq_actor_state>* self,
int start_gru, int num_gru) {
    // initialize openWQ object

self->state.start_gru = start_gru;
self->state.num_gru = num_gru;
                  self->state.global_openwq_state = std::make_unique<openWQGlobalData>();

    self->state.hru_timesteps.resize(self->state.num_gru, -1);
    self->state.ran_steps = 0;



    return {
        [=] (openwq_initialize) {
           auto err = self->state.global_openwq_state->defineGlobalData();
    if (err != 0) {
      aout(self) << "ERROR--Global State: Unable To Define Global Data\n";
      self->quit();
    }

        },
        [=] (start_step_openwq, int hru, int gru, int timestep) {


            // Run start space step for relevant hru/gru/timestep
        },
        [=] (space_step_openwq, int hru, int timestep, int idx) {
            int gru_index = abs(self->state.start_gru - hru); 
            self->state.hru_timesteps[gru_index] = timestep;

            int slowest_timestep = *std::min_element(
          self->state.hru_timesteps.begin(), 
          self->state.hru_timesteps.end()); 

          if (slowest_timestep > self->state.ran_steps) {
            self->state.ran_steps = slowest_timestep;
            openwq_start_step_fortran(idx);
            openwq_space_step_fortran(idx);
            openwq_space_step_end_fortran(idx);
          }


            // Run runSpaceStep for relevant info.
            // Check if we can run spaceTimeEnd for relevant 
        },
        [=] (time_to_exit) {
            self->quit();
        }
    };
}
}