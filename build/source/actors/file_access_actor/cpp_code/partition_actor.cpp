#include "partition_actor.hpp"


namespace caf {

behavior partition_actor(stateful_actor<partition_actor_state>* self, int start_local_gru_index, 
    int num_local_grus, int num_timesteps_simulation, int num_timesteps_buffer) {
    
    self->state.start_local_gru_index = start_local_gru_index;
    self->state.num_local_grus = num_local_grus;
    self->state.num_timesteps_simulation = num_timesteps_simulation;
    self->state.num_timesteps_buffer = num_timesteps_buffer;
    
    
    return {
    };
}
}