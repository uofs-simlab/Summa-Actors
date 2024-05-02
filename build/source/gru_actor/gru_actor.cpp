#include "gru_actor.hpp"


using namespace caf;

behavior gru_actor(stateful_actor<gru_actor_state>* self, int netcdf_index, 
                   int gru_job_index, caf::actor parent) {
  self->state.parent = parent;
  self->state.netcdf_index = netcdf_index;
  self->state.gru_job_index = gru_job_index;

  // Check for lateral flows
  getHruInfo_fortran(self->state.gru_job_index, self->state.num_hrus);

  aout(self) << "NUM HRUS: " << self->state.num_hrus << "\n";
  
  self->state.hrus.resize(self->state.num_hrus);
  for (int i = 0; i < self->state.num_hrus; i++) {
    self->state.hrus[i] = new_handle_hru_type();
  }

  return {};
}