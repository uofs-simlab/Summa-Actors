#pragma once
#include "caf/all.hpp"
#include "fortran_data_types.hpp"
#include <vector>

extern "C" {
  void getHruInfo_fortran(int& index_gru, int& num_hru);
}

struct gru_actor_state {
  caf::actor parent;
  int netcdf_index;
  int gru_job_index;

  int num_hrus;

  std::vector<void*> hrus;
};

caf::behavior gru_actor(caf::stateful_actor<gru_actor_state>* self, 
                        int netcdf_index, int gru_job_index, caf::actor parent);