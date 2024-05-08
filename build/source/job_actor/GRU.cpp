#include "GRU.hpp"

GRU::GRU(int index_netcdf, int index_job, caf::actor actor_ref, 
         int dt_init_factor, double rel_tol, double abs_tol, int max_attempt) {
  index_netcdf_ = index_netcdf;
  index_job_ = index_job;
  actor_ref_ = actor_ref;
  dt_init_factor_ = dt_init_factor;
  rel_tol_ = rel_tol;
  abs_tol_ = abs_tol;
  attempts_left_ = max_attempt;
  state_ = gru_state::running;
}

GRU::~GRU() {};
