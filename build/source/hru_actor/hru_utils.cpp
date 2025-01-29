#include "hru_actor.hpp"

using namespace caf;


/*********************************************
 * HRU Actor Serialization Functions
 *********************************************/
void serializeHru(stateful_actor<hru_state>* self, hru& serialized_state) {
  // std::vector<std::vector<std::vector<double>>> lookup_struct = 
      // get_lookup_struct(self->state.hru_data);
  serialized_state.indx_hru = self->state.indx_hru;
  serialized_state.indx_gru = self->state.indx_gru;
  serialized_state.ref_gru = self->state.ref_gru;
  serialized_state.timestep = self->state.timestep;
  serialized_state.forcing_step = self->state.forcingStep;
  serialized_state.num_steps = self->state.num_steps;
  serialized_state.iFile = self->state.iFile;
  serialized_state.dt_init_factor = self->state.dt_init_factor;
  serialized_state.output_structure_step_index = 
      self->state.output_structure_step_index; 
  serialized_state.beSteps = self->state.beSteps;
  serialized_state.rtol = self->state.rtol;
  serialized_state.atolWat = self->state.atolWat;
  serialized_state.atolNrg = self->state.atolNrg;

  // Statistic Structures
  serialized_state.forc_stat = get_var_dlength_by_indx(self->state.hru_data, 1); 
  serialized_state.prog_stat = get_var_dlength_by_indx(self->state.hru_data, 2); 
  serialized_state.diag_stat = get_var_dlength_by_indx(self->state.hru_data, 3); 
  serialized_state.flux_stat = get_var_dlength_by_indx(self->state.hru_data, 4); 
  serialized_state.indx_stat = get_var_dlength_by_indx(self->state.hru_data, 5); 
  serialized_state.bvar_stat = get_var_dlength_by_indx(self->state.hru_data, 6);
  // Primary Data Structures (scalars)
  serialized_state.time_struct = get_var_i_by_indx(self->state.hru_data, 1);
  serialized_state.forc_struct = get_var_d_by_indx(self->state.hru_data, 1);
  serialized_state.attr_struct = get_var_d_by_indx(self->state.hru_data, 2);
  serialized_state.type_struct = get_var_i_by_indx(self->state.hru_data, 2);
  serialized_state.id_struct = get_var_i8_by_indx(self->state.hru_data, 1);
  // Primary Data Structures (variable length vectors)
  serialized_state.indx_struct = 
      get_var_ilength_by_indx(self->state.hru_data, 1);
  serialized_state.mpar_struct = 
      get_var_dlength_by_indx(self->state.hru_data, 7);      
  serialized_state.prog_struct = 
      get_var_dlength_by_indx(self->state.hru_data, 8);
  serialized_state.diag_struct = 
      get_var_dlength_by_indx(self->state.hru_data, 9);
  serialized_state.flux_struct = 
      get_var_dlength_by_indx(self->state.hru_data, 10);
  // Basin-average structures
  serialized_state.bpar_struct = get_var_d_by_indx(self->state.hru_data, 3);
  serialized_state.bvar_struct = 
      get_var_dlength_by_indx(self->state.hru_data, 11);
  serialized_state.dpar_struct = get_var_d_by_indx(self->state.hru_data, 4);

  // Local HRU data structures
  serialized_state.start_time = get_var_i_by_indx(self->state.hru_data, 3);
  serialized_state.end_time = get_var_i_by_indx(self->state.hru_data, 4);
  serialized_state.ref_time = get_var_i_by_indx(self->state.hru_data, 5);
  serialized_state.old_time = get_var_i_by_indx(self->state.hru_data, 6);

  // Statistic flags
  serialized_state.stat_counter = get_var_i_by_indx(self->state.hru_data, 7);
  serialized_state.output_timestep = get_var_i_by_indx(self->state.hru_data, 8);
  serialized_state.reset_stats = get_flagVec_by_indx(self->state.hru_data, 1);
  serialized_state.finalize_stats = get_flagVec_by_indx(self->state.hru_data, 2);

  get_scalar_data(self->state.hru_data, serialized_state.frac_jul_day,
                  serialized_state.tm_zone_offset_frac_day,
                  serialized_state.year_length,
                  serialized_state.compute_veg_flux,
                  serialized_state.dt_init,
                  serialized_state.upArea);
}

void deserializeHru(stateful_actor<hru_state>* self, hru& new_state) {
  setFinalizeStatsFalse(&self->state.indx_gru);

  // Delete the old hru_data in Fortran
  delete_handle_hru_type(self->state.hru_data);
  self->state.hru_data = new_handle_hru_type();

  self->state.indx_hru = new_state.indx_hru;
  self->state.indx_gru = new_state.indx_gru;
  self->state.ref_gru = new_state.ref_gru;
  self->state.timestep = new_state.timestep;
  self->state.forcingStep = new_state.forcing_step;
  self->state.num_steps = new_state.num_steps;
  self->state.iFile = new_state.iFile;
  self->state.dt_init_factor = new_state.dt_init_factor;
  self->state.output_structure_step_index = 
      new_state.output_structure_step_index;
  self->state.beSteps = new_state.beSteps;
  self->state.rtol = new_state.rtol;
  self->state.atolWat = new_state.atolWat;
  self->state.atolNrg = new_state.atolNrg;
  // Statistic Structures
  set_var_dlength_by_indx(self->state.hru_data, new_state.forc_stat, 1);
  set_var_dlength_by_indx(self->state.hru_data, new_state.prog_stat, 2);
  set_var_dlength_by_indx(self->state.hru_data, new_state.diag_stat, 3);
  set_var_dlength_by_indx(self->state.hru_data, new_state.flux_stat, 4);
  set_var_dlength_by_indx(self->state.hru_data, new_state.indx_stat, 5);
  set_var_dlength_by_indx(self->state.hru_data, new_state.bvar_stat, 6);
 // Primary Data Structures (scalars)
  set_var_i_by_indx(self->state.hru_data, new_state.time_struct, 1);
  set_var_d_by_indx(self->state.hru_data, new_state.forc_struct, 1);
  set_var_d_by_indx(self->state.hru_data, new_state.attr_struct, 2);
  set_var_i_by_indx(self->state.hru_data, new_state.type_struct, 2);
  set_var_i8_by_indx(self->state.hru_data, new_state.id_struct, 1);
  // Primary Data Structures (variable length vectors)
  set_var_ilength_by_indx(self->state.hru_data, new_state.indx_struct, 1);
  set_var_dlength_by_indx(self->state.hru_data, new_state.mpar_struct, 7);
  set_var_dlength_by_indx(self->state.hru_data, new_state.prog_struct, 8);
  set_var_dlength_by_indx(self->state.hru_data, new_state.diag_struct, 9);
  set_var_dlength_by_indx(self->state.hru_data, new_state.flux_struct, 10);
  // Basin-average structures
  set_var_d_by_indx(self->state.hru_data, new_state.bpar_struct, 3);
  set_var_dlength_by_indx(self->state.hru_data, new_state.bvar_struct, 11);
  set_var_d_by_indx(self->state.hru_data, new_state.dpar_struct, 4);
  // Local HRU data structures
  set_var_i_by_indx(self->state.hru_data, new_state.start_time, 3);
  set_var_i_by_indx(self->state.hru_data, new_state.end_time, 4);
  set_var_i_by_indx(self->state.hru_data, new_state.ref_time, 5);
  set_var_i_by_indx(self->state.hru_data, new_state.old_time, 6);
    // statistic flags
  set_var_i_by_indx(self->state.hru_data, new_state.stat_counter, 7);
  set_var_i_by_indx(self->state.hru_data, new_state.output_timestep, 8);
  set_flagVec_by_indx(self->state.hru_data, new_state.reset_stats, 1);
  set_flagVec_by_indx(self->state.hru_data, new_state.finalize_stats, 2);
  // scalar data
  set_scalar_data(self->state.hru_data, new_state.frac_jul_day,
                  new_state.tm_zone_offset_frac_day, new_state.year_length,
                  new_state.compute_veg_flux, new_state.dt_init, 
                  new_state.upArea);
}