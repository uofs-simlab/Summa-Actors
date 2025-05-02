#pragma once
#include <vector>

// Serialization HRU Stucture
struct hru {
  int indx_hru;
  int indx_gru;
  int ref_gru;

  // Misc Variables
  int timestep;
  int forcing_step;
  int num_steps;
  int iFile;
  int dt_init_factor;
  int output_structure_step_index;

  // Sundials variables
  int beSteps;
  double rtol;
  double atol;
  double rtol_temp_cas;
  double atol_temp_cas;
  double rtol_temp_veg;
  double atol_temp_veg;
  double rtol_temp_soil_snow;
  double atol_temp_soil_snow;
  double rtol_wat_veg;
  double atol_wat_veg;
  double rtol_wat_snow;
  double atol_wat_snow;
  double rtol_matric;
  double atol_matric;
  double rtol_aquifr;
  double atol_aquifr;
  bool def_tol;
  // TODO: Ashley's New Variables
  double atolWat;
  double atolNrg;

  // HRU data structures
  // Statistic Structure
  std::vector<std::vector<double>> forc_stat;
  std::vector<std::vector<double>> prog_stat;
  std::vector<std::vector<double>> diag_stat;
  std::vector<std::vector<double>> flux_stat;
  std::vector<std::vector<double>> indx_stat;
  std::vector<std::vector<double>> bvar_stat;
  // Primary Data Strutures (scalars)
  std::vector<int> time_struct;
  std::vector<double> forc_struct;
  std::vector<double> attr_struct;
  std::vector<int> type_struct;
  std::vector<long int> id_struct;
  // Primary Data Structures (arrays)
  std::vector<std::vector<int>> indx_struct;
  std::vector<std::vector<double>> mpar_struct;
  std::vector<std::vector<double>> prog_struct;
  std::vector<std::vector<double>> diag_struct;
  std::vector<std::vector<double>> flux_struct;
  // Basin-average structures
  std::vector<double> bpar_struct;
  std::vector<std::vector<double>> bvar_struct;
  std::vector<double> dpar_struct;
  // Local HRU data structures
  std::vector<int> start_time;
  std::vector<int> end_time;
  std::vector<int> ref_time;
  std::vector<int> old_time;
  // statistic flags
  std::vector<int> stat_counter;
  std::vector<int> output_timestep;
  std::vector<int> reset_stats;
  std::vector<int> finalize_stats;

  // scalar data
  double frac_jul_day;
  double tm_zone_offset_frac_day;
  int year_length;
  int compute_veg_flux;
  double dt_init;
  double upArea;
};

template <class Inspector>
bool inspect(Inspector& inspector, hru& hru_data) {
  return inspector.object(hru_data).fields(
         inspector.field("indx_hru", hru_data.indx_hru),
         inspector.field("indx_gru", hru_data.indx_gru),
         inspector.field("ref_gru", hru_data.ref_gru),
         inspector.field("timestep", hru_data.timestep),
         inspector.field("forcing_step", hru_data.forcing_step),
         inspector.field("num_steps", hru_data.num_steps),
         inspector.field("iFile", hru_data.iFile),
         inspector.field("dt_init_factor", hru_data.dt_init_factor),
         inspector.field("output_structure_step_index", 
                         hru_data.output_structure_step_index),
         inspector.field("beSteps", hru_data.beSteps),
         inspector.field("rtol", hru_data.rtol),
         inspector.field("atol", hru_data.atol),
         inspector.field("rtol_temp_cas", hru_data.rtol_temp_cas),
         inspector.field("rtol_temp_veg", hru_data.rtol_temp_veg),
         inspector.field("rtol_wat_veg", hru_data.rtol_wat_veg),
         inspector.field("rtol_temp_soil_snow", hru_data.rtol_temp_soil_snow),
         inspector.field("rtol_wat_snow", hru_data.rtol_wat_snow),
         inspector.field("rtol_matric", hru_data.rtol_matric),
         inspector.field("rtol_aquifr", hru_data.rtol_aquifr),
         inspector.field("atol_temp_cas", hru_data.atol_temp_cas),
         inspector.field("atol_temp_veg", hru_data.atol_temp_veg),
         inspector.field("atol_wat_veg", hru_data.atol_wat_veg),
         inspector.field("atol_temp_soil_snow", hru_data.atol_temp_soil_snow),
         inspector.field("atol_wat_snow", hru_data.atol_wat_snow),
         inspector.field("atol_matric", hru_data.atol_matric),
         inspector.field("atol_aquifr", hru_data.atol_aquifr),
         inspector.field("def_tol", hru_data.def_tol),
         
         // TODO: Ashley's New Variables
         inspector.field("atolWat", hru_data.atolWat),
         inspector.field("atolNrg", hru_data.atolNrg),
         
         inspector.field("forc_stat", hru_data.forc_stat),
         inspector.field("prog_stat", hru_data.prog_stat),
         inspector.field("diag_stat", hru_data.diag_stat),
         inspector.field("flux_stat", hru_data.flux_stat),
         inspector.field("indx_stat", hru_data.indx_stat),
         inspector.field("bvar_stat", hru_data.bvar_stat),
         inspector.field("time_struct", hru_data.time_struct),
         inspector.field("forc_struct", hru_data.forc_struct),
         inspector.field("attr_struct", hru_data.attr_struct),
         inspector.field("type_struct", hru_data.type_struct),
         inspector.field("id_struct", hru_data.id_struct),
         inspector.field("indx_struct", hru_data.indx_struct),
         inspector.field("mpar_struct", hru_data.mpar_struct),
         inspector.field("prog_struct", hru_data.prog_struct),
         inspector.field("diag_struct", hru_data.diag_struct),
         inspector.field("flux_struct", hru_data.flux_struct),
         inspector.field("bpar_struct", hru_data.bpar_struct),
         inspector.field("bvar_struct", hru_data.bvar_struct),
         inspector.field("dpar_struct", hru_data.dpar_struct),
         inspector.field("start_time", hru_data.start_time),
         inspector.field("end_time", hru_data.end_time),
         inspector.field("ref_time", hru_data.ref_time),
         inspector.field("old_time", hru_data.old_time),
         inspector.field("stat_counter", hru_data.stat_counter),
         inspector.field("output_timestep", hru_data.output_timestep),
         inspector.field("reset_stats", hru_data.reset_stats),
         inspector.field("finalize_stats", hru_data.finalize_stats),
         inspector.field("frac_jul_day", hru_data.frac_jul_day),
         inspector.field("tm_zone_offset_frac_day", 
                         hru_data.tm_zone_offset_frac_day),
         inspector.field("year_length", hru_data.year_length),
         inspector.field("compute_veg_flux", hru_data.compute_veg_flux),
         inspector.field("dt_init", hru_data.dt_init),
         inspector.field("upArea", hru_data.upArea));
}