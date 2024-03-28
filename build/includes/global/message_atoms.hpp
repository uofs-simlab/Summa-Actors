#pragma once

#include "batch.hpp"
#include "batch_container.hpp"
#include "client.hpp"
#include "client_container.hpp"
#include <vector>
#include "settings_functions.hpp"
#include "global.hpp"
#include "caf/all.hpp"
#include <unordered_map>

// HRU Data structure used for serialization
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
  double dt_init;
  double upArea;

  // Sundials variables
  double rtol;
  double atol;

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
      inspector.field("dt_init", hru_data.dt_init),
      inspector.field("upArea", hru_data.upArea),
      inspector.field("rtol", hru_data.rtol),
      inspector.field("atol", hru_data.atol),
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
      inspector.field("compute_veg_flux", hru_data.compute_veg_flux));
}

struct NumGRUInfo {
  int start_gru_local;
  int start_gru_global; 
  int num_gru_local;
  int num_gru_global;
  int file_gru; 
  bool use_global_for_data_structures;

  // Constructor
  NumGRUInfo(int start_gru_local = 0, int start_gru_global= 0, 
      int num_gru_local = 0, int num_gru_global = 0, int file_gru = 0, 
      bool use_global_for_data_structures = false) 
    : start_gru_local(start_gru_local), start_gru_global(start_gru_global), 
    num_gru_local(num_gru_local), num_gru_global(num_gru_global), 
    file_gru(file_gru), 
    use_global_for_data_structures(use_global_for_data_structures) {}
};
template <class Insepctor>
bool inspect(Insepctor& inspector, NumGRUInfo& num_gru) {
  return inspector.object(num_gru).fields(
      inspector.field("start_gru_local", num_gru.start_gru_local),
      inspector.field("start_gru_global", num_gru.start_gru_global),
      inspector.field("num_gru_local", num_gru.num_gru_local),
      inspector.field("num_gru_global", num_gru.num_gru_global),
      inspector.field("file_gru", num_gru.file_gru),
      inspector.field("use_global_for_data_structures", 
          num_gru.use_global_for_data_structures));
}


enum class hru_error : uint8_t {
    run_physics_unhandleable = 1,
    run_physics_infeasible_state = 2,
};

enum class file_access_error : uint8_t {
    writing_error = 1,
    unhandleable_error = 2,
    mDecisions_error = 100,
};

// HRU Errors
std::string to_string(hru_error err);
bool from_string(caf::string_view in, hru_error& out);
bool from_integer(uint8_t in, hru_error& out);
template<class Inspector>
bool inspect(Inspector& f, hru_error& x) {
    return caf::default_enum_inspect(f, x);
}

// File Access Actor
std::string to_string(file_access_error err);
bool from_string(caf::string_view in, file_access_error& out);
bool from_integer(uint8_t in, file_access_error& out);
template<class Inspector>
bool inspect(Inspector& f, file_access_error& x) {
    return caf::default_enum_inspect(f, x);
}

CAF_BEGIN_TYPE_ID_BLOCK(summa, first_custom_type_id)
    // BMI Start
    CAF_ADD_ATOM(summa, update_hru)
    CAF_ADD_ATOM(summa, init_file_access_actor)
    CAF_ADD_ATOM(summa, update_timeZoneOffset)
    CAF_ADD_ATOM(summa, done_update)
    CAF_ADD_ATOM(summa, init_normal_mode)
    CAF_ADD_ATOM(summa, update_hru_async)
    CAF_ADD_ATOM(summa, start_job)
    CAF_ADD_ATOM(summa, serialize_hru)
    CAF_ADD_ATOM(summa, reinit_hru)
    CAF_ADD_ATOM(summa, def_output)
    CAF_ADD_ATOM(summa, load_balance)

    // Sender: job_actor 
    // Reciever: summa_actor
    // Summary: job_actor finished job
    CAF_ADD_ATOM(summa, done_job)
    CAF_ADD_ATOM(summa, exit_msg)
    // Sender: 
    // Reciever: 
    // Summary:
    CAF_ADD_ATOM(summa, err)
    CAF_ADD_ATOM(summa, err_atom)
    // Sender:
    // Reciever: 
    // Summary:
    CAF_ADD_ATOM(summa, init_gru)
    // Sender:
    // Reciever: 
    // Summary:
    CAF_ADD_ATOM(summa, done_init_gru)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, init_hru)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, done_init_hru)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, done_write)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, done_hru)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, run_failure)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, access_forcing)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, access_forcing_internal)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, write_output)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, deallocate_structures)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, write_param)
        // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, restart_failures)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, get_attributes_params)
    // Sender:
    // Reciever:
    // Summary:    
    CAF_ADD_ATOM(summa, run_hru)
    // Sender:
    // Reciever:
    // Summary:    
    CAF_ADD_ATOM(summa, start_hru)
    // Sender:
    // Reciever:
    // Summary:    
    CAF_ADD_ATOM(summa, dt_init_factor)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, connect_to_server)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, connect_as_backup)
    // Sender:
    // Reciever:
    // Summary:    
    CAF_ADD_ATOM(summa, compute_batch)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, done_batch)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, time_to_exit)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, update_with_current_state)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, new_assigned_batch)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, no_more_batches)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, update_backup_server_list)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, client_removed)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, new_client)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, is_lead_server)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, yes)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, no)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, new_forcing_file)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, num_steps_before_write)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, get_num_output_steps)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, finalize)
    
    // Struct Types
    CAF_ADD_TYPE_ID(summa, (Distributed_Settings))
    CAF_ADD_TYPE_ID(summa, (Summa_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (File_Access_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (Job_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (HRU_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (serializable_netcdf_gru_actor_info))

    CAF_ADD_TYPE_ID(summa, (hru))
    CAF_ADD_TYPE_ID(summa, (NumGRUInfo))

    // Class Types
    CAF_ADD_TYPE_ID(summa, (Client))
    CAF_ADD_TYPE_ID(summa, (Client_Container))
    CAF_ADD_TYPE_ID(summa, (Batch))
    CAF_ADD_TYPE_ID(summa, (Batch_Container))

    CAF_ADD_TYPE_ID(summa, (std::vector<std::vector<double>>))
    CAF_ADD_TYPE_ID(summa, (std::vector<std::vector<int>>))
    CAF_ADD_TYPE_ID(summa, (std::vector<int>))
    CAF_ADD_TYPE_ID(summa, (std::vector<double>))
    CAF_ADD_TYPE_ID(summa, (std::vector<long int>))
    CAF_ADD_TYPE_ID(summa, (std::vector<std::tuple<caf::actor, std::string>>))
    CAF_ADD_TYPE_ID(summa, (std::vector<serializable_netcdf_gru_actor_info>))
    CAF_ADD_TYPE_ID(summa, (std::unordered_map<caf::actor, double>))
    CAF_ADD_TYPE_ID(summa, (std::unordered_map<caf::actor, caf::actor>))

    // GRU Parameter/Attribute Vectors
    CAF_ADD_TYPE_ID(summa, (std::tuple<std::vector<double>, 
                                       std::vector<int>, 
                                       std::vector<long int>, 
                                       std::vector<double>, 
                                       std::vector<double>, 
                                       std::vector<std::vector<double>>>))
    
    // File_Access_Actor Read/Write times
    CAF_ADD_TYPE_ID(summa, (std::tuple<double, double>))

    CAF_ADD_TYPE_ID(summa, (std::optional<caf::strong_actor_ptr>))

    // error types
    CAF_ADD_TYPE_ID(summa, (hru_error))

    CAF_ADD_TYPE_ID(summa, (file_access_error))


CAF_END_TYPE_ID_BLOCK(summa)

CAF_ERROR_CODE_ENUM(hru_error)
CAF_ERROR_CODE_ENUM(file_access_error)