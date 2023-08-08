#pragma once

#include "batch/batch.hpp"
#include "batch/batch_container.hpp"
#include "client/client.hpp"
#include "client/client_container.hpp"
#include <vector>
#include "settings_functions.hpp"
#include "global.hpp"
#include "caf/all.hpp"

enum class hru_error : uint8_t {
    run_physics_unhandleable = 1,
    run_physics_infeasible_state = 2,
};

enum class file_access_error : uint8_t {
    writing_error = 1,
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
    // Sender: job_actor 
    // Reciever: summa_actor
    // Summary: job_actor finished job
    CAF_ADD_ATOM(summa, done_job)
    // Sender: 
    // Reciever: 
    // Summary:
    CAF_ADD_ATOM(summa, err)
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
    CAF_ADD_ATOM(summa, done_file_access_actor_init)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, file_access_actor_done)
    // Sender:
    // Reciever:
    // Summary:
    CAF_ADD_ATOM(summa, file_access_actor_err)
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
    
    // Struct Types
    CAF_ADD_TYPE_ID(summa, (Distributed_Settings))
    CAF_ADD_TYPE_ID(summa, (Summa_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (File_Access_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (Job_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (HRU_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (serializable_netcdf_gru_actor_info))

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

    CAF_ADD_TYPE_ID(summa, (std::tuple<std::vector<double>, 
                                       std::vector<int>, 
                                       std::vector<long int>, 
                                       std::vector<double>, 
                                       std::vector<double>, 
                                       std::vector<std::vector<double>>>))

    CAF_ADD_TYPE_ID(summa, (std::optional<caf::strong_actor_ptr>))

    // error types
    CAF_ADD_TYPE_ID(summa, (hru_error))

    CAF_ADD_TYPE_ID(summa, (file_access_error))


CAF_END_TYPE_ID_BLOCK(summa)

CAF_ERROR_CODE_ENUM(hru_error)
CAF_ERROR_CODE_ENUM(file_access_error)