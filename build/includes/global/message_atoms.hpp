#pragma once
#include "caf/all.hpp"

#include "batch.hpp"
#include "batch_container.hpp"

#include "client.hpp"
#include "client_container.hpp"

#include "logger.hpp"

#include "gru_struc.hpp"

#include "gru_data_structure.hpp"
#include "num_gru_info.hpp"
#include "settings_functions.hpp"

#include <vector>
#include <unordered_map>


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
    CAF_ADD_ATOM(summa, file_access_actor_ready)
    CAF_ADD_ATOM(summa, global_data_ready)

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
    // Sender: HRU Actor
    // Reciever: File Access Actor 
    // Summary: Updates FAA when hru reaches restart checkpoint
    CAF_ADD_ATOM(summa, write_restart)
    CAF_ADD_ATOM(summa, write_restart_da)
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
    CAF_ADD_TYPE_ID(summa, (DistributedSettings))
    CAF_ADD_TYPE_ID(summa, (SummaActorSettings))
    CAF_ADD_TYPE_ID(summa, (FileAccessActorSettings))
    CAF_ADD_TYPE_ID(summa, (JobActorSettings))
    CAF_ADD_TYPE_ID(summa, (HRUActorSettings))
    CAF_ADD_TYPE_ID(summa, (Settings))
    CAF_ADD_TYPE_ID(summa, (ToleranceSettings))
    
    CAF_ADD_TYPE_ID(summa, (HRU))
    CAF_ADD_TYPE_ID(summa, (std::vector<HRU>))
    CAF_ADD_TYPE_ID(summa, (NumGRUInfo))
    CAF_ADD_TYPE_ID(summa, (NodeGruInfo))

    // Class Types
    CAF_ADD_TYPE_ID(summa, (Client))
    CAF_ADD_TYPE_ID(summa, (Client_Container))
    CAF_ADD_TYPE_ID(summa, (Batch))
    CAF_ADD_TYPE_ID(summa, (BatchContainer))
    CAF_ADD_TYPE_ID(summa, (Logger))

    CAF_ADD_TYPE_ID(summa, (std::vector<std::vector<double>>))
    CAF_ADD_TYPE_ID(summa, (std::vector<std::vector<int>>))
    CAF_ADD_TYPE_ID(summa, (std::vector<int>))
    CAF_ADD_TYPE_ID(summa, (std::vector<double>))
    CAF_ADD_TYPE_ID(summa, (std::vector<long int>))
    CAF_ADD_TYPE_ID(summa, (std::vector<std::tuple<caf::actor, std::string>>))
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

CAF_END_TYPE_ID_BLOCK(summa)


