#pragma once

#include "batch/batch.hpp"
#include "batch/batch_container.hpp"
#include "client/client.hpp"
#include "client/client_container.hpp"
#include <vector>
#include "settings_functions.hpp"

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
    CAF_ADD_ATOM(summa, heartbeat)    
    // Struct Types
    CAF_ADD_TYPE_ID(summa, (Distributed_Settings))
    CAF_ADD_TYPE_ID(summa, (Summa_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (File_Access_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (Job_Actor_Settings))
    CAF_ADD_TYPE_ID(summa, (HRU_Actor_Settings))

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


CAF_END_TYPE_ID_BLOCK(summa)