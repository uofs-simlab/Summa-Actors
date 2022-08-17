#pragma once

#include "../summa_actor/batch_manager.hpp"

CAF_BEGIN_TYPE_ID_BLOCK(summa, first_custom_type_id)
    // Summa Actor
    CAF_ADD_ATOM(summa, start_summa)
    CAF_ADD_ATOM(summa, done_job)
    CAF_ADD_ATOM(summa, err)
    // GRU Actor
    CAF_ADD_ATOM(summa, init_gru)
    // Job Actor
    CAF_ADD_ATOM(summa, done_reading_forcingFile)
    CAF_ADD_ATOM(summa, done_reading_first_forcing_file)
    CAF_ADD_ATOM(summa, init_hru)
    CAF_ADD_ATOM(summa, done_init_hru)
    CAF_ADD_ATOM(summa, done_write)
    CAF_ADD_ATOM(summa, doneFile)
    CAF_ADD_ATOM(summa, done_hru)
    CAF_ADD_ATOM(summa, done_final_write)
    CAF_ADD_ATOM(summa, run_failure)
    CAF_ADD_ATOM(summa, done_file_access_actor_init)
    CAF_ADD_ATOM(summa, file_access_actor_done)
    CAF_ADD_ATOM(summa, file_access_actor_err)
    // FileAccess Actor
    CAF_ADD_ATOM(summa, initalize_outputStructure)
    CAF_ADD_ATOM(summa, access_forcing)
    CAF_ADD_ATOM(summa, access_first_forcing_file)
    CAF_ADD_ATOM(summa, access_forcing_internal)
    CAF_ADD_ATOM(summa, write_output)
    CAF_ADD_ATOM(summa, write_output_final)
    CAF_ADD_ATOM(summa, deallocate_structures)
    CAF_ADD_ATOM(summa, update_completed)
    CAF_ADD_ATOM(summa, update_failed)
    CAF_ADD_ATOM(summa, reset_outputCounter)
    CAF_ADD_ATOM(summa, read_and_write)
    CAF_ADD_ATOM(summa, write_param)
    CAF_ADD_ATOM(summa, restart_failures)
    // HRU Actor
    CAF_ADD_ATOM(summa, run_hru)
    CAF_ADD_ATOM(summa, start_hru)
    CAF_ADD_ATOM(summa, file_information)
    CAF_ADD_ATOM(summa, dt_init_factor)
    // Client Actor
    CAF_ADD_ATOM(summa, connect_to_server)
    CAF_ADD_ATOM(summa, batch)
    // Server Actor
    CAF_ADD_ATOM(summa, done_batch)
    CAF_ADD_ATOM(summa, time_to_exit)

CAF_END_TYPE_ID_BLOCK(summa)