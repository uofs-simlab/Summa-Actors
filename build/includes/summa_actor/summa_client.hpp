#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"
#include "batch/batch.hpp"
#include "summa_actor.hpp"
#include "message_atoms.hpp"
#include <string>
#include <optional>
#include <unistd.h>
#include <limits.h>

namespace caf {

struct summa_client_state {
    strong_actor_ptr current_server;
    std::string hostname;
    actor summa_actor_ref;
    uint16_t port;
    int batch_id;
    int client_id; // id held by server

    // tuple is the actor ref and hostname of the backup server
    std::vector<std::tuple<caf::actor, std::string>> backup_servers;

    Batch current_batch;

    Summa_Actor_Settings summa_actor_settings;
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings;
    HRU_Actor_Settings hru_actor_settings;
};

behavior summa_client_init(stateful_actor<summa_client_state>* self);
void connecting(stateful_actor<summa_client_state>*, const std::string& host, uint16_t port);
behavior summa_client(stateful_actor<summa_client_state>*, const actor& summa_server);

}