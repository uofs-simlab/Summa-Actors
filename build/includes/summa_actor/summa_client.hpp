#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"

#include <string>
#include <optional>

namespace caf {

struct summa_client_state {
    strong_actor_ptr current_server;
    std::string hostname;
    std::optional<std::string> config_path;
    actor summa_actor_ref;
    int batch_id;
    int client_id; // id held by server

    Summa_Actor_Settings summa_actor_settings;
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings;
    HRU_Actor_Settings hru_actor_settings;
};
behavior summa_client(stateful_actor<summa_client_state>* self, std::optional<std::string> config_path);
behavior unconnected(stateful_actor<summa_client_state>*);
void connecting(stateful_actor<summa_client_state>*, const std::string& host, uint16_t port);
behavior running(stateful_actor<summa_client_state>*, const actor& summa_server);

}