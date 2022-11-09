#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include <string>
namespace caf {

    struct summa_backup_state {
        strong_actor_ptr current_server;
        std::string hostname;
    };

behavior summa_backup_server(stateful_actor<summa_backup_state>* self);
behavior unconnected(stateful_actor<summa_backup_state>* self);
void connecting(stateful_actor<summa_backup_state>* self, const std::string& host, uint16_t port);
behavior running(stateful_actor<summa_backup_state>* self, const actor& server_actor);
behavior backup_server(stateful_actor<summa_backup_state>* self);
}