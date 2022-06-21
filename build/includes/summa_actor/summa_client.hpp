#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"

namespace caf {

struct summa_client_state {
    strong_actor_ptr current_server;
};
behavior summa_client(stateful_actor<summa_client_state>* self);
behavior unconnected(stateful_actor<summa_client_state>*);
void connecting(stateful_actor<summa_client_state>*, const std::string& host, uint16_t port);
behavior running(stateful_actor<summa_client_state>*, const actor& summa_server);

}