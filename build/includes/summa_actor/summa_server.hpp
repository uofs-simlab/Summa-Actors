#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"

namespace caf {

struct summa_server_state {

};

behavior summa_server(stateful_actor<summa_server_state>* self);

}