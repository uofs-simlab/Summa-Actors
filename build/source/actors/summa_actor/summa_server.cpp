#include "caf/all.hpp"
#include "caf/io/all.hpp"

#include "summa_server.hpp"

namespace caf {

behavior summa_server(stateful_actor<summa_server_state>* self) {
    aout(self) << "Summa Server has Started \n"; 

    return {
        [=](int i) {
            aout(self) << "Received " << i << "\n";
            return "Received";
        }

    };
}
}
