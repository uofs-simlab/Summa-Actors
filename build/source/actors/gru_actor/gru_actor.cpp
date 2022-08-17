#include "caf/all.hpp"
#include "gru_actor.hpp"
#include "global.hpp"
#include "message_atoms.hpp"
#include "gru_actor_subroutine_wrappers.hpp"

namespace caf {

behavior gru_actor(stateful_actor<gru_state>* self, int refGRU, int indxGRU, 
    std::string configPath, int outputStrucSize, caf::actor parent) {

    aout(self) << "GRU Actor Has Started\n";

    return {


        // What does a GRU need to assemble its data structure?
        //
        [=](init_gru) {
            // Get the variable data length, we also need the type information
            aout(self) << "init GRU";
        }


    };
}

}