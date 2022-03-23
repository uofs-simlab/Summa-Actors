#include "testCoordinator.h"

using namespace caf;
behavior test_coordinator(stateful_actor<test_state>* self) {
    aout(self) << "Starting Test Actor\n";
    return {
    };
}

void caf_main(caf::actor_system& sys) {
    caf::scoped_actor self{sys};
    aout(self) << "Starting Test \n";
    auto test_actor = self->spawn(test_coordinator);
}

CAF_MAIN()