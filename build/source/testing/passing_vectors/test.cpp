#include "caf/all.hpp"
#include <vector>

using namespace caf;
CAF_BEGIN_TYPE_ID_BLOCK(custom_types_1, first_custom_type_id)

  CAF_ADD_TYPE_ID(custom_types_1, (std::vector<int>) )
  CAF_ADD_TYPE_ID(custom_types_1, (std::vector<std::vector<int>>) )

CAF_END_TYPE_ID_BLOCK(custom_types_1)

behavior test_actor(event_based_actor* self) {
    aout(self) << "Spawned" << std::endl;

    return {
        [=](std::vector<std::vector<int>> test) {
            for (int i = 0; i < test.size(); i++) {
                aout(self) << test[i] << std::endl;
            }
        },

        [=](double i) {
            aout(self) << i << std::endl;
        }
    };
}


void caf_main(actor_system& sys) {
    scoped_actor self{sys};
    aout(self) << "Hello World\n";

    auto actor = sys.spawn(test_actor);
    std::vector<int> test_sub1 = {4,5,4,3};
    std::vector<int> test_sub2 = {9,8,7,6};
    std::vector<std::vector<int>> test = {test_sub1, test_sub2};
    self->send(actor, test);
}


CAF_MAIN(id_block::custom_types_1)