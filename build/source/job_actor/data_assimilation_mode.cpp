#include "job_actor.hpp"

using namespace caf;














//     [=](std::vector<actor> hru_actors) {},

//     [=](serialize_hru, hru hru_data) {
//       aout(self) << "Job_Actor: Recieved HRU Data\n";
//       auto sender = actor_cast<actor>(self->current_sender());

//       self->send(sender, reinit_hru_v, hru_data);
//     },


//     [=](finalize) { finalizeJob(self); },
//   };
// }