#include "job_actor.hpp"

namespace caf {

behavior distributed_job_actor(stateful_actor<distributed_job_state>* self,
                               Distributed_Settings distributed_settings,
                               File_Access_Actor_Settings file_access_actor_settings,
                               Job_Actor_Settings job_actor_settings, 
                               HRU_Actor_Settings hru_actor_settings) {
  aout(self) << "Starting Distributed Job Actor\n";
  
  return {};
}



} // namespace caf