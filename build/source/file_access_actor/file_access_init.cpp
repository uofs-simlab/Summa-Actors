#include "file_access_actor.hpp"


behavior file_access_init(stateful_actor<file_access_state>* self) {
  aout(self) << "File Access Actor: Intializing\n";
  
  return {};
}