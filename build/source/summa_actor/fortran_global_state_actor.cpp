#include "summa_actor.hpp"


namespace caf {

// This actor's lifetime manages the global state of the fortran vars
// Set by the function summa_defineGlobalData()
behavior fortran_global_state_actor(event_based_actor* self) {
  aout(self) << "Starting Global State Actor\n";
  self->attach_functor([=](const error& reason) {
    aout(self) << "Global State Actor Exited\n";
    std::unique_ptr<char[]> err_msg(new char[1024]);
    int err = 0;
    deallocateGlobalData_fortran(&err, &err_msg);
    if (err != 0) {
      aout(self) << "\n\nERROR: deallocateGlobalData_fortran() - " 
                 << err_msg.get() << "\n\n";
    }
    aout(self) << "HERE\n";
  });


  std::unique_ptr<char[]> err_msg(new char[1024]);
  int err = 0;
  defineGlobalData_fortran(&err, &err_msg);
  if (err != 0) {
    aout(self) << "\n\nERROR: defineGlobalData_fortran() - " 
               << err_msg.get() << "\n\n";
    self->quit();
    return {};
  }


  return {};
}


} // namespace caf