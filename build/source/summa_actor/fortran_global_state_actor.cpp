#include "summa_actor.hpp"


summaGlobalData::summaGlobalData() {
  global_data_ready = false;
}

summaGlobalData::~summaGlobalData() {
  if (global_data_ready) {
    std::unique_ptr<char[]> err_msg(new char[1024]);
    int err = 0;
    deallocateGlobalData_fortran(&err, &err_msg);
    if (err != 0) {
      std::cout << "\n\nERROR: deallocateGlobalData_fortran() - " 
                << err_msg.get() << "\n\n";
    }
  }
}

int summaGlobalData::defineGlobalData() {
  std::unique_ptr<char[]> err_msg(new char[1024]);
  int err = 0;
  defineGlobalData_fortran(&err, &err_msg);
  if (err != 0) {
    std::cout << "\n\nERROR: defineGlobalData_fortran() - " 
              << err_msg.get() << "\n\n";
    global_data_ready = false;
  } else {
    global_data_ready = true;
  }
  return err;
}



// namespace caf {

// // This actor's lifetime manages the global state of the fortran vars
// // Set by the function summa_defineGlobalData()
// behavior fortran_global_state_actor(event_based_actor* self, actor parent) {
//   aout(self) << "Starting Global State Actor\n";

//   self->attach_functor([=](const error& reason) {
//     aout(self) << "Global State Actor Exited\n";
//     std::unique_ptr<char[]> err_msg(new char[1024]);
//     int err = 0;
//     deallocateGlobalData_fortran(&err, &err_msg);
//     if (err != 0) {
//       aout(self) << "\n\nERROR: deallocateGlobalData_fortran() - " 
//                  << err_msg.get() << "\n\n";
//     }
//   });




//   self->send(parent, global_data_ready_v);

//   return {
//     // Needs a handler to persist, otherwise it gets cleaned up immediately
//     [=](define_global_data) {
//       std::unique_ptr<char[]> err_msg(new char[1024]);
//       int err = 0;
//       defineGlobalData_fortran(&err, &err_msg);
//       if (err != 0) {
//         aout(self) << "\n\nERROR: defineGlobalData_fortran() - " 
//                    << err_msg.get() << "\n\n";
//         self->send(parent, err_v);
//         self->quit();
//         return;
//       }
//       self->send(parent, global_data_ready_v);
//     }
//   };
// }


// } // namespace caf