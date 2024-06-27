#include "summa_global_data.hpp"
#include <memory>
#include <iostream>

SummaGlobalData::SummaGlobalData() {
  global_data_ready = false;
}

SummaGlobalData::~SummaGlobalData() {
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

int SummaGlobalData::defineGlobalData() {
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
