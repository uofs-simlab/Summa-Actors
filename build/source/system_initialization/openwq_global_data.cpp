#include "openwq_global_data.hpp"
#include <memory>
#include <iostream>

openWQGlobalData::openWQGlobalData() {
  global_data_ready = false;
}

openWQGlobalData::~openWQGlobalData() {
  if (global_data_ready) {
    std::unique_ptr<char[]> err_msg(new char[1024]);
    int err = 0;
  }
}

int openWQGlobalData::defineGlobalData() {
  std::unique_ptr<char[]> err_msg(new char[1024]);
  int err = 0;
  defineGlobalData_openWQ_fortran(&err, &err_msg);
  if (err != 0) {
    std::cout << "\n\nERROR: defineGlobalData_openWQ_fortran() - " 
              << err_msg.get() << "\n\n";
    global_data_ready = false;
  } else {
    global_data_ready = true;
  }
  return err;
}
