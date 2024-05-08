#include "gru_struc.hpp"
#include <iostream>
#include <memory>

GruStruc::GruStruc(int start_gru, int num_gru, int num_retry_attempts) {
  start_gru_ = start_gru;
  num_gru_ = num_gru;
  num_retry_attempts_left_ = num_retry_attempts;
  gru_info_.resize(num_gru);
}

int GruStruc::ReadDimension() {
  // gru_struc is set up in fortran here
  int err = 0; int num_hru, file_gru, file_hru;
  std::unique_ptr<char[]> err_msg(new char[256]);
  read_dimension_fortran(start_gru_, num_gru_, num_hru, file_gru, file_hru,
                         err, &err_msg);
  if (err != 0) { 
    std::cout << "ERROR: GruStruc - ReadDimension\n";
  }
  num_hru_ = num_hru;
  file_gru_ = file_gru;
  file_hru_ = file_hru;
  return err;
}

int GruStruc::ReadIcondNlayers() {
  int err = 0;
  std::unique_ptr<char[]> err_msg(new char[256]);
  read_icond_nlayers_fortran(num_gru_, err, &err_msg);
  if (err != 0) { 
    std::cout << "ERROR: GruStruc - ReadIcondNlayers\n";
  }
  return 0;
}

void GruStruc::getNumHrusPerGru() {
  num_hru_per_gru_.resize(num_gru_, 0);
  get_num_hru_per_gru_fortran(num_gru_, num_hru_per_gru_[0]);
}


