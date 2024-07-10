#include "summa_init_struc.hpp"
#include <memory>
#include <iostream>
using chrono_time = std::chrono::time_point<std::chrono::high_resolution_clock>;

SummaInitStruc::SummaInitStruc() {}

SummaInitStruc::~SummaInitStruc() {
  deallocate_init_struc();
}

int SummaInitStruc::allocate(int num_gru) {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  initialize_init_struc(num_gru, err, &message);
  if (err != 0) std::cout << message.get() << std::endl;
  return err;
}

int SummaInitStruc::summa_paramSetup() {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  paramSetup_fortran(err, &message);
  if (err != 0) std::cout << message.get() << std::endl;

  return err;
}

int SummaInitStruc::summa_readRestart() {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  readRestart_fortran(err, &message);
  if (err != 0) std::cout << message.get() << std::endl;

  return err;
}

void SummaInitStruc::getInitTolerance(double rel_tol, double abs_tol) {
  getInitTolerance_fortran(rel_tol, abs_tol);
}

