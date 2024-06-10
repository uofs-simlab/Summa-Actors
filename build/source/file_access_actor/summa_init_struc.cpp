#include "summa_init_struc.hpp"
#include <memory>
#include <iostream>

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

void SummaInitStruc::getInitTolerance(HRUActorSettings& hru_settings) {
  getInitTolerance_fortran(hru_settings.rel_tol, hru_settings.abs_tol);
}

