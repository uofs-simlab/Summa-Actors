#include "summa_init_struc.hpp"
#include <memory>

SummaInitStruc::SummaInitStruc() {}

SummaInitStruc::~SummaInitStruc() {
  deallocate_init_struc();
}

int SummaInitStruc::allocate(int num_gru) {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  initialize_init_struc(num_gru, err, &message);
  return err;
}

int SummaInitStruc::summa_paramSetup() {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  paramSetup_fortran(err, &message);
  return err;
}

int SummaInitStruc::summa_readRestart() {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  readRestart_fortran(err, &message);
  return err;
}

