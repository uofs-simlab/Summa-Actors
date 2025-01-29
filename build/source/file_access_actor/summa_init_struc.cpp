#include "summa_init_struc.hpp"
#include <memory>
#include <iostream>
using chrono_time = std::chrono::time_point<std::chrono::high_resolution_clock>;

int SummaInitStruc::allocate(int num_gru) {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  f_allocate(num_gru, err, &message);
  if (err != 0) std::cout << message.get() << std::endl;
  return err;
}

int SummaInitStruc::summa_paramSetup() {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  f_paramSetup(err, &message);
  if (err != 0) std::cout << message.get() << std::endl;

  return err;
}

int SummaInitStruc::summa_readRestart() {
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  f_readRestart(err, &message);
  if (err != 0) std::cout << message.get() << std::endl;

  return err;
}

void SummaInitStruc::getInitBEStepsIDATol(int be_steps, double rel_tol, double abs_tolWat, double abs_tolNrg) {
  f_getInitBEStepsIDATol(be_steps, rel_tol, abs_tolWat, abs_tolNrg);
}

