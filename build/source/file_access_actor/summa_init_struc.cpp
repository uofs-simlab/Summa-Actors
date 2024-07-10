#include "summa_init_struc.hpp"
#include <memory>
#include <iostream>
using chrono_time = std::chrono::time_point<std::chrono::high_resolution_clock>;

SummaInitStruc::SummaInitStruc() {}

SummaInitStruc::~SummaInitStruc() {
  deallocate_init_struc();
}

int SummaInitStruc::allocate(int num_gru) {
  chrono_time start = std::chrono::high_resolution_clock::now();
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  initialize_init_struc(num_gru, err, &message);
  if (err != 0) std::cout << message.get() << std::endl;

  chrono_time end = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed_seconds = end - start;
  std::cout << "Time taken for allocate: " << elapsed_seconds.count() << "s\n";

  return err;
}

int SummaInitStruc::summa_paramSetup() {
  chrono_time start = std::chrono::high_resolution_clock::now();
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  paramSetup_fortran(err, &message);
  if (err != 0) std::cout << message.get() << std::endl;

  chrono_time end = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed_seconds = end - start;
  std::cout << "Time taken for summa_paramSetup: " << elapsed_seconds.count() 
            << "s\n";

  return err;
}

int SummaInitStruc::summa_readRestart() {
  chrono_time start = std::chrono::high_resolution_clock::now();
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  readRestart_fortran(err, &message);
  if (err != 0) std::cout << message.get() << std::endl;

  chrono_time end = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed_seconds = end - start;
  std::cout << "Time taken for summa_readRestart: " << elapsed_seconds.count() 
            << "s\n";

  return err;
}

void SummaInitStruc::getInitTolerance(double rel_tol, double abs_tol) {
  getInitTolerance_fortran(rel_tol, abs_tol);
}

