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

void SummaInitStruc::getInitTolerance(double rel_tol, double abs_tol, double rel_tol_temp_cas,
                                      double rel_tol_temp_veg, double rel_tol_wat_veg, 
                                      double rel_tol_temp_soil_snow, double rel_tol_wat_snow, 
                                      double rel_tol_matric, double rel_tol_aquifr,
                                      double abs_tol_temp_cas, double abs_tol_temp_veg, 
                                      double abs_tol_wat_veg, double abs_tol_temp_soil_snow, 
                                      double abs_tol_wat_snow, double abs_tol_matric,
                                      double abs_tol_aquifr) {
  f_getInitTolerance(rel_tol, abs_tol);
}

