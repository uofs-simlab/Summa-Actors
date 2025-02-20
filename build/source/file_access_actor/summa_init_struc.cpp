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

void SummaInitStruc::getInitTolerance(double& rel_tol, double& abs_tol, double& rel_tol_temp_cas,
                                      double& rel_tol_temp_veg, double& rel_tol_wat_veg, 
                                      double& rel_tol_temp_soil_snow, double& rel_tol_wat_snow, 
                                      double& rel_tol_matric, double& rel_tol_aquifr,
                                      double& abs_tol_temp_cas, double& abs_tol_temp_veg, 
                                      double& abs_tol_wat_veg, double& abs_tol_temp_soil_snow, 
                                      double& abs_tol_wat_snow, double& abs_tol_matric,
                                      double& abs_tol_aquifr, bool& def_tol) {
  int def_tol_temp = 0;
  f_getInitTolerance(rel_tol, abs_tol, rel_tol_temp_cas, rel_tol_temp_veg, rel_tol_wat_veg, 
                     rel_tol_temp_soil_snow, rel_tol_wat_snow, rel_tol_matric, rel_tol_aquifr,
                     abs_tol_temp_cas, abs_tol_temp_veg, abs_tol_wat_veg, abs_tol_temp_soil_snow, 
                     abs_tol_wat_snow, abs_tol_matric, abs_tol_aquifr, def_tol);
  def_tol = (def_tol_temp == 1);
}

