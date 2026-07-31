#pragma once
#include "settings_functions.hpp"
extern "C" {
  void f_allocate(int& num_gru, int& err, void* message);
  void f_paramSetup(int& err, void* message);
  void f_readRestart(int& err, void* message);
  void f_getInitTolerance(double& rtol_temp_cas,
                          double& rtol_temp_veg, double& rtol_wat_veg, 
                          double& rtol_temp_soil_snow, double& rtol_wat_snow, 
                          double& rtol_matric, double& rtol_aquifr,
                          double& atol_temp_cas, double& atol_temp_veg, 
                          double& atol_wat_veg, double& atol_temp_soil_snow, 
                          double& atol_wat_snow, double& atol_matric,
                          double& atol_aquifr, bool& def_tol, int& be_steps);
  // void f_getInitBEStepsIDATol(int& beSteps, double& rtol, double& atolWat, double& atolNrg);
  void f_deallocateInitStruc();
}

class SummaInitStruc {
  public:
    SummaInitStruc() {};
    ~SummaInitStruc(){f_deallocateInitStruc();};

    int allocate(int num_gru); // allocate space in Fortran
    int summa_paramSetup();    // call summa_paramSetup
    int summa_readRestart();   // call summa_readRestart
    void getInitTolerance(double& rel_tol_temp_cas,
                          double& rel_tol_temp_veg, double& rel_tol_wat_veg, 
                          double& rel_tol_temp_soil_snow, double& rel_tol_wat_snow, 
                          double& rel_tol_matric, double& rel_tol_aquifr,
                          double& abs_tol_temp_cas, double& abs_tol_temp_veg, 
                          double& abs_tol_wat_veg, double& abs_tol_temp_soil_snow, 
                          double& abs_tol_wat_snow, double& abs_tol_matric,
                          double& abs_tol_aquifr, bool& def_tol, int& be_steps); 
    // void getInitBEStepsIDATol(int be_steps, double rel_tol, double abs_tolWat, double abs_tolNrg); 
};

