#pragma once
#include "settings_functions.hpp"
extern "C" {
  void f_allocate(int& num_gru, int& err, void* message);
  void f_paramSetup(int& err, void* message);
  void f_readRestart(int& err, void* message);
  void f_getInitTolerance(double& rtol, double& atol);
  void f_deallocateInitStruc();
}

class SummaInitStruc {
  public:
    SummaInitStruc() {};
    ~SummaInitStruc(){f_deallocateInitStruc();};

    int allocate(int num_gru); // allocate space in Fortran
    int summa_paramSetup();    // call summa_paramSetup
    int summa_readRestart();   // call summa_readRestart
    void getInitTolerance(double rel_tol, double abs_tol); 
};

