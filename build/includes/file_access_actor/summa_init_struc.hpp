#define SUMMA_INIT_STRUC
#ifdef SUMMA_INIT_STRUC

extern "C" {
  void initialize_init_struc(int& num_gru, int& err, void* message);
  void paramSetup_fortran(int& err, void* message);
  void readRestart_fortran(int& err, void* message);
  void deallocate_init_struc();
}

class SummaInitStruc {
  public:
    SummaInitStruc();
    ~SummaInitStruc();

    int allocate(int num_gru); // allocate space in Fortran
    int summa_paramSetup();    // call summa_paramSetup
    int summa_readRestart();   // call summa_readRestart
};


#endif
