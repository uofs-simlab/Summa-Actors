#pragma once

extern "C" {
  void defineGlobalData_fortran(int* err, void* err_msg);

  void deallocateGlobalData_fortran(int* err, void* err_msg);
}

// This is a class that wraps around the data created in 
// defineGlobalData()
class SummaGlobalData {
  public:
    SummaGlobalData();
    ~SummaGlobalData();

    int defineGlobalData();
  private:
    bool global_data_ready;
};  
