extern "C" {
  void defineGlobalData_openWQ_fortran(int* err, void* err_msg);

  void deallocateGlobalData_openWQ_fortran(int* err, void* err_msg);
}
#pragma once
// This is a class that wraps around the data created in 
// defineGlobalData()
class openWQGlobalData {
  public:
    openWQGlobalData();
    ~openWQGlobalData();

    int defineGlobalData();
  private:
    bool global_data_ready;
};  
