extern "C" {
  void defineGlobalData_fortran(int* err, void* err_msg);

  void deallocateGlobalData_fortran(int* err, void* err_msg);
}

// This is a class that wraps around the data created in 
// defineGlobalData()
class summaGlobalData {
  public:
    summaGlobalData();
    ~summaGlobalData();

    int defineGlobalData();
  private:
    bool global_data_ready;
};  
