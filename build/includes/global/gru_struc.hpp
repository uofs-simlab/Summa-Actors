#include <string>

extern "C" {
  void init_gru_struc(int* num_gru, int* file_hru, int* hru_ix);
  void pop_gru_struc(int* iGRU, long int* gru_id, long int* hru_id, long int* hru2gru_id, 
                     int* hru_ix, int* file_gru, int* file_hru, int* num_gru,
                     int* start_gru);
}

class gruStruc {
  
  public:
    gruStruc(const std::string &settingsPath, 
    const std::string &attributeFile, int num_gru, int start_gru);
    ~gruStruc() {};
};