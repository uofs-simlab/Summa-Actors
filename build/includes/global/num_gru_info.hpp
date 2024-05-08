#pragma once


struct NumGRUInfo {
  int start_gru_local;
  int start_gru_global; 
  int num_gru_local;
  int num_gru_global;
  int file_gru; 
  bool use_global_for_data_structures;

  // Constructor
  NumGRUInfo(int start_gru_local = 0, int start_gru_global= 0, 
             int num_gru_local = 0, int num_gru_global = 0, int file_gru = 0, 
             bool use_global_for_data_structures = false) 
      : start_gru_local(start_gru_local), start_gru_global(start_gru_global), 
        num_gru_local(num_gru_local), num_gru_global(num_gru_global), 
        file_gru(file_gru), 
        use_global_for_data_structures(use_global_for_data_structures) {}
};
template <class Insepctor>
bool inspect(Insepctor& inspector, NumGRUInfo& num_gru) {
  return inspector.object(num_gru).fields(
      inspector.field("start_gru_local", num_gru.start_gru_local),
      inspector.field("start_gru_global", num_gru.start_gru_global),
      inspector.field("num_gru_local", num_gru.num_gru_local),
      inspector.field("num_gru_global", num_gru.num_gru_global),
      inspector.field("file_gru", num_gru.file_gru),
      inspector.field("use_global_for_data_structures", 
          num_gru.use_global_for_data_structures));
}
