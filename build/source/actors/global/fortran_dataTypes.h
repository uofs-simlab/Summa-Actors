#ifndef FORTRAN_DATATYPES_H_
#define FORTRAN_DATATYPES_H_

extern "C" {
    // flagVec 
    void* new_handle_flagVec();
    void  delete_handle_flagVec(void* handle);
    void  set_data_flagVec(void* handle, const int* array, int size);
    void  get_size_data_flagVec(void* handle, int* size);
    void  get_data_flagVec(void* handle, int* array);

    // var_i 
    void* new_handle_var_i();
    void  delete_handle_var_i(void* handle);
    void  set_data_var_i(void* handle, const int* array, int size);
    void  get_size_data_var_i(void* handle, int* size);
    void  get_data_var_i(void* handle, int* array);

    // var_i8 
    void* new_handle_var_i8();
    void  delete_handle_var_i8(void* handle);
    void  set_data_var_i8(void* handle, const long int* array, int size);
    void  get_size_data_var_i8(void* handle, int* size);
    void  get_data_var_i8(void* handle, long int* array);

    // var_d
    void* new_handle_var_d();
    void  delete_handle_var_d(void* handle);
    void  set_data_var_d(void* handle, const double* array, int size);
    void  get_size_data_var_d(void* handle, int* size);
    void  get_data_var_d(void* handle, double* array);

    // ilength
    void* new_handle_ilength();
    void  delete_handle_ilength(void* handle);
    void  set_data_ilength(void* handle, const int* array, int size);
    void  get_size_data_ilength(void* handle, int* size);
    void  get_data_ilength(void* handle, int* array);

    // i8length
    void* new_handle_i8length();
    void  delete_handle_i8length(void* handle);
    void  set_data_i8length(void* handle, const long int* array, int size);
    void  get_size_data_i8length(void* handle, int* size);
    void  get_data_i8length(void* handle, long int* array);

    // dlength
    void* new_handle_dlength();
    void  delete_handle_dlength(void* handle);
    void  set_data_dlength(void* handle, const double* array, int size);
    void  get_size_data_dlength(void* handle, int* size);
    void  get_data_dlength(void* handle, double* array);

    // var_flagVec
    void* new_handle_var_flagVec();
    void  delete_handle_var_flagVec(void* handle);
    void  set_data_var_flagVec(void* handle, const int* array, int num_row, const int* num_col, int num_elements);
    void  get_size_var_flagVec(void* handle, int* num_var);
    void  get_size_data_var_flagVec(void* handle, int* num_var, int* num_dat);
    void  get_data_var_flagVec(void* handle, int* array);

    // var_ilength
    void* new_handle_var_ilength();
    void  delete_handle_var_ilength(void* handle);
    void  set_data_var_ilength(void* handle, const int* array, int num_row, const int* num_col, int num_elements);
    void  get_size_var_ilength(void* handle, int* num_var);
    void  get_size_data_var_ilength(void* handle, int* num_var, int* num_dat);
    void  get_data_var_ilength(void* handle, int* array);

    // var_i8length
    void* new_handle_var_i8length();
    void  delete_handle_var_i8length(void* handle);
    void  set_data_var_i8length(void* handle, const long int* array, int num_row, const int* num_col, int num_elements);
    void  get_size_var_i8length(void* handle, int* num_var);
    void  get_size_data_var_i8length(void* handle, int* num_var, int* num_dat);
    void  get_data_var_i8length(void* handle, long int* array);

    // var_dlength
    void* new_handle_var_dlength();
    void  delete_handle_var_dlength(void* handle);
    void  set_data_var_dlength(void* handle, const double* array, int num_row, const int* num_col, int num_elements);
    void  get_size_var_dlength(void* handle, int* num_var);
    void  get_size_data_var_dlength(void* handle, int* num_var, int* num_dat);
    void  get_data_var_dlength(void* handle, double* array);

    // var_dlength_array
    void* new_handle_dlength_array();
    void  delete_handle_dlength_array(void* handle);

    // var_info 
    void* new_handle_var_info();
    void  delete_handle_var_info(void* handle);
    void  set_data_var_info(void* handle, char const *str1, char const *str2, char const *str3,
    					    int type, const int* ncid, int ncid_size, const int* index, int index_size, int flag);

    // file_info
    void* new_handle_file_info();
    void delete_handle_file_info(void* handle);

}

#endif