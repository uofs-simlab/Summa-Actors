#pragma once

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
    void  get_size_data_typeStruct(void* handle, int* size);
    void  get_data_typeStruct(void* handle, int* array);

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
    void  get_data_attrStruct(void* handle, double* array);
    void  get_size_data_attrStruct(void* handle, int* size);
    void  get_data_bparStruct(void* handle, double* array);
    void  get_size_data_bparStruct(void* handle, int* size);

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
    void  get_size_var_mparStruct(void* handle, int* num_var);
    void  get_size_data_mparStruct(void* handle, int* num_var, int* num_dat);
    void  get_data_mparStruct(void* handle, double* array);

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

    // zLookup
    void* new_handle_z_lookup();
    void* delete_handle_z_lookup(void* handle);
    void get_size_z_lookup(void* handle, int* size_z);
    void get_size_var_lookup(void* handle, int* z, int* size_var);
    void get_size_data_lookup(void* handle, int* z, int* var, int* size_data);
    void get_data_zlookup(void* handle, int* z, int* var, double* array);

    // hru_type
    void* new_handle_hru_type();
    void delete_handle_hru_type(void* handle);
    // gru_type
    void* new_handle_gru_type(int& num_hru);
    void delete_handle_gru_type(void* handle);

    // var_dlength_by_indx
    void get_size_var_dlength_by_indx(void* handle, int* indx, int* num_var);
    void get_size_data_var_dlength_by_indx(void* handle, int* struct_indx, 
        int* size_var, int* var);
    void get_data_var_dlength_by_indx(void* handle, int* struct_indx, 
        double* dat);
    void set_data_var_dlength_by_indx(void* handle, int* struct_indx, 
        int* num_var, int* var, int* num_summa_data, double* summa_data);

    // var_ilength_by_indx
    void get_size_var_ilength_by_indx(void* handle, int* indx, int* num_var);
    void get_size_data_var_ilength_by_indx(void* handle, int* struct_indx, 
        int* size_var, int* var);
    void get_data_var_ilength_by_indx(void* handle, int* struct_indx, int* dat);
    void set_data_var_ilength_by_indx(void* handle, int* struct_indx, 
        int* num_var, int* var, int* num_summa_data, int* summa_data);
    
    // var_i_by_indx
    void get_size_data_var_i_by_indx(void* handle, int* indx, int* num_var);
    void get_data_var_i_by_indx(void* handle, int* struct_indx, int* dat);
    void set_data_var_i_by_indx(void* handle, int* struct_indx, int* num_var, 
        int* summa_struct);

    // var_d_by_indx
    void get_size_data_var_d_by_indx(void* handle, int* indx, int* num_var);
    void get_data_var_d_by_indx(void* handle, int* struct_indx, double* dat);
    void set_data_var_d_by_indx(void* handle, int* struct_indx, int* num_var, 
        double* summa_struct);

    // var_i8_by_indx
    void get_size_data_var_i8_by_indx(void* handle, int* indx, int* num_var);
    void get_data_var_i8_by_indx(void* handle, int* struct_indx, long int* dat);
    void set_data_var_i8_by_indx(void* handle, int* struct_indx, int* num_var, 
        long int* summa_struct);

    // flagVec_by_indx
    void get_size_data_flagVec_by_indx(void* handle, int* indx, int* num_var);
    void get_data_flagVec_by_indx(void* handle, int* struct_indx, int* dat);
    void set_data_flagVec_by_indx(void* handle, int* struct_indx, int* num_var, 
        int* summa_struct);

    // scalar types
    void get_scalar_data_fortran(void* handle, double* fracJulDay, 
                                 double* tmZoneOffsetFracDay, int* year_length, 
                                 int* computeVegFlux, double* dt_init,
                                 double* upArea);
    void set_scalar_data_fortran(void* handle, double* fracJulDay,
                                 double* tmZoneOffsetFracDay, int* year_length, 
                                 int* computeVegFlux, double* dt_init,
                                 double* upArea);
}