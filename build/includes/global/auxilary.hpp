#pragma once
// Setters
void set_flagVec(std::vector<int>& arr_i, void* handle);
void set_var_i(std::vector<int>& arr_i, void* handle);
void set_var_d(std::vector<double> &arr_d, void* handle);
void set_var_i8(std::vector<long int>& arr_i, void* handle);
void set_i8length(std::vector<long int> &arr_i8length, void* handle);
void set_ilength(std::vector<int> &arr_ilength, void* handle);
void set_dlength(std::vector<double> &arr_dlength, void* handle);
void set_var_flagVec(std::vector<std::vector<int> > &mat, void* handle);
void set_var_ilength(std::vector<std::vector<int> > &mat, void* handle);
void set_var_i8length(std::vector<std::vector<long int> > &mat, void* handle);
void set_var_dlength(std::vector<std::vector<double> > &mat, void *handle);
// Getters
std::vector<int> get_flagVec(void* handle);
std::vector<int> get_var_i(void* handle);
std::vector<double> get_var_d(void* handle);
std::vector<long int> get_var_i8(void* handle);
std::vector<long int> get_i8length(void* handle);
std::vector<int> get_ilength(void* handle);
std::vector<double> get_dlength(void* handle);
std::vector<std::vector<int> > get_var_flagVec(void* handle);
std::vector<std::vector<int> > get_var_ilength(void* handle);
std::vector<std::vector<long int> > get_var_i8length(void* handle);
std::vector<std::vector<double> > get_var_dlength(void* handle);
std::vector<std::vector<std::vector<double>>> get_lookup_struct(void *handle);


std::vector<double> get_attr_struct(void* handle);
std::vector<int> get_type_struct(void* handle);
std::vector<std::vector<double>> get_mpar_struct_array(void* handle);
std::vector<double> get_bpar_struct(void* handle);

// Getters by index
std::vector<std::vector<double>> get_var_dlength_by_indx(void* handle, 
    int struct_indx);
std::vector<std::vector<int>> get_var_ilength_by_indx(void* handle,
    int struct_indx);
std::vector<int> get_var_i_by_indx(void* handle, int struct_indx);
std::vector<double> get_var_d_by_indx(void* handle, int struct_indx);
std::vector<long int> get_var_i8_by_indx(void* handle, int struct_indx);
std::vector<int> get_flagVec_by_indx(void* handle, int struct_indx);

void get_scalar_data(void* handle, double fracJulDay, 
                     double tmZoneOffsetFracDay, int year_length, 
                     int computeVegFlux, double dt_init, double upArea);

// Setters by index
void set_var_dlength_by_indx(void* handle, 
    std::vector<std::vector<double>>& summa_struct, int struct_indx);
void set_var_ilength_by_indx(void* handle, 
    std::vector<std::vector<int>>& summa_struct, int struct_indx); 

void set_var_i8_by_indx(void* handle, std::vector<long int>& summa_struct, 
    int struct_indx);

void set_var_i_by_indx(void* handle, std::vector<int>& summa_struct, 
    int struct_indx);

void set_var_d_by_indx(void* handle, std::vector<double>& summa_struct, 
    int struct_indx);

void set_flagVec_by_indx(void* handle, std::vector<int>& summa_struct, 
    int struct_indx);

void set_scalar_data(void* handle, double fracJulDay, 
                     double tmZoneOffsetFracDay, int year_length, 
                     int computeVegFlux, double dt_init, double upArea);