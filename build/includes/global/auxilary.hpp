#pragma once


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