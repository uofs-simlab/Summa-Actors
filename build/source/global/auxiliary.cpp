#include <iostream>
#include <stdio.h>
#include <vector>
#include <string.h>
#include "fortran_data_types.hpp"
#include "auxilary.hpp"

/*****
 * These are all of the functions to get the Fortran data types into C
 * /
/*************** SET DATA **************/  
void set_flagVec(std::vector<int>& arr_i, void* handle) {
    set_data_flagVec(handle, &arr_i[0], arr_i.size());
}

void set_var_i(std::vector<int>& arr_i, void* handle) {
    set_data_var_i(handle, &arr_i[0], arr_i.size());
}

void set_var_d(std::vector<double> &arr_d, void* handle) {
    set_data_var_d(handle, &arr_d[0], arr_d.size());
}

void set_var_i8(std::vector<long int>& arr_i, void* handle) {
    set_data_var_i8(handle, &arr_i[0], arr_i.size());
}

void set_i8length(std::vector<long int> &arr_i8length, void* handle) {
    set_data_i8length(handle, &arr_i8length[0], arr_i8length.size());
}

void set_ilength(std::vector<int> &arr_ilength, void* handle) {
    set_data_ilength(handle, &arr_ilength[0], arr_ilength.size());
}

void set_dlength(std::vector<double> &arr_dlength, void* handle) {
    set_data_dlength(handle, &arr_dlength[0], arr_dlength.size());
}

void set_var_flagVec(std::vector<std::vector<int> > &mat, void* handle) {

    size_t num_row = mat.size();
    std::vector<int> num_col( num_row );
    std::vector<int> array;
    
    int num_elements = 0;
    for(size_t i=0; i<num_row; i++) {
        num_col[i] = mat[i].size();
        for(size_t j=0; j<num_col[i]; j++)
        array.push_back(mat[i][j]);
        num_elements += num_col[i];
    }
    
    set_data_var_flagVec(handle, &array[0], num_row, &num_col[0], num_elements);
}

void set_var_ilength(std::vector<std::vector<int> > &mat, void* handle) {

    size_t num_row = mat.size();
    std::vector<int> num_col( num_row );
    std::vector<int> array;
    
    int num_elements = 0;
    for(size_t i=0; i<num_row; i++) {
        num_col[i] = mat[i].size();
        for(size_t j=0; j<num_col[i]; j++)
        array.push_back(mat[i][j]);
        num_elements += num_col[i];
    }
    
    set_data_var_ilength(handle, &array[0], num_row, &num_col[0], num_elements);
}

void set_var_i8length(std::vector<std::vector<long int> > &mat, void* handle) {

    size_t num_row = mat.size();
    std::vector<int> num_col( num_row );
    std::vector<long int> array;
    
    int num_elements = 0;
    for(size_t i=0; i<num_row; i++) {
        num_col[i] = mat[i].size();
        for(size_t j=0; j<num_col[i]; j++)
        array.push_back(mat[i][j]);
        num_elements += num_col[i];
    }
    
    set_data_var_i8length(handle, &array[0], num_row, &num_col[0], num_elements);
}

void set_var_dlength(std::vector<std::vector<double> > &mat, void *handle) {

    size_t num_row = mat.size();
    std::vector<int> num_col( num_row );
    std::vector<double> array;
    
    int num_elements = 0;
    for(size_t i=0; i<num_row; i++) {
        num_col[i] = mat[i].size();
        for(size_t j=0; j<num_col[i]; j++)
        array.push_back(mat[i][j]);
        num_elements += num_col[i];
    }
    
    set_data_var_dlength(handle, &array[0], num_row, &num_col[0], num_elements);
}


/*************** GET DATA **************/

std::vector<int> get_flagVec(void* handle) {
    int size;
    get_size_data_flagVec(handle, &size);
    if (size == 0) return std::vector<int>();

    std::vector<int> array(size);
    get_data_flagVec(handle, &array[0]);
    return array;
}

std::vector<int> get_var_i(void* handle) {
    int size;
    get_size_data_var_i(handle, &size);
    if (size == 0) return std::vector<int>();

    std::vector<int> array(size);
    get_data_var_i(handle, &array[0]);
    return array;
}

std::vector<int> get_type_struct(void* handle) {
    int size;
    get_size_data_typeStruct(handle, &size);
    if (size == 0) return std::vector<int>();

    std::vector<int> array(size);
    get_data_typeStruct(handle, &array[0]);
    return array;
}


std::vector<double> get_var_d(void* handle) {
    int size;
    get_size_data_var_d(handle, &size);
    if (size == 0) return std::vector<double>();

    std::vector<double> array(size);
    get_data_var_d(handle, &array[0]);
    return array;
}

std::vector<double> get_attr_struct(void* handle) {
    int size;
    get_size_data_attrStruct(handle, &size);
    if (size == 0) return std::vector<double>();

    std::vector<double> array(size);
    get_data_attrStruct(handle, &array[0]);
    return array;
}

std::vector<double> get_bpar_struct(void* handle) {
    int size;
    get_size_data_bparStruct(handle, &size);
    if (size == 0) return std::vector<double>();

    std::vector<double> array(size);
    get_data_bparStruct(handle, &array[0]);
    return array;
}

std::vector<long int> get_var_i8(void* handle) {
    int size;
    get_size_data_var_i8(handle, &size);
    if (size == 0) return std::vector<long int>();

    std::vector<long int> array(size);
    get_data_var_i8(handle, &array[0]);
    return array;
}

std::vector<long int> get_i8length(void* handle) {
    int size;
    get_size_data_i8length(handle, &size);
    if (size == 0) return std::vector<long int>();

    std::vector<long int> array(size);
    get_data_i8length(handle, &array[0]);
    return array;
}

std::vector<int> get_ilength(void* handle) {
    int size;
    get_size_data_ilength(handle, &size);
    if (size == 0) return std::vector<int>();

    std::vector<int> array(size);
    get_data_ilength(handle, &array[0]);
    return array;
}

std::vector<double> get_dlength(void* handle) {
    int size;
    get_size_data_dlength(handle, &size);
    if (size == 0) return std::vector<double>();

    std::vector<double> array(size);
    get_data_dlength(handle, &array[0]);
    return array;
}



std::vector<std::vector<int> > get_var_flagVec(void* handle) {
    int num_row;
    get_size_var_flagVec(handle, &num_row);
    if (num_row == 0) return std::vector<std::vector<int> >();

    std::vector<int> num_col(num_row);
    get_size_data_var_flagVec(handle, &num_row, &num_col[0]);

    int num_elem = 0;
    for(int i=0; i<num_row; i++)
        num_elem += num_col[i];   	

    std::vector<int> array(num_elem);

    get_data_var_flagVec(handle, &array[0]);

    std::vector<std::vector<int> > mat(num_row);
    for(size_t i=0; i<num_row; i++)
        mat[i] = std::vector<int>(num_col[i]);

    num_elem = 0;
    for(size_t i=0; i<num_row; i++){
        for(size_t j=0; j<num_col[i]; j++)
            mat[i][j] = array[num_elem + j];
        num_elem += num_col[i];    		
    }


    return mat;
}

std::vector<std::vector<int> > get_var_ilength(void* handle) {
    int num_row;
    get_size_var_ilength(handle, &num_row);
    if (num_row == 0) return std::vector<std::vector<int> >();

    std::vector<int> num_col(num_row);
    get_size_data_var_ilength(handle, &num_row, &num_col[0]);

    int num_elem = 0;
    for(int i=0; i<num_row; i++)
        num_elem += num_col[i];   	

    std::vector<int> array(num_elem);

    get_data_var_ilength(handle, &array[0]);

    std::vector<std::vector<int> > mat(num_row);
    for(size_t i=0; i<num_row; i++)
        mat[i] = std::vector<int>(num_col[i]);

    num_elem = 0;
    for(size_t i=0; i<num_row; i++){
        for(size_t j=0; j<num_col[i]; j++)
            mat[i][j] = array[num_elem + j];
        num_elem += num_col[i];    		
    }    
    return mat;
}

std::vector<std::vector<long int> > get_var_i8length(void* handle) {
    int num_row;
    get_size_var_i8length(handle, &num_row);
    if (num_row == 0) return std::vector<std::vector<long int> >();

    std::vector<int> num_col(num_row);
    get_size_data_var_i8length(handle, &num_row, &num_col[0]);

    int num_elem = 0;
    for(int i=0; i<num_row; i++)
        num_elem += num_col[i];   	

    std::vector<long int> array(num_elem);

    get_data_var_i8length(handle, &array[0]);

    std::vector<std::vector<long int> > mat(num_row);
    for(size_t i=0; i<num_row; i++)
        mat[i] = std::vector<long int>(num_col[i]);

    num_elem = 0;
    for(size_t i=0; i<num_row; i++){
        for(size_t j=0; j<num_col[i]; j++)
            mat[i][j] = array[num_elem + j];
        num_elem += num_col[i];    		
    }    
    return mat;
}

std::vector<std::vector<double> > get_var_dlength(void* handle) {
    int num_row;
    get_size_var_dlength(handle, &num_row);
    if (num_row == 0) return std::vector<std::vector<double> >();

    std::vector<int> num_col(num_row);
    get_size_data_var_dlength(handle, &num_row, &num_col[0]);

    int num_elem = 0;
    for(int i=0; i<num_row; i++)
        num_elem += num_col[i];   	

    std::vector<double> array(num_elem);

    get_data_var_dlength(handle, &array[0]);

    std::vector<std::vector<double> > mat(num_row);
    for(size_t i=0; i<num_row; i++)
        mat[i] = std::vector<double>(num_col[i]);

    num_elem = 0;
    for(size_t i=0; i<num_row; i++){
        for(size_t j=0; j<num_col[i]; j++)
            mat[i][j] = array[num_elem + j];
        num_elem += num_col[i];    		
    }
        
    return mat;
}

std::vector<std::vector<double>> get_mpar_struct_array(void* handle) {
    int num_row;
    get_size_var_mparStruct(handle, &num_row);
    if (num_row == 0) return std::vector<std::vector<double> >();

    std::vector<int> num_col(num_row);
    get_size_data_mparStruct(handle, &num_row, &num_col[0]);

    int num_elem = 0;
    for(int i=0; i<num_row; i++)
        num_elem += num_col[i];   	

    std::vector<double> array(num_elem);

    get_data_mparStruct(handle, &array[0]);

    std::vector<std::vector<double> > mat(num_row);
    for(size_t i=0; i<num_row; i++)
        mat[i] = std::vector<double>(num_col[i]);

    num_elem = 0;
    for(size_t i=0; i<num_row; i++){
        for(size_t j=0; j<num_col[i]; j++)
            mat[i][j] = array[num_elem + j];
        num_elem += num_col[i];    		
    }
        
    return mat;
}


// HRU Data Serialization
// struct_indx maps to the following:
// 1: forc_stat
// 2: prog_stat
// 3: diag_stat
// 4: flux_stat
// 5: indx_stat
// 6: bvar_stat
// 7: mpar_struct
// 8: prog_struct
// 9: diag_struct
// 10: flux_struct
// 11: bvarStruct
std::vector<std::vector<double>> get_var_dlength_by_indx(void* handle, 
    int struct_indx) {
  int size_var;
  get_size_var_dlength_by_indx(handle, &struct_indx, &size_var);
  if (size_var == 0) return std::vector<std::vector<double>>();

  std::vector<int> var(size_var);
  get_size_data_var_dlength_by_indx(handle, &struct_indx, &size_var, &var[0]);

  int num_elem = 0;
  for(int i=0; i<size_var; i++)
    num_elem += var[i];

  std::vector<double> dat(num_elem);
  get_data_var_dlength_by_indx(handle, &struct_indx, &dat[0]);

  std::vector<std::vector<double>> hru_struct(size_var);
  for(size_t i=0; i<size_var; i++)
    hru_struct[i] = std::vector<double>(var[i]);
  
  num_elem = 0;
  for(size_t i=0; i<size_var; i++){
    for(size_t j=0; j<var[i]; j++)
      hru_struct[i][j] = dat[num_elem + j];
    num_elem += var[i];    		
  }
  
  return hru_struct;
}

// struct_indx maps to the following:
// 1: indxStruct
std::vector<std::vector<int>> get_var_ilength_by_indx(void* handle,
    int struct_indx) {
  
  int size_var;
  get_size_var_ilength_by_indx(handle, &struct_indx, &size_var);
  if (size_var == 0) return std::vector<std::vector<int>>();

  std::vector<int> var(size_var);
  get_size_data_var_ilength_by_indx(handle, &struct_indx, &size_var, &var[0]);

  int num_elem = 0;
  for(int i=0; i<size_var; i++)
    num_elem += var[i];

  std::vector<int> dat(num_elem);
  get_data_var_ilength_by_indx(handle, &struct_indx, &dat[0]);

  std::vector<std::vector<int>> hru_struct(size_var);
  for(size_t i=0; i<size_var; i++)
    hru_struct[i] = std::vector<int>(var[i]);
  
  num_elem = 0;
  for(size_t i=0; i<size_var; i++){
    for(size_t j=0; j<var[i]; j++)
      hru_struct[i][j] = dat[num_elem + j];
    num_elem += var[i];    		
  }

  return hru_struct;
}


// struct_indx maps to the following:
// 1: time_struct
// 2: type_struct
// 3: start_time
// 4: end_time
// 5: ref_time
// 6: old_time
// 7: stat_counter
// 8: output_timestep 
std::vector<int> get_var_i_by_indx(void* handle, int struct_indx) {
  int size_var;
  get_size_data_var_i_by_indx(handle, &struct_indx, &size_var);
  if (size_var == 0) return std::vector<int>();

  std::vector<int> array(size_var);
  get_data_var_i_by_indx(handle, &struct_indx, &array[0]);
  return array;
}

// Struct_indx maps to the following:
// 1: forc_struct
// 2: attr_struct
// 3: bpar_struct
// 4: dpar_struct
std::vector<double> get_var_d_by_indx(void* handle, int struct_indx) {
  int size_var;
  get_size_data_var_d_by_indx(handle, &struct_indx, &size_var);
  if (size_var == 0) return std::vector<double>();

  std::vector<double> array(size_var);
  get_data_var_d_by_indx(handle, &struct_indx, &array[0]);
  return array;
}

// struct_indx maps to the following:
// 1: id_struct
std::vector<long int> get_var_i8_by_indx(void* handle, int struct_indx) {
  int size_var;
  get_size_data_var_i8_by_indx(handle, &struct_indx, &size_var);
  if (size_var == 0) return std::vector<long int>();

  std::vector<long int> array(size_var);
  get_data_var_i8_by_indx(handle, &struct_indx, &array[0]);
  return array;
}

// struct_indx maps to the following:
// 1: reset_stats
// 2: finalize_stats
std::vector<int> get_flagVec_by_indx(void* handle, int struct_indx) {
  int size;
  get_size_data_flagVec_by_indx(handle, &struct_indx, &size);
  if (size == 0) return std::vector<int>();

  std::vector<int> array(size);
  get_data_flagVec_by_indx(handle, &struct_indx, &array[0]);
  return array;
}

std::vector<std::vector<std::vector<double>>> get_lookup_struct(void *handle) {
  int size_z;
  get_size_z_lookup(handle, &size_z);
  if (size_z == 0) return std::vector<std::vector<std::vector<double>>>();


  std::vector<std::vector<std::vector<double>>> lookup_struct;
  for (int z = 1; z <= size_z; z++) {
    int size_var;
    get_size_var_lookup(handle, &z, &size_var);
    std::vector<std::vector<double>> lookup_var(size_var);
    lookup_struct.push_back(lookup_var);
    for(int var = 1; var <= size_var; var++) {
      int size_data;
      get_size_data_lookup(handle, &z, &var, &size_data);
      std::vector<double> lookup(size_data);
      get_data_zlookup(handle, &z, &var, &lookup[0]);
      lookup_var[var] = lookup;
    }
  }

  return lookup_struct;
} 


// HRU Data Serialization
// struct_indx maps to the following:
// 1: forc_stat
// 2: prog_stat
// 3: diag_stat
// 4: flux_stat
// 5: indx_stat
// 6: bvar_stat
// 7: mpar_struct
// 8: prog_struct
// 9: diag_struct
// 10: flux_struct
// 11: bvarStruct
void set_var_dlength_by_indx(void* handle, 
    std::vector<std::vector<double>>& summa_struct, int struct_indx) {
  
  int num_var = summa_struct.size();
  std::vector<int> var(num_var);
  std::vector<double> dat_array;

  int num_elem = 0;
  for (size_t i=0; i<num_var; i++) {
    var[i] = summa_struct[i].size();
    for (size_t j=0; j<var[i]; j++)
      dat_array.push_back(summa_struct[i][j]);
    num_elem += var[i];
  }

  set_data_var_dlength_by_indx(handle, &struct_indx, &num_var, &var[0],
      &num_elem, &dat_array[0]);
}

// struct_indx maps to the following:
// 1: indxStruct
void set_var_ilength_by_indx(void* handle,
    std::vector<std::vector<int>>& summa_struct, int struct_indx) {
  
  int num_var = summa_struct.size();
  std::vector<int> var(num_var);
  std::vector<int> dat_array;

  int num_elem = 0;
  for (size_t i=0; i<num_var; i++) {
    var[i] = summa_struct[i].size();
    for (size_t j=0; j<var[i]; j++)
      dat_array.push_back(summa_struct[i][j]);
    num_elem += var[i];
  }

  set_data_var_ilength_by_indx(handle, &struct_indx, &num_var, &var[0],
      &num_elem, &dat_array[0]);
  
}

// struct_indx maps to the following:
// 1: id_struct
void set_var_i8_by_indx(void* handle, std::vector<long int>& summa_struct, 
    int struct_indx) {
  int num_var = summa_struct.size();
  set_data_var_i8_by_indx(handle, &struct_indx, &num_var, &summa_struct[0]);
}

// struct_indx maps to the following:
// 1: time_struct
// 2: type_struct
// 3: start_time
// 4: end_time
// 5: ref_time
// 6: old_time
// 7: stat_counter
// 8: output_timestep 
void set_var_i_by_indx(void* handle, std::vector<int>& summa_struct, 
    int struct_indx) {
  int num_var = summa_struct.size();
  set_data_var_i_by_indx(handle, &struct_indx, &num_var, &summa_struct[0]);
}

// Struct_indx maps to the following:
// 1: forc_struct
// 2: attr_struct
// 3: bpar_struct
// 4: dpar_struct
void set_var_d_by_indx(void* handle, std::vector<double>& summa_struct, 
    int struct_indx) {
  int num_var = summa_struct.size();
  set_data_var_d_by_indx(handle, &struct_indx, &num_var, &summa_struct[0]);
}

// struct_indx maps to the following:
// 1: reset_stats
// 2: finalize_stats
void set_flagVec_by_indx(void* handle, std::vector<int>& summa_struct, 
    int struct_indx) {
  int num_var = summa_struct.size();
  set_data_flagVec_by_indx(handle, &struct_indx, &num_var, &summa_struct[0]);
}