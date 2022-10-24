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

// void set_var_info(VarInfo v, void* handle) {
//     set_data_var_info(handle, v.varname, v.vardesc, v.varunit, v.vartype,
//                         &v.ncVarID[0], v.ncVarID.size(),  &v.statIndex[0], v.statIndex.size(), v.varDesire);
// }

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


std::vector<double> get_var_d(void* handle) {
    int size;
    get_size_data_var_d(handle, &size);
    if (size == 0) return std::vector<double>();

    std::vector<double> array(size);
    get_data_var_d(handle, &array[0]);
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