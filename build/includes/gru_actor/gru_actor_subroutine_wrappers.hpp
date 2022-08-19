#pragma once

extern "C" {

    void getVarSizes(int* num_var_types, int* num_bpar_vars, int* num_bvar_vars);

    void initVarType(void* var_type_lookup);

    void fillVarTypeLists( int* num_bpar_vars, int* num_bvar_vars,
        void* bpar_struct_var_type_list, void* bvar_struct_var_type_list, int* err);

}