#pragma once

extern "C" {

    void getVarSizes(int* num_var_types, int* num_bpar_vars, int* num_bvar_vars);

    void fillVarTypeLists(int* num_var_types, int* num_bpar_vars, int* num_bvar_vars, void* i_look_var_type_list, 
        void* bpar_struct_var_type_list, void* bvar_struct_var_type_list);

}