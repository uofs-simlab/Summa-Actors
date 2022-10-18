#pragma once

template<typename T>
struct Summa_Variable {
    std::vector<T> dat; // needs to be general as each array can have its own type
    int var_type;  // maps to varType() in Fortran-Side
    int var_fortran_index; // maps to the index of the var portion of the summa_variable
};

extern "C" {
    void getSummaVariableInfo(int* var_type, int* var_fortran_index, void* data_struct);
}




template <typename T>
std::vector<Summa_Variable<T>> init_summa_variable_vector(int num_variables) {
    std::vector<Summa_Variable<T>> summa_variables(num_variables);

    int variable_index = 1; // value to index into the fortran structure of the array
    for (Summa_Variable var : summa_variables) {
        var.var_fortran_index = variable_index;
        getSummaVariableInfo(var.var_type, var.var_fortran_index);
        variable_index++;
    }

    return summa_variables;
}



