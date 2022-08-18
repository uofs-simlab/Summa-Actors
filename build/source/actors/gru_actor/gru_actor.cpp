#include "caf/all.hpp"
#include "gru_actor.hpp"
#include "global.hpp"
#include "message_atoms.hpp"
#include <vector>
#include "gru_actor_subroutine_wrappers.hpp"

namespace caf {

behavior gru_actor(stateful_actor<gru_state>* self, int refGRU, int indxGRU, 
    std::string configPath, int outputStrucSize, caf::actor parent) {

    aout(self) << "GRU Actor Has Started\n";

    return {


        // What does a GRU need to assemble its data structure?
        //
        [=](init_gru) {
            // Get the variable data length, we also need the type information
            aout(self) << "init GRU \n";
            int integer_missing_value = -9999;

            // Get the sizes we need for the lists from Fortran
            getVarSizes(&self->state.num_var_types,
                        &self->state.num_bpar_vars,
                        &self->state.num_bvar_vars);
            
            aout(self) << "GRU: GOT VAR SIZES\n";
            aout(self) << "NUM VAR Type = " << self->state.num_var_types << "\n";
            aout(self) << "NUM BPAR = " << self->state.num_bpar_vars << "\n";
            aout(self) << "NUM BVAR = " << self->state.num_bvar_vars << "\n";

            for (int i = 0; i < self->state.num_var_types; i++) {
                self->state.i_look_var_type_list.push_back(integer_missing_value);
            }
            for (int i = 0; i < self->state.num_bpar_vars; i++) {
                self->state.bpar_struct_var_type_list.push_back(integer_missing_value);
            }
            for (int i = 0; i < self->state.num_bvar_vars; i++) {
                self->state.bvar_struct_var_type_list.push_back(integer_missing_value);
            }

            // Fill The lists with the values from the fortran lists
            fillVarTypeLists(&self->state.num_var_types,
                             &self->state.num_bpar_vars,
                             &self->state.num_bvar_vars,
                             &self->state.i_look_var_type_list[0],
                             &self->state.bpar_struct_var_type_list[0],
                             &self->state.bvar_struct_var_type_list[0]);


        }


    };
}

}