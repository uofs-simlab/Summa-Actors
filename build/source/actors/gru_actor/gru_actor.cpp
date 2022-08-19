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
    self->state.parent = parent;

    return {

        // What does a GRU need to assemble its data structure?
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

            initVarType(&self->state.var_type_lookup);
                // aout(self) << "************C++************\n";
                // aout(self) << self->state.var_type_lookup.scalarv << "\n";
                // aout(self) << self->state.var_type_lookup.wLength << "\n";
                // aout(self) << self->state.var_type_lookup.midSnow << "\n";
                // aout(self) << self->state.var_type_lookup.midSoil << "\n";
                // aout(self) << self->state.var_type_lookup.midToto << "\n";
                // aout(self) << self->state.var_type_lookup.ifcSnow << "\n";
                // aout(self) << self->state.var_type_lookup.ifcSoil << "\n";
                // aout(self) << self->state.var_type_lookup.ifcToto << "\n";
                // aout(self) << self->state.var_type_lookup.parSoil << "\n";
                // aout(self) << self->state.var_type_lookup.routing << "\n";
                // aout(self) << self->state.var_type_lookup.outstat << "\n";
                // aout(self) << self->state.var_type_lookup.unknown << "\n";
                // aout(self) << "************C++************\n";

            // Fill The lists with the values from the fortran lists
            int err = 0;
            fillVarTypeLists(&self->state.num_bpar_vars,
                             &self->state.num_bvar_vars,
                             &self->state.bpar_struct_var_type_list[0],
                             &self->state.bvar_struct_var_type_list[0],
                             &err);
            // aout(self) << "Printing BPAR\n";
            // for(int i = 0; i < self->state.num_bpar_vars; i++) {
            //     aout(self) << i << ": " << self->state.bpar_struct_var_type_list[i] << "\n";
            // }
            // aout(self) << "Printing BVAR\n";
            // for(int i = 0; i < self->state.num_bvar_vars; i++) {
            //     aout(self) << i << ": " << self->state.bvar_struct_var_type_list[i] << "\n";
            // }

            // Now we can allocate space for the structures
            for(int i = 0; i < self->state.num_bpar_vars; i++) {
                if (self->state.bpar_struct_var_type_list[i] == self->state.var_type_lookup.scalarv) {
                    self->state.bpar_struct.push_back(0.0);
                } else {
                    aout(self) << "ERROR: GRU - bpar_struct contains type that is not a scalar\n";
                }
            }
            for(int i = 0; i < self->state.num_bvar_vars; i++) {
                if (self->state.bvar_struct_var_type_list[i] == self->state.var_type_lookup.scalarv) {
                    std::vector<double> temp;
                    self->state.bvar_struct.push_back(temp);
                    self->state.bvar_struct[i].push_back(0.0);

                } else if(self->state.bvar_struct_var_type_list[i] == self->state.var_type_lookup.routing) {
                    std::vector<double> temp;
                    self->state.bvar_struct.push_back(temp);
                    for (int x = 0; x < self->state.nTimeDelay; x++) {
                        self->state.bvar_struct[i].push_back(0.0);
                    }
                } else {
                    aout(self) << "ERROR: GRU - bvar_struct contains type that is not a scalar or routing\n";

                }
            }

            self->send(self->state.parent, done_init_gru_v);
        }


    };
}

}