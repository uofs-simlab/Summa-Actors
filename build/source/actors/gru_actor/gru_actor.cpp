#include "caf/all.hpp"
#include "gru_actor.hpp"
#include "global.hpp"
#include "hru_actor.hpp"
#include "message_atoms.hpp"
#include <vector>
#include "gru_actor_subroutine_wrappers.hpp"

namespace caf {

behavior gru_actor(stateful_actor<gru_state>* self, 
    int ref_gru, int indx_gru, 
    std::string config_path, caf::actor file_access_actor, int output_struc_size, 
    caf::actor parent) {

    aout(self) << "GRU Actor Has Started\n";
    self->state.parent = parent;
    self->state.ref_gru = ref_gru;
    self->state.indx_gru = indx_gru;
    self->state.config_path = config_path;
    self->state.file_access_actor = file_access_actor;
    self->state.output_struc_size = output_struc_size;
    self->state.parent = parent;

    self->state.num_hrus = getNumHRU(&self->state.indx_gru);

    self->send(self, init_hru_v);

    return {

        [=](init_hru) {
            for (int i = 0; i < self->state.num_hrus; i++) {
                // auto hru = self->spawn(hru_actor,
                //         self->state.ref_gru, self->state.indx_gru, 
                //         self->state.config_path, self->state.file_access_actor,
                //         self->state.output_struc_size,
                //         self);
                // self->state.hru_list.push_back(hru);
            }
        },

        [=](done_hru, int indx_gru, double total_duration, double init_duration, 
            double forcing_duration, double run_physics_duration, double write_output_duration) {
            aout(self) << "GRU Received HRU is Done\n";

            self->state.hrus_complete++;
            if (self->state.hrus_complete >= self->state.num_hrus) {
                aout(self) << "All HRUs have finished";

                self->send(self->state.parent,
                    done_hru_v,
                    indx_gru, 
                    total_duration,
                    init_duration, 
                    forcing_duration,
                    run_physics_duration,
                    write_output_duration);
            }
        
        },

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