#include "caf/all.hpp"
#include "hru_actor.hpp"
#include "global.hpp"
#include "message_atoms.hpp"
#include "hru_actor_subroutine_wrappers.hpp"
#include "serialize_data_structure.hpp"
#include <thread>


namespace caf {

behavior hru_actor(stateful_actor<hru_state>* self, int refGRU, int indxGRU,
                   HRU_Actor_Settings hru_actor_settings, 
                   caf::actor file_access_actor, 
                   caf::actor parent) {
    
    // Actor References
    self->state.file_access_actor = file_access_actor;
    self->state.parent            = parent;
    // Indexes into global structures
    self->state.indxHRU           = 1;
    self->state.indxGRU           = indxGRU;
    self->state.refGRU            = refGRU;
    // initialize counters 
    self->state.timestep          = 1;  
    self->state.forcingStep       = 1;  
    self->state.output_structure_step_index = 1;
    self->state.iFile             = 1;
    // Get the settings for the HRU
    self->state.hru_actor_settings = hru_actor_settings;
    self->state.dt_init_factor = hru_actor_settings.dt_init_factor;


    initHRU(&self->state.indxGRU, 
            &self->state.num_steps, 
            self->state.handle_lookupStruct, 
            self->state.handle_forcStat,
            self->state.handle_progStat, 
            self->state.handle_diagStat, 
            self->state.handle_fluxStat, 
            self->state.handle_indxStat, 
            self->state.handle_bvarStat, 
            self->state.handle_timeStruct, 
            self->state.handle_forcStruct, 
            self->state.handle_indxStruct,
            self->state.handle_progStruct, 
            self->state.handle_diagStruct, 
            self->state.handle_fluxStruct,
            self->state.handle_bvarStruct, 
            self->state.handle_startTime, 
            self->state.handle_finshTime, 
            self->state.handle_refTime,
            self->state.handle_oldTime, 
            &self->state.err);

        
    if (self->state.err != 0) {
        aout(self) << "Error: HRU_Actor - Initialize - HRU = " << self->state.indxHRU  
                   << " - indxGRU = " << self->state.indxGRU 
                   << " - refGRU = "<< self->state.refGRU
                   << "\nError Code = " << self->state.err << "\n";
        self->quit();
    }

    // Initialize flags that are used for the output 
    initStatisticsFlags(self->state.handle_statCounter, 
                        self->state.handle_outputTimeStep, 
                        self->state.handle_resetStats, 
                        self->state.handle_finalizeStats, 
                        &self->state.err);

    // Get the number of timesteps required until needing to write
    self->request(self->state.file_access_actor, 
                  caf::infinite,
                  get_num_output_steps_v)
                  .await([=](int num_steps){
                    self->state.num_steps_until_write = num_steps;
                    self->state.output_structure_step_index = 1;
                  });

    // Get the attributes and parameters for the HRU
    self->request(self->state.file_access_actor,
                  caf::infinite,
                  get_attributes_params_v,
                  self->state.indxGRU)
                  .await([=](std::tuple<std::vector<double>,
                                        std::vector<int>,
                                        std::vector<long int>, 
                                        std::vector<double>, 
                                        std::vector<double>, 
                                        std::vector<std::vector<double>>> attr_and_params) {
                                int err = 0;
                                set_var_d(std::get<0>(attr_and_params), self->state.handle_attrStruct);
                                set_var_i(std::get<1>(attr_and_params), self->state.handle_typeStruct);
                                set_var_i8(std::get<2>(attr_and_params), self->state.handle_idStruct);
                                set_var_d(std::get<3>(attr_and_params), self->state.handle_bparStruct);
                                set_var_d(std::get<4>(attr_and_params), self->state.handle_dparStruct);
                                set_var_dlength(std::get<5>(attr_and_params), self->state.handle_mparStruct);

                                Initialize_HRU(self);

                                self->send(self, start_hru_v); });


    return {

        // First method called after initialization, starts the HRU and the HRU asks
        // for parameters and forcing data from the file_access_actor
        [=](start_hru) {
            int err = 0;
            std::vector<double> attr_struct_array = get_var_d(self->state.handle_attrStruct); 
            std::vector<int> type_struct_array    = get_var_i(self->state.handle_typeStruct);
            std::vector<std::vector<double>> mpar_struct_array = get_var_dlength(self->state.handle_mparStruct);
            std::vector<double> bpar_struct_array = get_var_d(self->state.handle_bparStruct);

            // ask file_access_actor to write parameters
            self->send(self->state.file_access_actor, 
                       write_param_v, 
                       self->state.indxGRU, 
                       self->state.indxHRU, 
                       attr_struct_array,
                       type_struct_array, 
                       mpar_struct_array, 
                       bpar_struct_array);
            
            // ask file_access_actor for forcing data
            self->send(self->state.file_access_actor,
                       access_forcing_v, 
                       self->state.iFile, 
                       self);
        },
        [=](num_steps_before_write, int num_steps) {
            self->state.num_steps_until_write = num_steps;
            self->state.output_structure_step_index = 1;
        },

        // Run HRU for a number of timesteps
        [=](run_hru) {
            int err = 0;

            if (self->state.timestep == 1) {
                getFirstTimestep(&self->state.iFile, &self->state.forcingStep, &err);
                if (self->state.forcingStep == -1) { aout(self) << "HRU - Wrong starting forcing file\n";} 
            }

            while(self->state.num_steps_until_write > 0) {
                if (self->state.forcingStep > self->state.stepsInCurrentFFile) {
                    self->send(self->state.file_access_actor, access_forcing_v, self->state.iFile+1, self);
                    break;
                }

                self->state.num_steps_until_write--;

                err = Run_HRU(self); // Simulate a Timestep

                if (err != 0) {
                    // We should have already printed the error to the screen if we get here

                    self->send(self->state.parent, hru_error::run_physics_unhandleable, self);
                    self->quit();
                    return;
                }

                writeHRUToOutputStructure(&self->state.indxHRU, 
                                          &self->state.indxGRU, 
                                          &self->state.output_structure_step_index,
                                          self->state.handle_forcStat,
                                          self->state.handle_progStat,
                                          self->state.handle_diagStat,
                                          self->state.handle_fluxStat,
                                          self->state.handle_indxStat,
                                          self->state.handle_bvarStat,
                                          self->state.handle_timeStruct,
                                          self->state.handle_forcStruct,
                                          self->state.handle_indxStruct,
                                          self->state.handle_mparStruct,
                                          self->state.handle_progStruct,
                                          self->state.handle_diagStruct,
                                          self->state.handle_fluxStruct,
                                          self->state.handle_bparStruct,
                                          self->state.handle_bvarStruct,
                                          self->state.handle_statCounter,
                                          self->state.handle_outputTimeStep,
                                          self->state.handle_resetStats,
                                          self->state.handle_finalizeStats,
                                          self->state.handle_finshTime,
                                          self->state.handle_oldTime,
                                          &err);
                if (err != 0) {
                    aout(self) << "Error: HRU_Actor - writeHRUToOutputStructure - HRU = " << self->state.indxHRU
                               << " - indxGRU = " << self->state.indxGRU << " - refGRU = " << self->state.refGRU
                               << "\nError = " << err << "\n";
                    self->send(self->state.parent, hru_error::run_physics_unhandleable, self);
                    self->quit();
                    return;
                }

                self->state.timestep++;
                self->state.forcingStep++;
                self->state.output_structure_step_index++;
 
                if (self->state.timestep > self->state.num_steps) {
                    self->send(self, done_hru_v);
                    break;
                }

            }
            // Our output structure is full
            if (self->state.num_steps_until_write <= 0) {
                self->send(self->state.file_access_actor, write_output_v, self->state.indxGRU, self->state.indxHRU, self);
            }
        },


        [=](new_forcing_file, int num_forcing_steps_in_iFile, int iFile) {
            int err;
            self->state.iFile = iFile;
            self->state.stepsInCurrentFFile = num_forcing_steps_in_iFile;
            setTimeZoneOffset(&self->state.iFile, &self->state.tmZoneOffsetFracDay, &err);
            self->state.forcingStep = 1;
            self->send(self, run_hru_v);
        },

        
        [=](done_hru) {

            self->send(self->state.parent, 
                done_hru_v,
                self->state.indxGRU);
            
            self->quit();
            return;
        },

        [=](dt_init_factor, int dt_init_factor) {
            aout(self) << "Recieved New dt_init_factor to attempt on next run \n";
        },
    };
}



void Initialize_HRU(stateful_actor<hru_state>* self) {

    setupHRUParam(&self->state.indxHRU, 
                  &self->state.indxGRU,
                  self->state.handle_attrStruct, 
                  self->state.handle_typeStruct, 
                  self->state.handle_idStruct,
                  self->state.handle_mparStruct, 
                  self->state.handle_bparStruct, 
                  self->state.handle_bvarStruct,
                  self->state.handle_dparStruct, 
                  self->state.handle_lookupStruct,
                  self->state.handle_startTime, 
                  self->state.handle_oldTime,
                  &self->state.upArea, 
                  &self->state.err);
    if (self->state.err != 0) {
        aout(self) << "Error: HRU_Actor - SetupHRUParam - HRU = " << self->state.indxHRU
                   << " - indxGRU = " << self->state.indxGRU 
                   << " - refGRU = " << self->state.refGRU << "\n";
        self->quit();
        return;
    }
            
    summa_readRestart(&self->state.indxGRU, 
                      &self->state.indxHRU, 
                      self->state.handle_indxStruct, 
                      self->state.handle_mparStruct, 
                      self->state.handle_progStruct,
                      self->state.handle_diagStruct, 
                      self->state.handle_fluxStruct, 
                      self->state.handle_bvarStruct, 
                      &self->state.dt_init, 
                      &self->state.err);
    if (self->state.err != 0) {
        aout(self) << "Error: HRU_Actor - summa_readRestart - HRU = " << self->state.indxHRU
                   << " - indxGRU = " << self->state.indxGRU 
                   << " - refGRU = " << self->state.refGRU << "\n";
        self->quit();
        return;
    }

    // Set HRU Tolerances
    setIDATolerances(self->state.handle_mparStruct, 
                     &self->state.hru_actor_settings.relTolTempCas,
                     &self->state.hru_actor_settings.absTolTempCas,
                     &self->state.hru_actor_settings.relTolTempVeg,
                     &self->state.hru_actor_settings.absTolTempVeg,
                     &self->state.hru_actor_settings.relTolWatVeg,
                     &self->state.hru_actor_settings.absTolWatVeg,
                     &self->state.hru_actor_settings.relTolTempSoilSnow,
                     &self->state.hru_actor_settings.absTolTempSoilSnow,
                     &self->state.hru_actor_settings.relTolWatSnow,
                     &self->state.hru_actor_settings.absTolWatSnow,
                     &self->state.hru_actor_settings.relTolMatric,
                     &self->state.hru_actor_settings.absTolMatric,
                     &self->state.hru_actor_settings.relTolAquifr,
                     &self->state.hru_actor_settings.absTolAquifr);
            
}

int Run_HRU(stateful_actor<hru_state>* self) {
    /**********************************************************************
    ** READ FORCING
    **********************************************************************/    

    readForcingHRU(&self->state.indxGRU,
                   &self->state.timestep,
                   &self->state.forcingStep,
                   self->state.handle_timeStruct,
                   self->state.handle_forcStruct, 
                   &self->state.iFile,
                   &self->state.err);
    if (self->state.err != 0) {
        aout(self) << "Error---HRU_Actor: ReadForcingHRU\n" 
                   << "     IndxGRU = " << self->state.indxGRU << "\n"
                   << "     RefGRU = " << self->state.refGRU << "\n"
                   << "     Forcing Step = " << self->state.forcingStep << "\n"
                   << "     Timestep = " << self->state.timestep << "\n"
                   << "     iFile = " << self->state.iFile << "\n"
                   << "     Steps in Forcing File = " << self->state.stepsInCurrentFFile << "\n";
        self->quit();
        return -1;
    }

    computeTimeForcingHRU(self->state.handle_timeStruct,
                          self->state.handle_forcStruct, 
                          &self->state.fracJulDay,
                          &self->state.yearLength,
                          &self->state.err);
    if (self->state.err != 0) {
        aout(self) << "Error---HRU_Actor - ComputeTimeForcingHRU\n"
                   << "     IndxGRU = " << self->state.indxGRU << "\n"
                   << "     RefGRU = " << self->state.refGRU << "\n"
                   << "     Forcing Step = " << self->state.forcingStep << "\n"
                   << "     Timestep = " << self->state.timestep << "\n"
                   << "     iFile = " << self->state.iFile << "\n"
                   << "     Steps in Forcing File = " << self->state.stepsInCurrentFFile << "\n";
        self->quit();
        return -1;
    }

    if (self->state.hru_actor_settings.print_output && 
        self->state.timestep % self->state.hru_actor_settings.output_frequency == 0) {
        printOutput(self);
    }
    

    /**********************************************************************
    ** RUN_PHYSICS    
    **********************************************************************/    

    self->state.err = 0;
    RunPhysics(&self->state.indxHRU,
        &self->state.timestep,
        self->state.handle_timeStruct, 
        self->state.handle_forcStruct,
        self->state.handle_attrStruct,
        self->state.handle_typeStruct, 
        self->state.handle_indxStruct,
        self->state.handle_mparStruct, 
        self->state.handle_progStruct,
        self->state.handle_diagStruct,
        self->state.handle_fluxStruct,
        self->state.handle_bvarStruct,
        self->state.handle_lookupStruct,
        &self->state.fracJulDay,
        &self->state.tmZoneOffsetFracDay,
        &self->state.yearLength,
        &self->state.computeVegFlux,
        &self->state.dt_init, 
        &self->state.dt_init_factor,
        &self->state.err);

    if (self->state.err != 0) {
        aout(self) << "Error---RunPhysics:\n"
                   << "     IndxGRU = " << self->state.indxGRU 
                   << "     RefGRU = " << self->state.refGRU 
                   << "     Timestep = " << self->state.timestep <<  "\n";
        self->quit();
        return 20;
    }

    return 0;      
}


void printOutput(stateful_actor<hru_state>* self) {
        aout(self) << self->state.refGRU << " - Timestep = " << self->state.timestep << "\n";
}

}