#include "caf/all.hpp"
#include "hru_actor.hpp"
#include "global.hpp"
#include "message_atoms.hpp"
#include "hru_actor_subroutine_wrappers.hpp"
#include "serialize_data_structure.hpp"


namespace caf {

behavior hru_actor(stateful_actor<hru_state>* self, int refGRU, int indxGRU,
    std::string configPath, caf::actor file_access_actor, int outputStrucSize, caf::actor parent) {
    
    // Timing Information
    self->state.hru_timing = TimingInfo();
    self->state.hru_timing.addTimePoint("total_duration");
    self->state.hru_timing.updateStartPoint("total_duration");
    // Add the rest of the timing
    self->state.hru_timing.addTimePoint("init_duration");
    self->state.hru_timing.addTimePoint("forcing_duration");
    self->state.hru_timing.addTimePoint("run_physics_duration");
    self->state.hru_timing.addTimePoint("write_output_duration");

    // Actor References
    self->state.file_access_actor = file_access_actor;
    self->state.parent      = parent;

    // Indexes into global structures
    self->state.indxHRU     = 1;
    self->state.indxGRU     = indxGRU;
    self->state.refGRU      = refGRU;

    // OutputStructure Size (how many timesteps we can compute before we need to write)
    self->state.outputStrucSize = outputStrucSize;

    // initialize counters 
    self->state.timestep   = 1;     // Timestep of total simulation
    self->state.outputStep = 1;     // Index of the output structure
    self->state.forcingStep = 1;    // Index into the forcing file
    self->state.iFile = 1;

    self->state.printOutput = getSettings(configPath, "HRUActor", "printOutput", 
		self->state.printOutput).value_or(true);
    self->state.outputFrequency = getSettings(configPath, "HRUActor", "outputFrequency", 
		self->state.outputFrequency).value_or(500);
    
    // We only want to print this once
    if (indxGRU == 1) {
        aout(self) << "\nSETTINGS FOR HRU_ACTOR\n";
        aout(self) << "Print Output = " << self->state.printOutput << "\n";
        aout(self) << "Print Output every " << self->state.outputFrequency << " timesteps\n\n";
    }


    initHRU(&self->state.indxGRU, &self->state.num_steps, self->state.handle_lookupStruct, self->state.handle_forcStat,
        self->state.handle_progStat, self->state.handle_diagStat, self->state.handle_fluxStat, self->state.handle_indxStat, 
        self->state.handle_bvarStat, self->state.handle_timeStruct, self->state.handle_forcStruct, self->state.handle_indxStruct,
        self->state.handle_progStruct, self->state.handle_diagStruct, self->state.handle_fluxStruct,
        self->state.handle_bvarStruct, self->state.handle_startTime, self->state.handle_finshTime, 
        self->state.handle_refTime,self->state.handle_oldTime, &self->state.err);
    if (self->state.err != 0) {
        aout(self) << "Error: HRU_Actor - Initialize - HRU = " << self->state.indxHRU << 
            " - indxGRU = " << self->state.indxGRU << " - refGRU = "<< self->state.refGRU << std::endl;
        aout(self) << "Error = " << self->state.err << "\n";
        self->quit();
    }


    // Get attributes
    self->send(self->state.file_access_actor, get_attributes_v, self->state.refGRU, self);


    self->state.hru_timing.updateEndPoint("total_duration");

    return {
        // Starts the HRU and tells it to ask for data from the file_access_actor
        [=](get_attributes, std::vector<double> attr_struct, std::vector<int> type_struct, 
            std::vector<long int> id_struct, std::vector<double> bpar_struct, 
            std::vector<double> dpar_struct, std::vector<std::vector<double>> mpar_struct) {
            
            int err = 0;
            set_var_d(attr_struct, self->state.handle_attrStruct);
            set_var_i(type_struct, self->state.handle_typeStruct);
            set_var_i8(id_struct, self->state.handle_idStruct);
            set_var_d(bpar_struct, self->state.handle_bparStruct);
            set_var_d(dpar_struct, self->state.handle_dparStruct);
            set_var_dlength(mpar_struct, self->state.handle_mparStruct);

            Initialize_HRU(self);

            self->send(self, start_hru_v);

        },


        [=](start_hru) {
            self->state.hru_timing.updateStartPoint("total_duration");

            int err = 0;
            std::vector<double> attr_struct_array = get_var_d(self->state.handle_attrStruct); 
            std::vector<int> type_struct_array    = get_var_i(self->state.handle_typeStruct);
            std::vector<std::vector<double>> mpar_struct_array = get_var_dlength(self->state.handle_mparStruct);
            std::vector<double> bpar_struct_array = get_var_d(self->state.handle_bparStruct);

            // ask file_access_actor to write paramaters
            self->send(self->state.file_access_actor, write_param_v, 
                self->state.indxGRU, self->state.indxHRU, attr_struct_array,
                type_struct_array, mpar_struct_array, bpar_struct_array);
            self->send(self->state.file_access_actor, access_forcing_v, self->state.iFile, self);
            self->state.hru_timing.updateEndPoint("total_duration");
        },

        
        // The file_access_actor sends the HRU a message with 
        // the number of steps it can run based on the forcing data
        [=](run_hru, int stepsInCurrentFFile) {
            self->state.hru_timing.updateStartPoint("total_duration");

            bool keepRunning = true;
            int err = 0;
            self->state.stepsInCurrentFFile = stepsInCurrentFFile;
        
            while( keepRunning ) {

                err = Run_HRU(self); // Simulate a Timestep

                // Get Data from fortran
                // statistic structures
                std::vector<std::vector<double>> forc_stat_array    = get_var_dlength(self->state.handle_forcStat);
                std::vector<std::vector<double>> prog_stat_array    = get_var_dlength(self->state.handle_progStat);
                std::vector<std::vector<double>> diag_stat_array    = get_var_dlength(self->state.handle_diagStat);
                std::vector<std::vector<double>> flux_stat_array    = get_var_dlength(self->state.handle_fluxStat);
                std::vector<std::vector<double>> indx_stat_array    = get_var_dlength(self->state.handle_indxStat);
                std::vector<std::vector<double>> bvar_stat_array    = get_var_dlength(self->state.handle_bvarStat);
                // primary data structures (scalars)
                std::vector<int>      time_struct_array             = get_var_i(self->state.handle_timeStruct);
                std::vector<double>   forc_struct_array             = get_var_d(self->state.handle_forcStruct);
                std::vector<double>   attr_struct_array             = get_var_d(self->state.handle_attrStruct); 
                std::vector<int>      type_struct_array             = get_var_i(self->state.handle_typeStruct);
                std::vector<long int> id_struct_array               = get_var_i8(self->state.handle_idStruct);
                // primary data structures (variable length vectors)
                std::vector<std::vector<int>>    indx_struct_array  = get_var_ilength(self->state.handle_indxStruct);
                std::vector<std::vector<double>> mpar_struct_array  = get_var_dlength(self->state.handle_mparStruct);
                std::vector<std::vector<double>> prog_struct_array  = get_var_dlength(self->state.handle_progStruct);
                std::vector<std::vector<double>> diag_struct_array  = get_var_dlength(self->state.handle_diagStruct);
                std::vector<std::vector<double>> flux_struct_array  = get_var_dlength(self->state.handle_fluxStruct);
                // basin-average structures
                std::vector<double>              bpar_struct_array  = get_var_d(self->state.handle_bparStruct);
                std::vector<std::vector<double>> bvar_struct_array  = get_var_dlength(self->state.handle_bvarStruct);
                // ancillary data structures
                std::vector<double>   dpar_struct_array             = get_var_d(self->state.handle_dparStruct);
                std::vector<int>      finalize_stats_array          = get_flagVec(self->state.handle_finalizeStats);
                std::vector<int>      output_time_step_array        = get_var_i(self->state.handle_outputTimeStep);
                
                self->send(self->state.file_access_actor, write_output_v,
                    self->state.indxGRU,
                    self->state.indxHRU,
                    // statistic structures
                    forc_stat_array,
                    prog_stat_array,
                    diag_stat_array,
                    flux_stat_array,
                    indx_stat_array,
                    bvar_stat_array,
                    // primary data structures (scalars)
                    time_struct_array,
                    forc_struct_array,
                    attr_struct_array,
                    type_struct_array,
                    id_struct_array,
                    // primary data structures (variable length vectors)
                    indx_struct_array,
                    mpar_struct_array,
                    prog_struct_array,
                    diag_struct_array,
                    flux_struct_array,
                    // basin-average structures
                    bpar_struct_array,
                    bvar_struct_array,
                    // ancillary data structures
                    dpar_struct_array,
                    finalize_stats_array,
                    output_time_step_array);

                updateCounters(self->state.handle_timeStruct, self->state.handle_statCounter, self->state.handle_outputTimeStep,
                        self->state.handle_resetStats, self->state.handle_oldTime, self->state.handle_finalizeStats);

                // update Timings
                self->state.timestep += 1;
                self->state.outputStep += 1;
                self->state.forcingStep += 1;
                

                keepRunning = check_HRU(self, err); // check if we are done, need to write

            }

            self->send(self, done_write_v);
     
            self->state.hru_timing.updateEndPoint("total_duration");
        },

        [=](done_write) {
            self->state.hru_timing.updateStartPoint("total_duration");

            // We receive a done_write message so we ensure that
            // stepsInCurrentFFile remains unchanged
            if (self->state.timestep >= self->state.num_steps) {
                self->state.hru_timing.updateEndPoint("total_duration");

                // Tell our parent we are done, convert all timings to seconds
                aout(self) << "\n________________HRU TIMING INFO RESULTS________________\n";
                aout(self) << "Total Duration = " << self->state.hru_timing.getDuration("total_duration").value_or(-1.0) << " Seconds\n";
                aout(self) << "Init Duration = " << self->state.hru_timing.getDuration("init_duration").value_or(-1.0) << " Seconds\n";
                aout(self) << "Forcing Duration = " << self->state.hru_timing.getDuration("forcing_duration").value_or(-1.0) << " Seconds\n";
                aout(self) << "Run Physics Duration = " << self->state.hru_timing.getDuration("run_physics_duration").value_or(-1.0) << " Seconds\n";
                aout(self) << "Write Output Duration = " << self->state.hru_timing.getDuration("write_output_duration").value_or(-1.0) << " Seconds\n\n";

                self->send(self->state.parent, 
                    done_hru_v,
                    self->state.indxGRU, 
                    self->state.hru_timing.getDuration("total_duration").value_or(-1.0),
                    self->state.hru_timing.getDuration("init_duration").value_or(-1.0), 
                    self->state.hru_timing.getDuration("forcing_duration").value_or(-1.0), 
                    self->state.hru_timing.getDuration("run_physics_duration").value_or(-1.0), 
                    self->state.hru_timing.getDuration("write_output_duration").value_or(-1.0));
                
                deallocateHRUStructures(self);

                self->quit();
                return;
            }

            self->state.hru_timing.updateEndPoint("total_duration");
            self->send(self, run_hru_v, self->state.stepsInCurrentFFile);
        },

        [=](serialize_data) {

            self->send(self->state.file_access_actor, serialized_hru_data_v, self);


        },

        [=](dt_init_factor, int dt_init_factor) {
            aout(self) << "Recieved New dt_init_factor to attempt on next run \n";
            self->state.dt_init_factor = dt_init_factor;
        },
    };
}



void Initialize_HRU(stateful_actor<hru_state>* self) {
    self->state.hru_timing.updateStartPoint("init_duration");

    // Need to send a message to the file_access_actor for the data

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
            &self->state.upArea, &self->state.err);
    if (self->state.err != 0) {
        aout(self) << "Error: HRU_Actor - SetupHRUParam - HRU = " << self->state.indxHRU <<
        " - indxGRU = " << self->state.indxGRU << " - refGRU = " << self->state.refGRU << std::endl;
        self->quit();
        return;
    }
            
    Restart(&self->state.indxGRU, 
            &self->state.indxHRU, 
            self->state.handle_indxStruct, 
            self->state.handle_mparStruct, 
            self->state.handle_progStruct,
            self->state.handle_diagStruct, 
            self->state.handle_fluxStruct, 
            self->state.handle_bvarStruct, 
            &self->state.dt_init, &self->state.err);
    if (self->state.err != 0) {
        aout(self) << "Error: HRU_Actor - Restart - HRU = " << self->state.indxHRU <<
        " - indxGRU = " << self->state.indxGRU << " - refGRU = " << self->state.refGRU << std::endl;
        self->quit();
        return;
    }
            
    self->state.hru_timing.updateEndPoint("init_duration");
}

int Run_HRU(stateful_actor<hru_state>* self) {
    /**********************************************************************
    ** READ FORCING
    **********************************************************************/    
    self->state.hru_timing.updateStartPoint("forcing_duration");

    Forcing(&self->state.indxGRU,
        &self->state.timestep,
        self->state.handle_timeStruct,
        self->state.handle_forcStruct, 
        &self->state.iFile,
        &self->state.forcingStep,
        &self->state.fracJulDay,
        &self->state.tmZoneOffsetFracDay,
        &self->state.yearLength,
        &self->state.err);
    if (self->state.err != 0) { 
        aout(self) << "Error: Forcing - HRU = " << self->state.indxHRU <<
        " - indxGRU = " << self->state.indxGRU << " - refGRU = " << self->state.refGRU <<
        " - Timestep = " << self->state.timestep << std::endl;
        return 10;

    }
    self->state.hru_timing.updateEndPoint("forcing_duration");

    if (self->state.printOutput && 
        self->state.timestep % self->state.outputFrequency == 0) {
        printOutput(self);
    }
    

    /**********************************************************************
    ** RUN_PHYSICS    
    **********************************************************************/    
    self->state.hru_timing.updateStartPoint("run_physics_duration");

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
        aout(self) << "Error: RunPhysics - HRU = " << self->state.indxHRU << 
            " - indxGRU = " << self->state.indxGRU << " - refGRU = " << self->state.refGRU <<
            " - Timestep = " << self->state.timestep << std::endl;
        return 20;
    }
    self->state.hru_timing.updateEndPoint("run_physics_duration");

    self->state.hru_timing.updateStartPoint("write_output_duration");
    prepareOutput(&self->state.timestep,
        self->state.handle_forcStat,
        self->state.handle_progStat,
        self->state.handle_diagStat,
        self->state.handle_fluxStat,
        self->state.handle_indxStat,
        self->state.handle_bvarStat,
        self->state.handle_timeStruct,
        self->state.handle_forcStruct,
        self->state.handle_attrStruct,
        self->state.handle_typeStruct,
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
        &self->state.err);
    if (self->state.err != 0) {
        aout(self) << "Error: WriteOutput - HRU = " << self->state.indxHRU << 
            " - indxGRU = " << self->state.indxGRU << " - refGRU = " << self->state.refGRU <<
            " - Timestep = " << self->state.timestep << std::endl;
        return 30;
    }
    self->state.hru_timing.updateEndPoint("write_output_duration");

    return 0;      
}

bool check_HRU(stateful_actor<hru_state>* self, int err) {

    if (err != 0) { 
        // check for error
        
        self->send(self->state.parent, run_failure_v, self, self->state.indxGRU, err);
        self->quit();
        return false;
    
    } else if (self->state.timestep > self->state.num_steps) {
        // check if simulation is finished
        self->state.outputStep -= 1; // prevents segfault
        
        self->send(self->state.file_access_actor, write_output_v, 
            self->state.indxGRU, self->state.indxHRU, self->state.outputStep, self);

        self->state.hru_timing.updateEndPoint("total_duration");

        return false;

    } else if (self->state.outputStep > self->state.outputStrucSize && 
        self->state.forcingStep > self->state.stepsInCurrentFFile) {
        // Special case where we need both reading and writing
        self->state.outputStep -= 1; // prevents segfault

        self->send(self->state.file_access_actor, read_and_write_v, self->state.indxGRU, 
            self->state.indxHRU, self->state.outputStep, self->state.iFile + 1, self);
        self->state.outputStep = 1;

        return false; 

    } else if (self->state.outputStep > self->state.outputStrucSize) {
        // check if we need to clear the output struc
        self->state.outputStep -= 1;
        
        self->send(self->state.file_access_actor, write_output_v, 
            self->state.indxGRU, self->state.indxHRU, self->state.outputStep, self);
        self->state.outputStep = 1;

        return false;

    } else if (self->state.forcingStep > self->state.stepsInCurrentFFile) {
        // we need more forcing data
        self->send(self->state.file_access_actor, access_forcing_v, self->state.iFile + 1, self);

        return false;

    } else {
        return true;
    }
}

void deallocateHRUStructures(stateful_actor<hru_state>* self) {

    DeallocateStructures(self->state.handle_forcStat, 
        self->state.handle_progStat,
        self->state.handle_diagStat,
        self->state.handle_fluxStat,
        self->state.handle_indxStat,
        self->state.handle_bvarStat,
        self->state.handle_timeStruct,
        self->state.handle_forcStruct,
        self->state.handle_attrStruct,
        self->state.handle_typeStruct,
        self->state.handle_idStruct,
        self->state.handle_indxStruct,
        self->state.handle_mparStruct,
        self->state.handle_progStruct,
        self->state.handle_diagStruct,
        self->state.handle_fluxStruct,
        self->state.handle_bparStruct,
        self->state.handle_bvarStruct,
        self->state.handle_dparStruct,
        self->state.handle_startTime,
        self->state.handle_finshTime,
        self->state.handle_refTime,
        self->state.handle_oldTime,
        self->state.handle_ncid,
        self->state.handle_statCounter,
        self->state.handle_outputTimeStep,
        self->state.handle_resetStats,
        self->state.handle_finalizeStats,
        &self->state.err);
}

void printOutput(stateful_actor<hru_state>* self) {
        aout(self) << self->state.refGRU << " - Timestep = " << self->state.timestep << std::endl;
}

}