#include "caf/all.hpp"
#include "hru_actor.hpp"
#include "global.hpp"
#include "message_atoms.hpp"
#include "hru_actor_subroutine_wrappers.hpp"


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

    // Get the settings for the HRU
    // parseSettings(self, configPath);

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


    Initialize_HRU(self);
    self->state.hru_timing.updateEndPoint("total_duration");
    self->send(self, start_hru_v);

    return {
        // Starts the HRU and tells it to ask for data from the file_access_actor
        [=](start_hru) {
            self->state.hru_timing.updateStartPoint("total_duration");

            
            int err;
            
            err = 0;
            // Write Paramaters to OutputStruc
            Write_Param_C(&self->state.indxGRU, &self->state.indxHRU, 
                self->state.handle_attrStruct, self->state.handle_typeStruct,
                self->state.handle_mparStruct, self->state.handle_bparStruct, 
                &err);

            // ask file_access_actor to write paramaters
            self->send(self->state.file_access_actor, write_param_v, self->state.indxGRU, self->state.indxHRU);
            
            
            self->send(self->state.file_access_actor, access_forcing_v, self->state.iFile, self);
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

        [=](run_hru, int stepsInCurrentFFile) {
            self->state.hru_timing.updateStartPoint("total_duration");

            bool keepRunning = true;
            int err = 0;
            self->state.stepsInCurrentFFile = stepsInCurrentFFile;
        
            while( keepRunning ) {

                err = Run_HRU(self); // Simulate a Timestep

                // update Timings
                self->state.timestep += 1;
                self->state.outputStep += 1;
                self->state.forcingStep += 1;

                keepRunning = check_HRU(self, err); // check if we are done, need to write

            }
     
            self->state.hru_timing.updateEndPoint("total_duration");

        },

        [=](dt_init_factor, int dt_init_factor) {
            aout(self) << "Recieved New dt_init_factor to attempt on next run \n";
            self->state.dt_init_factor = dt_init_factor;
        },
    };
}



void Initialize_HRU(stateful_actor<hru_state>* self) {
    self->state.hru_timing.updateStartPoint("init_duration");
    
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
            &self->state.err);


    if (self->state.err != 0) {
        aout(self) << "Error: Initialize - HRU = " << self->state.indxHRU << 
        " - indxGRU = " << self->state.indxGRU << " - refGRU = "<< self->state.refGRU << std::endl;
        aout(self) << "Error = " << self->state.err << "\n";
        self->quit();
        return;
    }

    setupHRUParam(&self->state.indxGRU,
            &self->state.indxHRU, 
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
        aout(self) << "Error: SetupParam - HRU = " << self->state.indxHRU <<
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
        aout(self) << "Error: Restart - HRU = " << self->state.indxHRU <<
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
    WriteOutput(&self->state.indxHRU,
            &self->state.indxGRU,
            &self->state.timestep,
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
            &self->state.outputStep, 
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