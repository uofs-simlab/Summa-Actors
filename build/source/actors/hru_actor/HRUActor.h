#ifndef HRUActor_H_
#define HRUActor_H_

#include "HRU.h"
using json = nlohmann::json;


/**
 * @brief HRU Actor is reponsible for carrying out the computation component of SUMMA
 * 
 * @param self The Actor Ref
 * @param refGRU The GRU we are computing in reference to the forcingFile
 * @param indxGRU The GRU we are computing's index in gru_struc
 * @param parent 
 * @return behavior 
 */
behavior hru_actor(stateful_actor<hru_state>* self, int refGRU, int indxGRU,
    std::string configPath,
    caf::actor file_access_actor, int outputStrucSize, caf::actor parent) {
    // Timing Information
    self->state.start = std::chrono::high_resolution_clock::now();
    self->state.duration            = 0.0;
    self->state.initDuration        = 0.0;

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
    parseSettings(self, configPath);
    // We only want to print this once
    if (indxGRU == 1) {
        aout(self) << "\nSETTINGS FOR HRU_ACTOR\n";
        aout(self) << "Print Output = " << self->state.printOutput << "\n";
        aout(self) << "Print Output every " << self->state.outputFrequency << " timesteps\n\n";
    }


    Initialize_HRU(self);

    self->state.end = std::chrono::high_resolution_clock::now();
    self->state.duration += calculateTime(self->state.start, self->state.end);

    self->send(self->state.parent, done_init_hru_v);

    return {
        // Starts the HRU and tells it to ask for data from the file_access_actor
        [=](start_hru) {
            self->state.start = std::chrono::high_resolution_clock::now();
            
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
            self->state.end = std::chrono::high_resolution_clock::now();
            self->state.duration += calculateTime(self->state.start, self->state.end);
        },

        [=](done_write) {
            self->state.start = std::chrono::high_resolution_clock::now();

            // We receive a done_write message so we ensure that
            // stepsInCurrentFFile remains unchanged
            if (self->state.timestep >= self->state.num_steps) {
                
                self->state.end = std::chrono::high_resolution_clock::now();
                self->state.duration += calculateTime(self->state.start, self->state.end);
                // Tell our parent we are done, convert all timings to seconds

                self->state.duration = self->state.duration / 1000; // Convert to milliseconds
                self->state.initDuration = self->state.initDuration / 1000; // Convert to milliseconds
                self->state.forcingDuration = self->state.forcingDuration / 1000; // Convert to milliseconds
                self->state.runPhysicsDuration = self->state.runPhysicsDuration / 1000; // Convert to milliseconds
                self->state.writeOutputDuration = self->state.writeOutputDuration / 1000; // Convert to milliseconds

                self->send(self->state.parent, 
                    done_hru_v,
                    self->state.indxGRU, 
                    self->state.duration / 1000,
                    self->state.initDuration / 1000, 
                    self->state.forcingDuration / 1000, 
                    self->state.runPhysicsDuration / 1000, 
                    self->state.writeOutputDuration / 1000);
                
                deallocateHRUStructures(self);

                self->quit();
                return;
            }

            self->state.end = std::chrono::high_resolution_clock::now();
            self->state.duration += calculateTime(self->state.start, self->state.end);
            
            self->send(self, run_hru_v, self->state.stepsInCurrentFFile);
        },

        [=](run_hru, int stepsInCurrentFFile) {
            self->state.start = std::chrono::high_resolution_clock::now();
            bool keepRunning = true;
            int err = 0;
            self->state.stepsInCurrentFFile = stepsInCurrentFFile;
        
            while( keepRunning ) {

                err = Run_HRU(self); // Simulate a Timestep

                // update Timings
                self->state.timestep += 1;
                self->state.outputStep += 1;
                self->state.forcingStep += 1;

                // if (self->state.timestep == 450 && self->state.indxGRU == 5) {
                //     err = 20;
                // }

                keepRunning = check_HRU(self, err); // check if we are done, need to write

            }
     
            self->state.end = std::chrono::high_resolution_clock::now();
            self->state.duration += calculateTime(self->state.start, self->state.end);

        },

        [=](dt_init_factor, int dt_init_factor) {
            aout(self) << "Recieved New dt_init_factor to attempt on next run \n";
            self->state.dt_init_factor = dt_init_factor;
        },
    };
    /*********************************************************************************************************
     *********************************** END ACTOR MESSAGE HANDLERS ******************************************
     *********************************************************************************************************/
}

void parseSettings(stateful_actor<hru_state>* self, std::string configPath) {
    json settings;
    std::string SummaActorsSettings = "/Summa_Actors_Settings.json";
    std::ifstream settings_file(configPath + SummaActorsSettings);
    settings_file >> settings;
    settings_file.close();

    if (settings.find("HRUActor") != settings.end()) {
        json HRUActorConfig = settings["HRUActor"];
        // find if we want to print output to stdout
        if (HRUActorConfig.find("printOutput") != HRUActorConfig.end()) {
            self->state.printOutput = HRUActorConfig["printOutput"];
        } else {
            aout(self) << "Error finding printOutput in JSON File - Reverting to default value\n";
            self->state.printOutput = true;
        }

        if (self->state.printOutput) {
            // get the frequency in number of timesteps we want to print the output
            if(HRUActorConfig.find("outputFrequency") != HRUActorConfig.end()) {
                self->state.outputFrequency = HRUActorConfig["outputFrequency"];
            } else {
                aout(self) << "Error finding outputFrequency in JSON File - Reverting to default value\n";
                self->state.outputFrequency = 10000;
            }
        }
    } else {
        aout(self) << "Error finding HRUActor in JSON File - Reverting to default values for HRUs\n";
        self->state.printOutput = true;
        self->state.outputFrequency = 10000;
    }


}

void Initialize_HRU(stateful_actor<hru_state>* self) {
    self->state.initStart = std::chrono::high_resolution_clock::now();
    // aout(self) << "Initalizing HRU" << std::endl;
    // aout(self) << "Entering Initalize \n"; 
    Initialize(&self->state.indxGRU,
            &self->state.num_steps, 
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
            self->state.handle_oldTime, &self->state.err);

    if (self->state.err != 0) {
        aout(self) << "Error: Initialize - HRU = " << self->state.indxHRU << 
        " - indxGRU = " << self->state.indxGRU << " - refGRU = "<< self->state.refGRU << std::endl;
        aout(self) << "Error = " << self->state.err << "\n";
        self->quit();
        return;
    }

    SetupParam(&self->state.indxGRU,
            &self->state.indxHRU, 
            self->state.handle_attrStruct, 
            self->state.handle_typeStruct, 
            self->state.handle_idStruct,
            self->state.handle_mparStruct, 
            self->state.handle_bparStruct, 
            self->state.handle_bvarStruct,
            self->state.handle_dparStruct, 
            self->state.handle_startTime, 
            self->state.handle_oldTime,
            &self->state.upArea, &self->state.err);
    if (self->state.err != 0) {
        aout(self) << "Error: SetupParam - HRU = " << self->state.indxHRU <<
        " - indxGRU = " << self->state.indxGRU << " - refGRU = " << self->state.refGRU << std::endl;
        self->quit();
        return;
    }
    // aout(self) << "Restart" << std::endl;
            
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
            
    // aout(self) << self->state.refGRU << " - Done Init" << std::endl;
    self->state.initEnd = std::chrono::high_resolution_clock::now();
    self->state.initDuration = calculateTime(self->state.initStart, self->state.initEnd);
}

int Run_HRU(stateful_actor<hru_state>* self) {
    /**********************************************************************
    ** READ FORCING
    **********************************************************************/    
    self->state.forcingStart = std::chrono::high_resolution_clock::now();
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
    self->state.forcingEnd = std::chrono::high_resolution_clock::now();
    self->state.forcingDuration += calculateTime(self->state.forcingStart, self->state.forcingEnd);


    if (self->state.printOutput && 
        self->state.timestep % self->state.outputFrequency == 0) {
        printOutput(self);
    }
    

    /**********************************************************************
    ** RUN_PHYSICS    
    **********************************************************************/    
    self->state.runPhysicsStart = std::chrono::high_resolution_clock::now();
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
    self->state.runPhysicsEnd = std::chrono::high_resolution_clock::now();
    self->state.runPhysicsDuration += calculateTime(self->state.runPhysicsStart, self->state.runPhysicsEnd);

    /**********************************************************************
    ** WRITE_OUTPUT  
    **********************************************************************/
    self->state.writeOutputStart = std::chrono::high_resolution_clock::now();
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
    self->state.writeOutputEnd = std::chrono::high_resolution_clock::now();
    self->state.writeOutputDuration += calculateTime(self->state.writeOutputStart, self->state.writeOutputEnd);

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

        if (debug)
            aout(self) << "Sending Final Write" << 
                "forcingStep = " << self->state.forcingStep << "\n" << 
                "stepsInCurrentFFile = " << self->state.stepsInCurrentFFile << "\n" <<
                "timeStep = " << self->state.timestep << "\n" << 
                "outputStep = " << self->state.outputStep << "\n";
        
        self->send(self->state.file_access_actor, write_output_v, 
            self->state.indxGRU, self->state.indxHRU, self->state.outputStep, self);

        self->state.end = std::chrono::high_resolution_clock::now();
        self->state.duration += calculateTime(self->state.start, self->state.end);

        return false;

    } else if (self->state.outputStep > self->state.outputStrucSize && 
        self->state.forcingStep > self->state.stepsInCurrentFFile) {
        // Special case where we need both reading and writing
        self->state.outputStep -= 1; // prevents segfault

        if (debug)
            aout(self) << "Need to read forcing and write to outputstruc\n" << 
                "forcingStep = " << self->state.forcingStep << "\n" << 
                "stepsInCurrentFFile = " << self->state.stepsInCurrentFFile << "\n" <<
                "timeStep = " << self->state.timestep << "\n" << 
                "outputStep = " << self->state.outputStep << "\n";
        

        self->send(self->state.file_access_actor, read_and_write_v, self->state.indxGRU, 
            self->state.indxHRU, self->state.outputStep, self->state.iFile + 1, self);
        self->state.outputStep = 1;

        return false; 

    } else if (self->state.outputStep > self->state.outputStrucSize) {
        // check if we need to clear the output struc
        self->state.outputStep -= 1;

        if (debug)
            aout(self) << "Sending Write \n" << 
                "forcingStep = " << self->state.forcingStep << "\n" << 
                "stepsInCurrentFFile = " << self->state.stepsInCurrentFFile << "\n" <<
                "timeStep = " << self->state.timestep << "\n" << 
                "outputStep = " << self->state.outputStep << "\n";
        
        
        self->send(self->state.file_access_actor, write_output_v, 
            self->state.indxGRU, self->state.indxHRU, self->state.outputStep, self);
        self->state.outputStep = 1;

        return false;

    } else if (self->state.forcingStep > self->state.stepsInCurrentFFile) {
        // we need more forcing data

        if (debug)
            aout(self) << "Asking for more forcing data\n" << 
                "forcingStep = " << self->state.forcingStep << "\n" << 
                "stepsInCurrentFFile = " << self->state.stepsInCurrentFFile << "\n" <<
                "timeStep = " << self->state.timestep << "\n" << 
                "outputStep = " << self->state.outputStep << "\n";
        
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
        aout(self) << self->state.refGRU << ":Accumulated Run Physics Time = " << 
        self->state.runPhysicsDuration << std::endl;
}
#endif