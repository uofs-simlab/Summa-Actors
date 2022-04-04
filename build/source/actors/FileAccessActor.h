#ifndef FILEACCESSACTOR_H_
#define FILEACCESSACTOR_H_

#include "FileAccess.h"

using namespace caf;
void initalizeFileAccessActor(stateful_actor<file_access_state>* self);
int writeOutput(stateful_actor<file_access_state>* self, int indxGRU, int indxHRU, int numStepsToWrite);
int readForcing(stateful_actor<file_access_state>* self, int currentFile);


behavior file_access_actor(stateful_actor<file_access_state>* self, int startGRU, int numGRU, 
    int outputStrucSize, actor parent) {
    // Set File_Access_Actor variables
    self->state.parent = parent;
    self->state.numGRU = numGRU;
    self->state.startGRU = startGRU;
    self->state.outputStrucSize = outputStrucSize;
    initalizeFileAccessActor(self);

    return {
        [=](initalize_outputStrucure) {
            Init_OutputStruct(self->state.handle_forcFileInfo, &self->state.outputStrucSize, 
                &self->state.numGRU, &self->state.err);
        },

        [=](access_forcing, int currentFile, caf::actor refToRespondTo) {
            int err;
            // Check if this file is greater than what we expect
            if (currentFile > self->state.numFiles) {
                aout(self) << "\nERROR: FILE_ACCESS_ACTOR - Current File is Larger than the Number of Files\n";
            } else {

                err = readForcing(self, currentFile);
                if (err != 0) 
                    aout(self) << "\nERROR: FILE_ACCESS_ACTOR - READING_FORCING FAILED\n";
                
            }

            // Check if we have loaded all forcing files, if no read more data
            if(self->state.filesLoaded != self->state.numFiles) {
                self->send(self, access_forcing_internal_v, currentFile + 1);
            }

            // Respond to HRU
            self->send(refToRespondTo, run_hru_v, 
                self->state.forcFileList[currentFile - 1].getNumSteps());

        },

        [=](access_forcing_internal, int currentFile) {

            if (self->state.filesLoaded <= self->state.numFiles &&
                currentFile <= self->state.numFiles) {
                
                readForcing(self, currentFile);
                
                self->send(self, access_forcing_internal_v, currentFile + 1);
            } else {
                if (debug) 
                    aout(self) << "All Forcing Files Loaded \n";
            }

            
        },

        [=](write_output, int indxGRU, int indxHRU, int numStepsToWrite,
            caf::actor refToRespondTo) {
            int err;
            
            err = writeOutput(self, indxGRU, indxHRU, numStepsToWrite);
            if (err != 0) {
                aout(self) << "FILE_ACCESS_ACTOR - ERROR Writing Output \n";
            } else {
                self->send(refToRespondTo, done_write_v);
            }


        },

        [=](read_and_write, int indxGRU, int indxHRU, int numStepsToWrite, int currentFile, 
            caf::actor refToRespondTo) {
            int err;

            err = writeOutput(self, indxGRU, indxHRU, numStepsToWrite);
            if (err != 0)
                aout(self) << "FILE_ACCESS_ACTOR - ERROR Writing Output \n";

            err = readForcing(self, currentFile);
            if (err != 0)
                aout(self) << "\nERROR: FILE_ACCESS_ACTOR - READING_FORCING FAILED\n";

            // Respond to HRU
            self->send(refToRespondTo, run_hru_v, 
                self->state.forcFileList[currentFile - 1].getNumSteps());
        },


        [=](deallocate_structures) {
            aout(self) << "Deallocating Structure" << std::endl;
            FileAccessActor_DeallocateStructures(self->state.handle_forcFileInfo, self->state.handle_ncid);
            
            self->state.readDuration = self->state.readDuration / 1000;
            self->state.writeDuration = self->state.writeDuration / 1000;
            
            self->send(self->state.parent, file_access_actor_done_v, self->state.readDuration, 
                self->state.writeDuration);
            self->quit();
        },

        [=](reset_outputCounter, int indxGRU) {
            resetOutputCounter(&indxGRU);
        }

    };
}

void initalizeFileAccessActor(stateful_actor<file_access_state>* self) {
    int indx = 1;
    int err = 0;
    // aout(self) << "Set Up the forcing file" << std::endl;
    ffile_info_C(&indx, self->state.handle_forcFileInfo, &self->state.numFiles, &err);
    if (err != 0) {
        aout(self) << "Error: ffile_info_C - HRU = " << indx <<
        " - indxGRU = " << indx << " - refGRU = " << std::endl;
        self->quit();
    }

    mDecisions_C(&self->state.num_steps, &err);
    if (err != 0) {
        aout(self) << "Error: mDecisions - FileAccess Actor " << std::endl;
        self->quit();
    }

    aout(self) << "\n\nNumber of timesteps for the simulation = " << self->state.num_steps << "\n";

    read_pinit_C(&err);
    
    read_vegitationTables(&err);
    
    
    // initalize vector for knowing if HRU output has init'd
    for(int i = 0; i < self->state.numGRU; i++) {
        self->state.outputFileInitHRU.push_back(false);
    }

    self->send(self->state.parent, done_file_access_actor_init_v);
    // initalize the forcingFile array
    self->state.filesLoaded = 0;
    for (int i = 1; i <= self->state.numFiles; i++) {
        self->state.forcFileList.push_back(forcingFile(i));
    }
}

int writeOutput(stateful_actor<file_access_state>* self, int indxGRU, int indxHRU, 
    int numStepsToWrite) {

    int err = 0;
    bool hruInit = self->state.outputFileInitHRU[indxGRU - 1];
    self->state.writeStart = std::chrono::high_resolution_clock::now();
    FileAccessActor_WriteOutput(self->state.handle_ncid, &self->state.outputFileExists, 
        &numStepsToWrite, &self->state.startGRU, &self->state.numGRU, 
        &hruInit, &indxGRU, &indxHRU, &err);
    self->state.outputFileInitHRU[indxGRU - 1] = true;
    self->state.writeEnd = std::chrono::high_resolution_clock::now();
    self->state.writeDuration += calculateTime(self->state.writeStart, self->state.writeEnd);

    return err;

}

int readForcing(stateful_actor<file_access_state>* self, int currentFile) {
    // Check if we have already loaded this file
    if(self->state.forcFileList[currentFile -1].isFileLoaded()) {
        if (debug)
            aout(self) << "ForcingFile Already Loaded \n";
        return 0;
    
    } else {
        
        // File Needs to be loaded
        self->state.readStart = std::chrono::high_resolution_clock::now();
                    
        // Load the file
        FileAccessActor_ReadForcing(self->state.handle_forcFileInfo, &currentFile,
            &self->state.stepsInCurrentFile, &self->state.startGRU, 
            &self->state.numGRU, &self->state.err);
        
        if (self->state.err != 0) {
            if (debug)
                aout(self) << "ERROR: FileAccessActor_ReadForcing\n" << 
                "currentFile = " << currentFile << "\n" << "number of steps = " 
                << self->state.stepsInCurrentFile << "\n";
            return -1;
        } else {
            self->state.filesLoaded += 1;
            self->state.forcFileList[currentFile - 1].updateNumSteps(self->state.stepsInCurrentFile);

            self->state.readEnd = std::chrono::high_resolution_clock::now();
            self->state.readDuration += calculateTime(self->state.readStart, self->state.readEnd);
            return 0;
        }
    }

}
#endif