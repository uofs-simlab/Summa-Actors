#ifndef FILEACCESSACTOR_H_
#define FILEACCESSACTOR_H_

#include "FileAccess.h"

using namespace caf;
void initalizeFileAccessActor(stateful_actor<file_access_state>* self);

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
            // aout(self) << "Received Current FIle = " << currentFile << std::endl;
            if (currentFile <= self->state.numFiles) {
                if(self->state.forcFileList[currentFile - 1].isFileLoaded()) { // C++ starts at 0 Fortran starts at 1
                    // aout(self) << "ForcingFile Already Loaded \n";
                    self->send(refToRespondTo, run_hru_v, 
                        self->state.forcFileList[currentFile - 1].getNumSteps());

                } else {
                    // Load the file
                    FileAccessActor_ReadForcing(self->state.handle_forcFileInfo, &currentFile,
                        &self->state.stepsInCurrentFile, &self->state.startGRU, 
                        &self->state.numGRU, &self->state.err);
                    if (self->state.err != 0) {
                        aout(self) << "ERROR: Reading Forcing" << std::endl;
                    }
                    self->state.filesLoaded += 1;
                    self->state.forcFileList[currentFile - 1].updateNumSteps(self->state.stepsInCurrentFile);

                    // Check if we have loaded all forcing files
                    if(self->state.filesLoaded <= self->state.numFiles) {
                        self->send(self, access_forcing_internal_v, currentFile + 1);
                    }

                    self->send(refToRespondTo, run_hru_v, 
                        self->state.forcFileList[currentFile - 1].getNumSteps());
                }
            } else {
                aout(self) << currentFile << "is larger than expected" << std::endl;
            }
            
        },

        [=](access_forcing_internal, int currentFile) {
            if (self->state.filesLoaded <= self->state.numFiles &&
                currentFile <= self->state.numFiles) {
                // aout(self) << "Loading in background, File:" << currentFile << "\n";
                if (self->state.forcFileList[currentFile - 1].isFileLoaded()) {
                    aout(self) << "File Loaded when shouldn't be \n";
                }
                FileAccessActor_ReadForcing(self->state.handle_forcFileInfo, &currentFile,
                    &self->state.stepsInCurrentFile, &self->state.startGRU, 
                    &self->state.numGRU, &self->state.err);
                if (self->state.err != 0) {
                    aout(self) << "ERROR: Reading Forcing" << std::endl;
                }
                self->state.filesLoaded += 1;
                self->state.forcFileList[currentFile - 1].updateNumSteps(self->state.stepsInCurrentFile);
                self->send(self, access_forcing_internal_v, currentFile + 1);
            } else {
                aout(self) << "All Forcing Files Loaded \n";
            }

            
        },

        [=](write_output, int indxGRU, int indxHRU, int numStepsToWrite,
            caf::actor refToRespondTo) {
            int err = 0;
            bool hruInit = self->state.outputFileInitHRU[indxGRU - 1];
            FileAccessActor_WriteOutput(self->state.handle_ncid, &self->state.outputFileExists, 
                &numStepsToWrite, &self->state.startGRU, &self->state.numGRU, 
                &hruInit, &indxGRU, &indxHRU, &err);
            if (err != 0) {
                aout(self) << "ERROR: Writing Output" << std::endl;
            }
            self->state.outputFileInitHRU[indxGRU - 1] = true; //
            
            
            self->send(refToRespondTo, done_write_v);
        },


        [=](deallocate_structures) {
            aout(self) << "Deallocating Structure" << std::endl;
            FileAccessActor_DeallocateStructures(self->state.handle_forcFileInfo, self->state.handle_ncid);
            self->send(self->state.parent, deallocate_structures_v);
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



#endif