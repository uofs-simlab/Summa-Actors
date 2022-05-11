#ifndef FILEACCESSACTOR_H_
#define FILEACCESSACTOR_H_

#include "FileAccess.h"

using namespace caf;
void initalizeFileAccessActor(stateful_actor<file_access_state>* self);
int writeOutput(stateful_actor<file_access_state>* self, int indxGRU, int indxHRU, int numStepsToWrite, int returnMessage, caf::actor actorRef);
int readForcing(stateful_actor<file_access_state>* self, int currentFile);

behavior file_access_actor(stateful_actor<file_access_state>* self, int startGRU, int numGRU, 
    int outputStrucSize, actor parent) {
    // Set File_Access_Actor variables
    self->state.parent = parent;
    self->state.numGRU = numGRU;
    self->state.startGRU = startGRU;
    self->state.outputStrucSize = outputStrucSize;

    aout(self) << "\nFile Access Actor Started\n";
    initalizeFileAccessActor(self);

    return {
        [=](initalize_outputStructure) {
            aout(self) << "Initalizing Output Structure" << std::endl;
            Init_OutputStruct(self->state.handle_forcFileInfo, &self->state.outputStrucSize, 
                &self->state.numGRU, &self->state.err);
        },

        [=](write_param, int indxGRU, int indxHRU) {
            int err;
            err = 0;
            Write_HRU_Param(self->state.handle_ncid, &indxGRU, &indxHRU, &err);
            if (err != 0) {
                aout(self) << "ERROR: Write_HRU_PARAM -- For HRU = " << indxHRU << "\n"; 
            }
        },

        [=](access_forcing, int currentFile, caf::actor refToRespondTo) {
            // aout(self) << "Received Current FIle = " << currentFile << std::endl;
            if (currentFile <= self->state.numFiles) {
                if(self->state.forcFileList[currentFile - 1].isFileLoaded()) { // C++ starts at 0 Fortran starts at 1
                    // aout(self) << "ForcingFile Already Loaded \n";
                    self->send(refToRespondTo, run_hru_v, 
                        self->state.forcFileList[currentFile - 1].getNumSteps());

                } else {
                    self->state.readStart = std::chrono::high_resolution_clock::now();
                    
                    // Load the file
                    FileAccessActor_ReadForcing(self->state.handle_forcFileInfo, &currentFile,
                        &self->state.stepsInCurrentFile, &self->state.startGRU, 
                        &self->state.numGRU, &self->state.err);
                    if (self->state.err != 0) {
                        aout(self) << "ERROR: Reading Forcing" << std::endl;
                    }
                    self->state.filesLoaded += 1;
                    self->state.forcFileList[currentFile - 1].updateNumSteps(self->state.stepsInCurrentFile);

                    self->state.readEnd = std::chrono::high_resolution_clock::now();
                    self->state.readDuration += calculateTime(self->state.readStart, self->state.readEnd);
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
                self->state.readStart = std::chrono::high_resolution_clock::now();
                FileAccessActor_ReadForcing(self->state.handle_forcFileInfo, &currentFile,
                    &self->state.stepsInCurrentFile, &self->state.startGRU, 
                    &self->state.numGRU, &self->state.err);
                if (self->state.err != 0) {
                    aout(self) << "ERROR: Reading Forcing" << std::endl;
                }
                self->state.filesLoaded += 1;
                self->state.forcFileList[currentFile - 1].updateNumSteps(self->state.stepsInCurrentFile);
                
                self->state.readEnd = std::chrono::high_resolution_clock::now();
                self->state.readDuration += calculateTime(self->state.readStart, self->state.readEnd);
                
                self->send(self, access_forcing_internal_v, currentFile + 1);
            } else {
                aout(self) << "All Forcing Files Loaded \n";
            }
        },


        [=](write_output, int indxGRU, int indxHRU, int numStepsToWrite,
            caf::actor refToRespondTo) {
            int err;
            int returnMessage = 9999;
            
            err = writeOutput(self, indxGRU, indxHRU, numStepsToWrite, returnMessage, refToRespondTo);
            if (err != 0) {
                aout(self) << "FILE_ACCESS_ACTOR - ERROR Writing Output \n";
            } 


        },

        [=](read_and_write, int indxGRU, int indxHRU, int numStepsToWrite, int currentFile, 
            caf::actor refToRespondTo) {
            int err;

            err = readForcing(self, currentFile);
            if (err != 0)
                aout(self) << "\nERROR: FILE_ACCESS_ACTOR - READING_FORCING FAILED\n";

            err = writeOutput(self, indxGRU, indxHRU, numStepsToWrite, currentFile, refToRespondTo);
            if (err != 0)
                aout(self) << "FILE_ACCESS_ACTOR - ERROR Writing Output \n";
        },

        [=](deallocate_structures) {
            aout(self) << "Deallocating Structure" << std::endl;
            FileAccessActor_DeallocateStructures(self->state.handle_forcFileInfo, self->state.handle_ncid);
            
            self->state.readDuration = self->state.readDuration / 1000; // Convert to milliseconds
            self->state.readDuration = self->state.readDuration / 1000; // Convert to seconds

            self->state.writeDuration = self->state.writeDuration / 1000; // Convert to milliseconds
            self->state.writeDuration = self->state.writeDuration / 1000; // Convert to milliseconds
            
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
        aout(self) << "Error: ffile_info_C - File_Access_Actor \n";
        std::string function = "ffile_info_C";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }

    mDecisions_C(&self->state.num_steps, &err);
    if (err != 0) {
        aout(self) << "Error: mDecisions - FileAccess Actor \n";
        std::string function = "mDecisions_C";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }

    read_pinit_C(&err);
    if (err != 0) {
        aout(self) << "ERROR: read_pinit_C\n";
        std::string function = "read_pinit_C";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }
    
    read_vegitationTables(&err);
    if (err != 0) {
        aout(self) << "ERROR: read_vegitationTables\n";
        std::string function = "read_vegitationTables";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }

    Create_Output_File(self->state.handle_ncid, &self->state.numGRU, &self->state.startGRU, &err);
    if (err != 0) {
        aout(self) << "ERROR: Create_OutputFile\n";
        std::string function = "Create_Output_File";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }

    // Initalize the output Structure
    aout(self) << "Initalizing Output Structure" << std::endl;
    Init_OutputStruct(self->state.handle_forcFileInfo, &self->state.outputStrucSize, 
        &self->state.numGRU, &self->state.err);

    // Initalize the output manager  
    self->state.output_manager = new OutputManager(self->state.num_vectors_in_output_manager, self->state.numGRU);
    
    self->send(self->state.parent, done_file_access_actor_init_v);
    // initalize the forcingFile array
    self->state.filesLoaded = 0;
    for (int i = 1; i <= self->state.numFiles; i++) {
        self->state.forcFileList.push_back(forcingFile(i));
    }
}

int writeOutput(stateful_actor<file_access_state>* self, int indxGRU, int indxHRU, 
    int numStepsToWrite, int returnMessage, caf::actor actorRef) {
    self->state.writeStart = std::chrono::high_resolution_clock::now();
    if (debug) {
        aout(self) << "Recieved Write Request From GRU: " << indxGRU << "\n";
    }

    
    int err = 0;
    int listIndex = self->state.output_manager->addActor(actorRef, indxGRU, returnMessage);
    if (self->state.output_manager->isFull(listIndex)) {
        if (debug) {
            aout(self) << "List with Index " << listIndex << " is full and ready to write\n";
            aout(self) << "Minimum GRU Index = " << self->state.output_manager->getMinIndex(listIndex) << "\n";
            aout(self) << "Maximum GRU Index = " << self->state.output_manager->getMaxIndex(listIndex) << "\n";
        }

        int minGRU = self->state.output_manager->getMinIndex(listIndex);
        int maxGRU = self->state.output_manager->getMaxIndex(listIndex);
        FileAccessActor_WriteOutput(self->state.handle_ncid,
            &numStepsToWrite, &minGRU, 
            &maxGRU, &err);
        
        // Pop The actors and send them the correct continue message
        while(!self->state.output_manager->isEmpty(listIndex)) {
            std::tuple<caf::actor, int> actor = self->state.output_manager->popActor(listIndex);
            if (get<1>(actor) == 9999) {
                
                self->send(get<0>(actor), done_write_v);

            }  else {
                self->send(get<0>(actor), run_hru_v, 
                    self->state.forcFileList[get<1>(actor) - 1].getNumSteps());
            }
        }
        

    } else {
        if (debug) {
            aout(self) << "List with Index " << listIndex << " is not full yet waiting to write\n";
            aout(self) << "Size of list is " << self->state.output_manager->getSize(listIndex) << "\n";
        }
    }
    
   



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