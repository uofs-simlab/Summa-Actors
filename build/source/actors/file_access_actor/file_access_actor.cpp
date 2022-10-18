#include "caf/all.hpp"
#include "file_access_actor.hpp"
#include "output_manager.hpp"
#include "forcing_file_info.hpp"
#include "file_access_actor_subroutine_wrappers.hpp"
#include "fortran_data_types.hpp"
#include "message_atoms.hpp"
#include "global.hpp"
#include "json.hpp"

using json = nlohmann::json;

bool debug;


namespace caf {

behavior file_access_actor(stateful_actor<file_access_state>* self, int startGRU, int numGRU, 
    int outputStrucSize, std::string configPath, actor parent) {
    aout(self) << "\n----------File_Access_Actor Started----------\n";
    // Set Up timing Info we wish to track
    self->state.file_access_timing = TimingInfo();
    self->state.file_access_timing.addTimePoint("read_duration");
    self->state.file_access_timing.addTimePoint("write_duration");


    self->state.parent = parent;
    self->state.numGRU = numGRU;
    self->state.startGRU = startGRU;
    self->state.outputStrucSize = outputStrucSize;
    self->state.handle_forcing_file_info = new_handle_file_info();
    self->state.handle_ncid = new_handle_var_i();
    self->state.err = 0;

    // Get Settings from configuration file
    self->state.num_vectors_in_output_manager = getSettings(configPath, "FileAccessActor", "num_vectors_in_output_manager", 
		self->state.num_vectors_in_output_manager).value_or(1);
        
    initalizeFileAccessActor(self);

    return {
        [=](initalize_outputStructure) {
            aout(self) << "Initalizing Output Structure" << std::endl;
            Init_OutputStruct(self->state.handle_forcing_file_info, &self->state.outputStrucSize, 
                &self->state.numGRU, &self->state.err);
            if (self->state.err != 0) {
                aout(self) << "ERROR: FILE_ACCESS_ACTOR init_OutputStruct \n";
                std::string function = "init_OutputStruc";
                self->send(self->state.parent, file_access_actor_err_v, function);
                self->quit();
                return;
            }
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
                if(self->state.forcing_file_list[currentFile - 1].isFileLoaded()) { // C++ starts at 0 Fortran starts at 1
                    // aout(self) << "ForcingFile Already Loaded \n";
                    self->send(refToRespondTo, run_hru_v, 
                        self->state.forcing_file_list[currentFile - 1].getNumSteps());

                } else {
                    self->state.file_access_timing.updateStartPoint("read_duration");
                    
                    // Load the file
                    FileAccessActor_ReadForcing(self->state.handle_forcing_file_info, &currentFile,
                        &self->state.stepsInCurrentFile, &self->state.startGRU, 
                        &self->state.numGRU, &self->state.err);
                    if (self->state.err != 0) {
                        aout(self) << "ERROR: Reading Forcing" << std::endl;
                    }
                    self->state.filesLoaded += 1;
                    self->state.forcing_file_list[currentFile - 1].updateNumSteps(self->state.stepsInCurrentFile);

                    self->state.file_access_timing.updateEndPoint("read_duration");
                    // Check if we have loaded all forcing files
                    if(self->state.filesLoaded <= self->state.numFiles) {
                        self->send(self, access_forcing_internal_v, currentFile + 1);
                    }

                    self->send(refToRespondTo, run_hru_v, 
                        self->state.forcing_file_list[currentFile - 1].getNumSteps());
                }
            } else {
                aout(self) << currentFile << " is larger than expected" << std::endl;
            }
            
        },

        [=](access_forcing_internal, int currentFile) {
            if (self->state.filesLoaded <= self->state.numFiles &&
                currentFile <= self->state.numFiles) {
                // aout(self) << "Loading in background, File:" << currentFile << "\n";
                if (self->state.forcing_file_list[currentFile - 1].isFileLoaded()) {
                    aout(self) << "File Loaded when shouldn't be \n";
                }
                self->state.file_access_timing.updateStartPoint("read_duration");

                FileAccessActor_ReadForcing(self->state.handle_forcing_file_info, &currentFile,
                    &self->state.stepsInCurrentFile, &self->state.startGRU, 
                    &self->state.numGRU, &self->state.err);
                if (self->state.err != 0) {
                    aout(self) << "ERROR: Reading Forcing" << std::endl;
                }
                self->state.filesLoaded += 1;
                self->state.forcing_file_list[currentFile - 1].updateNumSteps(self->state.stepsInCurrentFile);
                
                self->state.file_access_timing.updateEndPoint("read_duration");

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

        [=](run_failure, int indxGRU) {
            int listIndex;

            // update the list in Fortran
            updateFailed(&indxGRU);

            listIndex = self->state.output_manager->decrementMaxSize(indxGRU);
          
            // Check if this list is now full
            if(self->state.output_manager->isFull(listIndex)) {
                write(self, listIndex);
            }
        },

        /**
         * Message from JobActor
         * OutputManager needs to be adjusted so the failed HRUs can run again
         */
        [=](restart_failures) {
            resetFailedArray();
            self->state.output_manager->restartFailures();
        },

        [=](deallocate_structures) {
            aout(self) << "Deallocating Structure" << std::endl;
            FileAccessActor_DeallocateStructures(self->state.handle_forcing_file_info, self->state.handle_ncid);
            aout(self) << "\n________________FILE_ACCESS_ACTOR TIMING INFO RESULTS________________\n";
            aout(self) << "Total Read Duration = " << self->state.file_access_timing.getDuration("read_duration").value_or(-1.0) << " Seconds\n";
            aout(self) << "Total Write Duration = " << self->state.file_access_timing.getDuration("write_duration").value_or(-1.0) << " Seconds\n";
            
            self->send(self->state.parent, 
                file_access_actor_done_v, 
                self->state.file_access_timing.getDuration("read_duration").value_or(-1.0), 
                self->state.file_access_timing.getDuration("write_duration").value_or(-1.0));
            self->quit();
        },

        [=](reset_outputCounter, int indxGRU) {
            resetOutputCounter(&indxGRU);
        },

        [=](serialized_hru_data, 
            // Statistic Structures
            std::vector<std::vector<double>> forcStat,
            std::vector<std::vector<double>> progStat,
            std::vector<std::vector<double>> diagStat,
            std::vector<std::vector<double>> fluxStat,
            std::vector<std::vector<double>> indxStat,
            std::vector<std::vector<double>> bvarStat,
            // primary data structures (scalars)
            std::vector<int> timeStruct,
            std::vector<double> forcStruct,
            std::vector<double> attrStruct,
            std::vector<int> typestruct,
            std::vector<long int> idStruct,
            // primary data structures (variable length vectors)
            std::vector<std::vector<int>> indxStruct,
            std::vector<std::vector<double>> mparStruct,
            std::vector<std::vector<double>> progStruct,
            std::vector<std::vector<double>> diagStruct,
            std::vector<std::vector<double>> fluxStruct,
            // basin-average structures
            std::vector<double>              bparSturct,
            std::vector<std::vector<double>> bvarStruct,
            // ancillary data structures
            std::vector<double> dparStruct) {
            aout(self) << "Receieved HRU Data\n";

            








        },

    };
}


void initalizeFileAccessActor(stateful_actor<file_access_state>* self) {
    int indx = 1;
    int err = 0;

    ffile_info_C(&indx, self->state.handle_forcing_file_info, &self->state.numFiles, &err);
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

    initFailedHRUTracker(&self->state.numGRU);

    def_output(self->state.handle_ncid, &self->state.startGRU, &self->state.numGRU, &self->state.numGRU, &err);
    if (err != 0) {
        aout(self) << "ERROR: Create_OutputFile\n";
        std::string function = "def_output";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }

    // Initalize the output Structure
    aout(self) << "Initalizing Output Structure" << std::endl;
    Init_OutputStruct(self->state.handle_forcing_file_info, &self->state.outputStrucSize, 
        &self->state.numGRU, &self->state.err);

    // Read In all of the attribres for the number of GRUs in the run Domian
    readAttributeFileAccessActor(&self->state.numGRU, &err);
    if (err != 0) {
        aout(self) << "ERROR: FILE_ACCESS_ACTOR readAttributeFilAccessActor() \n";
        std::string function = "readAttributeFileAccessActor";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }

    overwriteParam(&self->state.numGRU, &err);
    if (err != 0) {
        aout(self) << "ERROR: FILE_ACCESS_ACTOR overwriteParam() \n";
        std::string function = "overwriteParam";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }


    // Read in all of the parmeters for the number of GRUs in the run Domain
    readParamFileAccessActor(&self->state.startGRU, &self->state.numGRU, &err);
    if (err != 0) {
        aout(self) <<  "ERROR: FILE_ACCESS_ACTOR readParamFileAccessActor() \n";
        std::string function = "readParamFileAccessActor";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }

    // Initalize the output manager  
    self->state.output_manager = new OutputManager(self->state.num_vectors_in_output_manager, self->state.numGRU);
    
    self->send(self->state.parent, done_file_access_actor_init_v);
    // initalize the forcingFile array
    self->state.filesLoaded = 0;
    for (int i = 1; i <= self->state.numFiles; i++) {
        self->state.forcing_file_list.push_back(Forcing_File_Info(i));
    }
}

int write(stateful_actor<file_access_state>* self, int listIndex) {
    int err = 0;
    int minGRU = self->state.output_manager->getMinIndex(listIndex);
    int maxGRU = self->state.output_manager->getMaxIndex(listIndex);
    int numStepsToWrite = self->state.output_manager->getNumStepsToWrite(listIndex);
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
                self->state.forcing_file_list[get<1>(actor) - 1].getNumSteps());
        }
    }

    return 0;
}

int writeOutput(stateful_actor<file_access_state>* self, int indxGRU, int indxHRU, 
    int numStepsToWrite, int returnMessage, caf::actor actorRef) {
    self->state.file_access_timing.updateStartPoint("write_duration");

    if (debug) {
        aout(self) << "Recieved Write Request From GRU: " << indxGRU << "\n";
    }
    int err = 0;
    int listIndex = self->state.output_manager->addActor(actorRef, indxGRU, returnMessage, numStepsToWrite);
    if (self->state.output_manager->isFull(listIndex)) {
        if (debug) {
            aout(self) << "List with Index " << listIndex << " is full and ready to write\n";
            aout(self) << "Minimum GRU Index = " << self->state.output_manager->getMinIndex(listIndex) << "\n";
            aout(self) << "Maximum GRU Index = " << self->state.output_manager->getMaxIndex(listIndex) << "\n";
        }

       err = write(self, listIndex);

    } else {
        if (debug) {
            aout(self) << "List with Index " << listIndex << " is not full yet waiting to write\n";
            aout(self) << "Size of list is " << self->state.output_manager->getSize(listIndex) << "\n";
        }
    }
   
    self->state.file_access_timing.updateEndPoint("write_duration");
    return err;

}

int readForcing(stateful_actor<file_access_state>* self, int currentFile) {
    // Check if we have already loaded this file
    if(self->state.forcing_file_list[currentFile -1].isFileLoaded()) {
        if (debug)
            aout(self) << "ForcingFile Already Loaded \n";
        return 0;
    
    } else { // File Needs to be loaded

        self->state.file_access_timing.updateStartPoint("read_duration");

        // Load the file
        FileAccessActor_ReadForcing(self->state.handle_forcing_file_info, &currentFile,
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
            self->state.forcing_file_list[currentFile - 1].updateNumSteps(self->state.stepsInCurrentFile);
            self->state.file_access_timing.updateEndPoint("read_duration");
            return 0;
        }
    }

}

} // end namespace