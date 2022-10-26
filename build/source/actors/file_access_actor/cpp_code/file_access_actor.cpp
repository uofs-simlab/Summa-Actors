#include "caf/all.hpp"
#include "file_access_actor.hpp"
#include "output_manager.hpp"
#include "forcing_file_info.hpp"
#include "file_access_actor_subroutine_wrappers.hpp"
#include "fortran_data_types.hpp"
#include "message_atoms.hpp"
#include "global.hpp"
#include "json.hpp"
#include "auxilary.hpp"

using json = nlohmann::json;

bool debug;


namespace caf {

behavior file_access_actor(stateful_actor<file_access_state>* self, int start_gru, int num_gru, 
    int outputStrucSize, std::string configPath, actor parent) {
    aout(self) << "\n----------File_Access_Actor Started----------\n";
    // Set Up timing Info we wish to track
    self->state.file_access_timing = TimingInfo();
    self->state.file_access_timing.addTimePoint("read_duration");
    self->state.file_access_timing.addTimePoint("write_duration");


    self->state.parent = parent;
    self->state.num_gru = num_gru;
    self->state.start_gru = start_gru;
    self->state.outputStrucSize = outputStrucSize;
    self->state.handle_forcing_file_info = new_handle_file_info();
    self->state.handle_ncid = new_handle_var_i();
    self->state.err = 0;

    // Get Settings from configuration file
    self->state.num_vectors_in_output_manager = getSettings(configPath, "FileAccessActor", "num_vectors_in_output_manager", 
		self->state.num_vectors_in_output_manager).value_or(1);
        
    initalizeFileAccessActor(self);

    return {
        [=](write_param, int index_gru, int index_hru, std::vector<double> attr_struct, 
            std::vector<int> type_struct, std::vector<std::vector<double>> mpar_struct,
            std::vector<double> bpar_struct) {
            int err = 0;

            self->state.file_access_timing.updateStartPoint("write_duration");

            // create structures to populate in Fortran
            void *handle_attr_struct = new_handle_var_d();
            void *handle_type_struct = new_handle_var_i();
            void *handle_mpar_struct = new_handle_var_dlength();
            void *handle_bpar_struct = new_handle_var_d(); 
            // populate the newly created Fortran structures
            set_var_d(attr_struct, handle_attr_struct);
            set_var_i(type_struct, handle_type_struct);
            set_var_dlength(mpar_struct, handle_mpar_struct);
            set_var_d(bpar_struct, handle_bpar_struct);
            // write the populated data to netCDF
            writeParamToNetCDF(self->state.handle_ncid, &index_gru, &index_hru, 
                handle_attr_struct, handle_type_struct, handle_mpar_struct, 
                handle_bpar_struct, &err);

            self->state.file_access_timing.updateEndPoint("write_duration");
            
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
                        &self->state.stepsInCurrentFile, &self->state.start_gru, 
                        &self->state.num_gru, &self->state.err);
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
                    &self->state.stepsInCurrentFile, &self->state.start_gru, 
                    &self->state.num_gru, &self->state.err);
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

        [=](write_output, int index_gru, int index_hru, 
            // statistic structures
            std::vector<std::vector<double>> forc_stat, std::vector<std::vector<double>> prog_stat, std::vector<std::vector<double>> diag_stat,
            std::vector<std::vector<double>> flux_stat, std::vector<std::vector<double>> indx_stat, std::vector<std::vector<double>> bvar_stat,
            // primary data structures (scalars)
            std::vector<int> time_struct, std::vector<double> forc_struct, std::vector<double> attr_struct,
            std::vector<int> type_struct, std::vector<long int> id_struct,  
            // primary data structures (variable length vectors)
            std::vector<std::vector<int>> indx_struct, std::vector<std::vector<double>> mpar_struct, std::vector<std::vector<double>> prog_struct,
            std::vector<std::vector<double>> diag_struct, std::vector<std::vector<double>> flux_struct,
             // basin-average structures
            std::vector<double> bpar_struct, std::vector<std::vector<double>> bvar_struct,
            // ancillary data structures
            std::vector<double> dpar_struct, std::vector<int> finalize_stats, std::vector<int> output_timestep ) {
            
            self->state.file_access_timing.updateStartPoint("write_duration");
            
            int err = 0;
            // statistic structures
            void* handle_forc_stat        = new_handle_var_dlength();
            void* handle_prog_stat        = new_handle_var_dlength();
            void* handle_diag_stat        = new_handle_var_dlength();
            void* handle_flux_stat        = new_handle_var_dlength();
            void* handle_indx_stat        = new_handle_var_dlength();
            void* handle_bvar_stat        = new_handle_var_dlength();
            set_var_dlength(forc_stat, handle_forc_stat);
            set_var_dlength(prog_stat, handle_prog_stat);
            set_var_dlength(diag_stat, handle_diag_stat);
            set_var_dlength(flux_stat, handle_flux_stat);
            set_var_dlength(indx_stat, handle_indx_stat);
            set_var_dlength(bvar_stat, handle_bvar_stat);
            // primary data structures (scalars)
            void* handle_time_struct      = new_handle_var_i();
            void* handle_forc_struct      = new_handle_var_d();
            void* handle_attr_struct      = new_handle_var_d();
            void* handle_type_struct      = new_handle_var_i();
            void* handle_id_struct        = new_handle_var_i8();
            set_var_i(time_struct, handle_time_struct);
            set_var_d(forc_struct, handle_forc_struct);
            set_var_d(attr_struct, handle_attr_struct);
            set_var_i(type_struct, handle_type_struct);
            set_var_i8(id_struct, handle_id_struct);
            // primary data structures (variable length vectors)
            void* handle_indx_struct      = new_handle_var_ilength();
            void* handle_mpar_struct      = new_handle_var_dlength();
            void* handle_prog_struct      = new_handle_var_dlength();
            void* handle_diag_struct      = new_handle_var_dlength();
            void* handle_flux_struct      = new_handle_var_dlength();
            set_var_ilength(indx_struct, handle_indx_struct);
            set_var_dlength(mpar_struct, handle_mpar_struct);
            set_var_dlength(prog_struct, handle_prog_struct);
            set_var_dlength(diag_struct, handle_diag_struct);
            set_var_dlength(flux_struct, handle_flux_struct);
            // basin-average structures
            void* handle_bpar_struct      = new_handle_var_d();
            void* handle_bvar_struct      = new_handle_var_dlength();
            set_var_d(bpar_struct,handle_bpar_struct);
            set_var_dlength(bvar_struct,handle_bvar_struct);
            // ancillary data structures
            void* handle_dpar_struct      = new_handle_var_d();
            void* handle_finalize_stats   = new_handle_var_i();
            void* handle_output_timestep = new_handle_var_i();
            set_var_d(dpar_struct, handle_dpar_struct);
            set_var_i(finalize_stats, handle_finalize_stats);
            set_var_i(output_timestep, handle_output_timestep);

            writeBasinToNetCDF(self->state.handle_ncid, &index_gru,
                handle_finalize_stats, handle_output_timestep, handle_bvar_stat,
                handle_bvar_struct, &err);
        
            writeTimeToNetCDF(self->state.handle_ncid,
                handle_finalize_stats, handle_output_timestep, handle_time_struct, &err);

            writeDataToNetCDF(self->state.handle_ncid, &index_gru, &index_hru,
                handle_finalize_stats, handle_forc_stat, handle_forc_struct,
                handle_prog_stat, handle_prog_struct, handle_diag_stat, 
                handle_diag_struct, handle_flux_stat, handle_flux_struct,
                handle_indx_stat, handle_indx_struct, handle_output_timestep,
                &err);
            
            self->state.file_access_timing.updateEndPoint("write_duration");
            

        },

        [=](write_output, int indxGRU, int indxHRU, int numStepsToWrite,
            caf::actor refToRespondTo) {
            int err;
            int returnMessage = 9999;
            
        },

        [=](read_and_write, int indxGRU, int indxHRU, int numStepsToWrite, int currentFile, 
            caf::actor refToRespondTo) {
            int err;

            err = readForcing(self, currentFile);
            if (err != 0)
                aout(self) << "\nERROR: FILE_ACCESS_ACTOR - READING_FORCING FAILED\n";
        },

        [=](run_failure, int indxGRU) {
            int listIndex;

            // update the list in Fortran
            updateFailed(&indxGRU);

            listIndex = self->state.output_manager->decrementMaxSize(indxGRU);
          
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

    initFailedHRUTracker(&self->state.num_gru);

    def_output(self->state.handle_ncid, &self->state.start_gru, &self->state.num_gru, &self->state.num_gru, &err);
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
        &self->state.num_gru, &self->state.err);

    // // Read In all of the attribres for the number of GRUs in the run Domian
    // readAttributeFileAccessActor(&self->state.num_gru, &err);
    // if (err != 0) {
    //     aout(self) << "ERROR: FILE_ACCESS_ACTOR readAttributeFilAccessActor() \n";
    //     std::string function = "readAttributeFileAccessActor";
    //     self->send(self->state.parent, file_access_actor_err_v, function);
    //     self->quit();
    //     return;
    // }

    // Noah-MP table information
    overwriteParam(&self->state.num_gru, &err);
    if (err != 0) {
        aout(self) << "ERROR: FILE_ACCESS_ACTOR overwriteParam() \n";
        std::string function = "overwriteParam";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }


    // // Read in all of the parmeters for the number of GRUs in the run Domain
    // readParamFileAccessActor(&self->state.start_gru, &self->state.num_gru, &err);
    // if (err != 0) {
    //     aout(self) <<  "ERROR: FILE_ACCESS_ACTOR readParamFileAccessActor() \n";
    //     std::string function = "readParamFileAccessActor";
    //     self->send(self->state.parent, file_access_actor_err_v, function);
    //     self->quit();
    //     return;
    // }

    // Initalize the output manager  
    self->state.output_manager = new OutputManager(self->state.num_vectors_in_output_manager, self->state.num_gru);
    
    self->send(self->state.parent, done_file_access_actor_init_v);
    // initalize the forcingFile array
    self->state.filesLoaded = 0;
    for (int i = 1; i <= self->state.numFiles; i++) {
        self->state.forcing_file_list.push_back(Forcing_File_Info(i));
    }
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
            &self->state.stepsInCurrentFile, &self->state.start_gru, 
            &self->state.num_gru, &self->state.err);
        
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


void readAttributes(stateful_actor<file_access_state>* self) {
    int err = 0;
    openAttributeFile(&self->state.attribute_ncid, &err);
    getNumVar(&self->state.attribute_ncid, &self->state.num_var_in_attributes_file, &err);
    for (int index_gru = self->state.start_gru; 
        index_gru < self->state.num_gru + self->state.start_gru; index_gru++) {

        std::vector<double> attr_array(self->state.num_var_in_attributes_file);
        std::vector<int> type_array(self->state.num_var_in_attributes_file);
        std::vector<long int> id_array(self->state.num_var_in_attributes_file);

        int index_hru = 1;
        readAttributeFromNetCDF(&self->state.attribute_ncid, &index_gru, &index_hru,
            &self->state.num_var_in_attributes_file, &attr_array[0], &type_array[0],
            &id_array[0], &err);

        self->state.attr_arrays_for_hrus.push_back(attr_array);
        self->state.type_arrays_for_hrus.push_back(type_array);
        self->state.id_arrays_for_hrus.push_back(id_array);
    }

    closeAttributeFile(&self->state.attribute_ncid, &err);

}

// void readParameters(stateful_actor<file_access_state>* self) {

// }

} // end namespace