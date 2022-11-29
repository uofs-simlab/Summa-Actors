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


namespace caf {

behavior file_access_actor(stateful_actor<file_access_state>* self, int start_gru, int num_gru, 
    File_Access_Actor_Settings file_access_actor_settings, actor parent) {
    aout(self) << "\n----------File_Access_Actor Started----------\n";

    // Set Up timing Info we wish to track
    self->state.file_access_timing = TimingInfo();
    self->state.file_access_timing.addTimePoint("read_duration");
    self->state.file_access_timing.addTimePoint("write_duration");

    self->state.file_access_actor_settings = file_access_actor_settings;

    self->state.parent = parent;
    self->state.num_gru = num_gru;
    self->state.start_gru = start_gru;
    self->state.handle_forcing_file_info = new_handle_file_info();
    self->state.handle_ncid = new_handle_var_i();
    self->state.err = 0;

    // Setup output container


    self->state.num_output_steps = 73;
    self->state.output_container = new Output_Container(num_gru, self->state.num_output_steps);

        
    initalizeFileAccessActor(self);

    return {
        [=](write_param, int index_gru, int index_hru, std::vector<double> attr_struct, 
            std::vector<int> type_struct, std::vector<std::vector<double>> mpar_struct,
            std::vector<double> bpar_struct) {
            int err = 0;

            self->state.file_access_timing.updateStartPoint("write_duration");
            self->state.output_handles.handle_attr_struct = new_handle_var_d();
            self->state.output_handles.handle_type_struct = new_handle_var_i();
            self->state.output_handles.handle_mpar_struct = new_handle_var_dlength();
            self->state.output_handles.handle_bpar_struct = new_handle_var_d();
            // populate the newly created Fortran structures
            set_var_d(attr_struct, self->state.output_handles.handle_attr_struct);
            set_var_i(type_struct, self->state.output_handles.handle_type_struct);
            set_var_dlength(mpar_struct, self->state.output_handles.handle_mpar_struct);
            set_var_d(bpar_struct, self->state.output_handles.handle_bpar_struct);
            // write the populated data to netCDF
            writeParamToNetCDF(self->state.handle_ncid, &index_gru, &index_hru, 
                self->state.output_handles.handle_attr_struct, 
                self->state.output_handles.handle_type_struct, 
                self->state.output_handles.handle_mpar_struct, 
                self->state.output_handles.handle_bpar_struct, &err);
            
            delete_handle_var_d(self->state.output_handles.handle_attr_struct);
            delete_handle_var_i(self->state.output_handles.handle_type_struct);
            delete_handle_var_dlength(self->state.output_handles.handle_mpar_struct);
            delete_handle_var_d(self->state.output_handles.handle_bpar_struct);
            

            self->state.file_access_timing.updateEndPoint("write_duration");
            
        },

        [=](access_forcing, int currentFile, caf::actor refToRespondTo) {
            if (currentFile <= self->state.numFiles) {
                if(self->state.forcing_file_list[currentFile - 1].isFileLoaded()) { // C++ starts at 0 Fortran starts at 1
                    // Send the HRU actor the new forcing file
                    // then tell it to get back to running
                    self->send(refToRespondTo, new_forcing_file_v, 
                        self->state.forcing_file_list[currentFile - 1].getNumSteps(),
                        currentFile);

                } else {
                    self->state.file_access_timing.updateStartPoint("read_duration");
                    
                    // Load the file
                    read_forcingFile(self->state.handle_forcing_file_info, &currentFile,
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

                    // Send the HRU actor the new forcing file
                    // then tell it to get back to running
                    self->send(refToRespondTo, new_forcing_file_v, 
                        self->state.forcing_file_list[currentFile - 1].getNumSteps(),
                        currentFile);
                }
            } else {
                aout(self) << currentFile << " is larger than expected" << std::endl;
            }
            
        },

        [=](access_forcing_internal, int currentFile) {
            if (self->state.filesLoaded <= self->state.numFiles &&
                currentFile <= self->state.numFiles) {
                if (self->state.forcing_file_list[currentFile - 1].isFileLoaded()) {
                    aout(self) << "File Loaded when shouldn't be \n";
                }
                self->state.file_access_timing.updateStartPoint("read_duration");

                read_forcingFile(self->state.handle_forcing_file_info, &currentFile,
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

        [=] (get_attributes_params, int index_gru, caf::actor actor_to_respond) {
            // From Attributes File
            std::vector<double> attr_struct_to_send = self->state.attr_structs_for_hrus[index_gru-1];
            std::vector<int> type_struct_to_send = self->state.type_structs_for_hrus[index_gru-1];
            std::vector<long int> id_struct_to_send = self->state.id_structs_for_hrus[index_gru-1];

            // From Parameters File
            std::vector<double> bpar_struct_to_send = self->state.bpar_structs_for_hrus[index_gru-1];
            std::vector<double> dpar_struct_to_send = self->state.dpar_structs_for_hrus[index_gru-1];
            std::vector<std::vector<double>> mpar_struct_to_send = self->state.mpar_structs_for_hrus[index_gru-1];

            self->send(actor_to_respond, get_attributes_params_v, attr_struct_to_send,
                type_struct_to_send, id_struct_to_send, bpar_struct_to_send, 
                dpar_struct_to_send, mpar_struct_to_send);
            
        },

        [=] (get_num_output_steps, caf::actor hru) {
            self->send(hru, num_steps_before_write_v, self->state.num_output_steps);
        },

        [=](write_output, int index_gru, int index_hru, caf::actor hru_actor,
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

            /**
             - We recive the output from one HRU
             - We convert it to the correct storage format - from array to fortran handle
             - We store it in out data structure
             - Check if we need to write
             - If we do, write
             - If no do nothing
            */


            // initalizeOutputHandles(self);
            hru_output_handles hru_output;
            
            int err = 0;
            // hru information
            // hru_output.hru_actor = hru_actor;
            // hru_output.index_gru = index_gru;
            // hru_output.index_hru = index_hru;
            // statistic structures
            set_var_dlength(forc_stat, hru_output.handle_forc_stat);
            set_var_dlength(prog_stat, hru_output.handle_prog_stat);
            set_var_dlength(diag_stat, hru_output.handle_diag_stat);
            set_var_dlength(flux_stat, hru_output.handle_flux_stat);
            set_var_dlength(indx_stat, hru_output.handle_indx_stat);
            set_var_dlength(bvar_stat, hru_output.handle_bvar_stat);
            // primary data structures (scalars)
            set_var_i(time_struct, hru_output.handle_time_struct);
            set_var_d(forc_struct, hru_output.handle_forc_struct);
            set_var_d(attr_struct, hru_output.handle_attr_struct);
            set_var_i(type_struct, hru_output.handle_type_struct);
            set_var_i8(id_struct,  hru_output.handle_id_struct);
            // primary data structures (variable length vectors)
            set_var_ilength(indx_struct, hru_output.handle_indx_struct);
            set_var_dlength(mpar_struct, hru_output.handle_mpar_struct);
            set_var_dlength(prog_struct, hru_output.handle_prog_struct);
            set_var_dlength(diag_struct, hru_output.handle_diag_struct);
            set_var_dlength(flux_struct, hru_output.handle_flux_struct);
            // basin-average structures
            set_var_d(bpar_struct, hru_output.handle_bpar_struct);
            set_var_dlength(bvar_struct, hru_output.handle_bvar_struct);
            // ancillary data structures
            set_var_d(dpar_struct, hru_output.handle_dpar_struct);
            set_var_i(finalize_stats, hru_output.handle_finalize_stats);
            set_var_i(output_timestep, hru_output.handle_output_timestep);

            // self->state.vector_of_output_handles.push_back(self->state.output_handles);

            self->state.output_container->insertOutput(index_gru, hru_output);

            if (self->state.output_container->isFull(index_gru)) {
                aout(self) << "Writing output for GRU: " << index_gru << "\n";

                std::vector<std::vector<hru_output_handles>> hru_output = self->state.output_container->getAllHRUOutput();
                

                for (int i = 0; i < hru_output[0].size(); i++) {

                writeBasinToNetCDF(self->state.handle_ncid, &index_gru,
                    hru_output[0][i].handle_finalize_stats, 
                    hru_output[0][i].handle_output_timestep, 
                    hru_output[0][i].handle_bvar_stat,
                    hru_output[0][i].handle_bvar_struct, &err);

                writeTimeToNetCDF(self->state.handle_ncid,
                    hru_output[0][i].handle_finalize_stats, 
                    hru_output[0][i].handle_output_timestep, 
                    hru_output[0][i].handle_time_struct, &err);

                writeDataToNetCDF(self->state.handle_ncid, &index_gru, &index_hru,
                    hru_output[0][i].handle_finalize_stats, 
                    hru_output[0][i].handle_forc_stat, 
                    hru_output[0][i].handle_forc_struct,
                    hru_output[0][i].handle_prog_stat, 
                    hru_output[0][i].handle_prog_struct, 
                    hru_output[0][i].handle_diag_stat, 
                    hru_output[0][i].handle_diag_struct, 
                    hru_output[0][i].handle_flux_stat, 
                    hru_output[0][i].handle_flux_struct,
                    hru_output[0][i].handle_indx_stat, 
                    hru_output[0][i].handle_indx_struct, 
                    hru_output[0][i].handle_output_timestep,
                    &err);
                }
                

                // reset the hrus counter
                // for all hrus that just wrote, reset the counter
                self->state.output_container->clearAll();
                self->send(hru_actor, num_steps_before_write_v, self->state.num_output_steps);
                self->send(hru_actor, run_hru_v);
            }
            
            self->state.file_access_timing.updateEndPoint("write_duration");

            // deallocateOutputHandles(self);
        
        },

        [=](run_failure, int indxGRU) {
            int listIndex;

            // update the list in Fortran
            updateFailed(&indxGRU);

            // listIndex = self->state.output_manager->decrementMaxSize(indxGRU);
          
        },

        [=](done_hru, caf::actor hru_actor, int index_gru, int index_hru) {
            aout(self) << "HRU: " << index_hru << " is done" << "\n";

        },

        /**
         * Message from JobActor
         * OutputManager needs to be adjusted so the failed HRUs can run again
         */
        [=](restart_failures) {
            resetFailedArray();
            // self->state.output_manager->restartFailures();
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

    };
}


void initalizeFileAccessActor(stateful_actor<file_access_state>* self) {
    int indx = 1;
    int err = 0;
    
    // read information on model forcing files
    ffile_info(&indx, 
        self->state.handle_forcing_file_info, &self->state.numFiles, &err);
    if (err != 0) {
        aout(self) << "Error: ffile_info_C - File_Access_Actor \n";
        std::string function = "ffile_info_C";
        self->send(self->state.parent, file_access_actor_err_v, function);
        self->quit();
        return;
    }

    // save model decisions as named integers
    mDecisions(&self->state.num_steps, &err); 
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

    // Read in the attribute and parameter information for the HRUs to request
    readAttributes(self);
    readParameters(self);

    // read in the inital conditions for the grus/hrus
    readInitConditions(self);
    
    self->send(self->state.parent, done_file_access_actor_init_v);
    // initalize the forcingFile array
    self->state.filesLoaded = 0;
    for (int i = 1; i <= self->state.numFiles; i++) {
        self->state.forcing_file_list.push_back(Forcing_File_Info(i));
    }
}

void readAttributes(stateful_actor<file_access_state>* self) {
    int err = 0;
    openAttributeFile(&self->state.attribute_ncid, &err);
    
    getNumVarAttr(&self->state.attribute_ncid, &self->state.num_var_in_attributes_file, &err);
    
    for (int index_gru = 1; index_gru < self->state.num_gru + 1; index_gru++) {

        void* handle_attr_struct = new_handle_var_d();
        void* handle_type_struct = new_handle_var_i();
        void* handle_id_struct   = new_handle_var_i8();
        int index_hru = 1;

        allocateAttributeStructures(&index_gru, &index_hru, handle_attr_struct, handle_type_struct,
            handle_id_struct, &err);

        readAttributeFromNetCDF(&self->state.attribute_ncid, &index_gru, &index_hru,
            &self->state.num_var_in_attributes_file, handle_attr_struct, handle_type_struct,
            handle_id_struct, &err);
        
        // attr struct
        std::vector<double> attr_struct_to_push = get_var_d(handle_attr_struct);
        self->state.attr_structs_for_hrus.push_back(attr_struct_to_push);
        delete_handle_var_d(handle_attr_struct);
        // type struct
        std::vector<int> type_struct_to_push = get_var_i(handle_type_struct);
        self->state.type_structs_for_hrus.push_back(type_struct_to_push);
        delete_handle_var_i(handle_type_struct);
        // id struct
        std::vector<long int> id_struct_to_push = get_var_i8(handle_id_struct);
        self->state.id_structs_for_hrus.push_back(id_struct_to_push);
        delete_handle_var_i8(handle_id_struct);
    }

    closeAttributeFile(&self->state.attribute_ncid, &err);
}

void readParameters(stateful_actor<file_access_state>* self) {
    int err = 0;
    int index_hru = 1;

    openParamFile(&self->state.param_ncid, &self->state.param_file_exists, 
        &err);

    getParamSizes(&self->state.dpar_array_size, &self->state.bpar_array_size,
        &self->state.type_array_size);
    

    if (self->state.param_file_exists) {
        getNumVarParam(&self->state.param_ncid, &self->state.num_var_in_param_file,
            &err);
    } else {
        self->state.num_var_in_param_file = self->state.type_array_size;
    }

    for (int index_gru = 1; index_gru < self->state.num_gru + 1; index_gru++) {

        std::vector<double> dpar_array(self->state.dpar_array_size);
        void* handle_type_struct = new_handle_var_i();
        void* handle_dpar_struct = new_handle_var_d();
        void* handle_mpar_struct = new_handle_var_dlength();      
        void* handle_bpar_struct = new_handle_var_d();  
        std::vector<double> bpar_array(self->state.dpar_array_size);        

        allocateParamStructures(&index_gru, &index_hru, handle_dpar_struct, 
            handle_mpar_struct, handle_bpar_struct, &err);

        // need to convert attr_struct to FORTRAN format   
        set_var_i(self->state.type_structs_for_hrus[index_gru-1], handle_type_struct); 
    
        overwriteParam(&index_gru, &index_hru, 
            handle_type_struct,
            handle_dpar_struct, 
            handle_mpar_struct, 
            handle_bpar_struct, 
            &err);

        if (self->state.param_file_exists) {
            readParamFromNetCDF(&self->state.param_ncid, &index_gru, &index_hru,
                &self->state.start_gru, 
                &self->state.num_var_in_param_file, 
                handle_mpar_struct, 
                handle_bpar_struct,
                &err);
        }

        // type_struct
        delete_handle_var_i(handle_type_struct);
        
        // dpar_struct
        std::vector<double> dpar_struct_to_push = get_var_d(handle_dpar_struct);
        self->state.dpar_structs_for_hrus.push_back(dpar_struct_to_push);
        delete_handle_var_d(handle_dpar_struct);
        // mpar_struct
        std::vector<std::vector<double>> mpar_struct_to_push = get_var_dlength(handle_mpar_struct);
        self->state.mpar_structs_for_hrus.push_back(mpar_struct_to_push);
        delete_handle_var_dlength(handle_mpar_struct);
        // bpar_struct
        std::vector<double> bpar_struct_to_push = get_var_d(handle_bpar_struct);
        self->state.bpar_structs_for_hrus.push_back(bpar_struct_to_push);
        delete_handle_var_d(handle_bpar_struct);
    }
    closeParamFile(&self->state.param_ncid, &err);

}


void readInitConditions(stateful_actor<file_access_state>* self) {
    int err;
    openInitCondFile(&self->state.init_cond_ncid, &err);
    readInitCond_prog(&self->state.init_cond_ncid, &self->state.start_gru, &self->state.num_gru, &err);
    readInitCond_bvar(&self->state.init_cond_ncid, &self->state.start_gru, &self->state.num_gru, &err);
    closeInitCondFile(&self->state.init_cond_ncid, &err); 
}

void initalizeOutputHandles(stateful_actor<file_access_state>* self) {
    // Statistic Structures
    self->state.output_handles.handle_forc_stat = new_handle_var_dlength();
    self->state.output_handles.handle_prog_stat = new_handle_var_dlength();
    self->state.output_handles.handle_diag_stat = new_handle_var_dlength();
    self->state.output_handles.handle_flux_stat = new_handle_var_dlength();
    self->state.output_handles.handle_indx_stat = new_handle_var_dlength();
    self->state.output_handles.handle_bvar_stat = new_handle_var_dlength();
    // primary data structures (scalars)
    self->state.output_handles.handle_time_struct = new_handle_var_i();
    self->state.output_handles.handle_forc_struct = new_handle_var_d();
    self->state.output_handles.handle_attr_struct = new_handle_var_d();
    self->state.output_handles.handle_type_struct = new_handle_var_i();
    self->state.output_handles.handle_id_struct   = new_handle_var_i8();
    // primary data structures (variable length vectors)
    self->state.output_handles.handle_indx_struct = new_handle_var_ilength();
    self->state.output_handles.handle_mpar_struct = new_handle_var_dlength();
    self->state.output_handles.handle_prog_struct = new_handle_var_dlength();
    self->state.output_handles.handle_diag_struct = new_handle_var_dlength();
    self->state.output_handles.handle_flux_struct = new_handle_var_dlength();
    // basin-average structures
    self->state.output_handles.handle_bpar_struct = new_handle_var_d();
    self->state.output_handles.handle_bvar_struct = new_handle_var_dlength();
    // ancillary data structures
    self->state.output_handles.handle_dpar_struct     = new_handle_var_d();
    self->state.output_handles.handle_finalize_stats  = new_handle_var_i();
    self->state.output_handles.handle_output_timestep = new_handle_var_i();
}

void deallocateOutputHandles(stateful_actor<file_access_state>* self) {
            // Statistic Structures
    delete_handle_var_dlength(self->state.output_handles.handle_forc_stat);
    delete_handle_var_dlength(self->state.output_handles.handle_prog_stat);
    delete_handle_var_dlength(self->state.output_handles.handle_diag_stat);
    delete_handle_var_dlength(self->state.output_handles.handle_flux_stat);
    delete_handle_var_dlength(self->state.output_handles.handle_indx_stat);
    delete_handle_var_dlength(self->state.output_handles.handle_bvar_stat);
    // primary data structures (scalars)
    delete_handle_var_i(self->state.output_handles.handle_time_struct);
    delete_handle_var_d(self->state.output_handles.handle_forc_struct);
    delete_handle_var_d(self->state.output_handles.handle_attr_struct);
    delete_handle_var_i(self->state.output_handles.handle_type_struct);
    delete_handle_var_i8(self->state.output_handles.handle_id_struct);
    // primary data structures (variable length vectors)
    delete_handle_var_ilength(self->state.output_handles.handle_indx_struct);
    delete_handle_var_dlength(self->state.output_handles.handle_mpar_struct);
    delete_handle_var_dlength(self->state.output_handles.handle_prog_struct);
    delete_handle_var_dlength(self->state.output_handles.handle_diag_struct);
    delete_handle_var_dlength(self->state.output_handles.handle_flux_struct);
    // basin-average structures
    delete_handle_var_d(self->state.output_handles.handle_bpar_struct);
    delete_handle_var_dlength(self->state.output_handles.handle_bvar_struct);
    // ancillary data structures
    delete_handle_var_d(self->state.output_handles.handle_dpar_struct);
    delete_handle_var_i(self->state.output_handles.handle_finalize_stats);
    delete_handle_var_i(self->state.output_handles.handle_output_timestep);
}

} // end namespace