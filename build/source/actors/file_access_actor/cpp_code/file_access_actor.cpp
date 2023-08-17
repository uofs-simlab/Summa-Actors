#include "file_access_actor.hpp"
#include "forcing_file_info.hpp"
#include "file_access_actor_subroutine_wrappers.hpp"
#include "fortran_data_types.hpp"
#include "message_atoms.hpp"
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

    self->state.num_output_steps = self->state.file_access_actor_settings.num_timesteps_in_output_buffer;


    fileAccessActor_init_fortran(self->state.handle_forcing_file_info, 
                                 &self->state.numFiles,
                                 &self->state.num_steps,
                                 &self->state.file_access_actor_settings.num_timesteps_in_output_buffer, 
                                 self->state.handle_ncid,
                                 &self->state.start_gru, 
                                 &self->state.num_gru, 
                                 &self->state.num_gru, // Filler for num_hrus
                                 &self->state.gru_actor_stats,
                                 &self->state.err);

    aout(self) << "Simluations Steps: " << self->state.num_steps << "\n";


    // read in the inital conditions for the grus/hrus
    readInitConditions(self);
    
    // Inital Files Have Been Loaded - Send Message to Job_Actor to Start Simulation
    self->send(self->state.parent, init_gru_v);
    // initalize the forcingFile array
    self->state.filesLoaded = 0;
    for (int i = 1; i <= self->state.numFiles; i++) {
        self->state.forcing_file_list.push_back(Forcing_File_Info(i));
    }

    // Check that the number of timesteps in the output buffer is not greater than the number of timesteps in the simulation
    if (self->state.num_steps < self->state.file_access_actor_settings.num_timesteps_in_output_buffer) {
        self->state.num_output_steps = self->state.num_steps;
        self->state.file_access_actor_settings.num_timesteps_in_output_buffer = self->state.num_steps;
    }

    // Set up the output container
    self->state.output_container = new Output_Container(self->state.file_access_actor_settings.num_partitions_in_output_buffer,
                                                        self->state.num_gru,
                                                        self->state.file_access_actor_settings.num_timesteps_in_output_buffer,
                                                        self->state.num_steps); 

    return {
        [=](write_param, int index_gru, int index_hru, std::vector<double> attr_struct, 
            std::vector<int> type_struct, std::vector<std::vector<double>> mpar_struct,
            std::vector<double> bpar_struct) {
            int err = 0;

            std::shared_ptr<hru_output_handles> params = std::make_shared<hru_output_handles>();

            self->state.file_access_timing.updateStartPoint("write_duration");

            // populate the newly created Fortran structures
            set_var_d(attr_struct, params->handle_attr_struct);
            set_var_i(type_struct, params->handle_type_struct);
            set_var_dlength(mpar_struct, params->handle_mpar_struct);
            set_var_d(bpar_struct, params->handle_bpar_struct);
            // write the populated data to netCDF
            writeParamToNetCDF(self->state.handle_ncid, &index_gru, &index_hru, 
                               params->handle_attr_struct, 
                               params->handle_type_struct, 
                               params->handle_mpar_struct, 
                               params->handle_bpar_struct, 
                               &err);
        

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
                aout(self) << currentFile << " is larger than expected for a forcing file request from an HRU" << std::endl;
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

        [=] (get_attributes_params, int index_gru) {
            // From Attributes File

            std::vector<double> attr_struct_to_send = self->state.attr_structs_for_hrus[index_gru-1];
            std::vector<int> type_struct_to_send = self->state.type_structs_for_hrus[index_gru-1];
            std::vector<long int> id_struct_to_send = self->state.id_structs_for_hrus[index_gru-1];

            // From Parameters File
            std::vector<double> bpar_struct_to_send = self->state.bpar_structs_for_hrus[index_gru-1];
            std::vector<double> dpar_struct_to_send = self->state.dpar_structs_for_hrus[index_gru-1];
            std::vector<std::vector<double>> mpar_struct_to_send = self->state.mpar_structs_for_hrus[index_gru-1];

            return std::make_tuple(attr_struct_to_send, 
                                   type_struct_to_send, 
                                   id_struct_to_send, 
                                   bpar_struct_to_send, 
                                   dpar_struct_to_send, 
                                   mpar_struct_to_send);
        },

        [=] (get_num_output_steps) { return self->state.num_output_steps; },

        [=](write_output, int index_gru, int index_hru, caf::actor hru_actor) {
            self->state.file_access_timing.updateStartPoint("write_duration");

            Output_Partition *output_partition = self->state.output_container->getOutputPartition(index_gru);

            output_partition->setGRUReadyToWrite(hru_actor);
        
            if (output_partition->isReadyToWrite()) {
                writeOutput(self, output_partition);
            }

            self->state.file_access_timing.updateEndPoint("write_duration");
        },

        [=](restart_failures) {
            self->state.output_container->reconstruct();
        },

        [=](run_failure, int local_gru_index) {
            self->state.file_access_timing.updateStartPoint("write_duration");

            Output_Partition *output_partition = self->state.output_container->getOutputPartition(local_gru_index);
            
            output_partition->addFailedGRUIndex(local_gru_index);

            if (output_partition->isReadyToWrite()) {
                writeOutput(self, output_partition);
            }
            self->state.file_access_timing.updateEndPoint("write_duration");
        },


        [=](finalize, std::vector<serializable_netcdf_gru_actor_info> &netcdf_gru_info) {
            int num_gru = netcdf_gru_info.size();
            WriteGRUStatistics(self->state.handle_ncid, 
                               &self->state.gru_actor_stats, 
                               netcdf_gru_info.data(), 
                               &num_gru, 
                               &self->state.err);

            
            // call output_container deconstructor
            self->state.output_container->~Output_Container();


            aout(self) << "Deallocating Structure" << std::endl;
            FileAccessActor_DeallocateStructures(self->state.handle_forcing_file_info, self->state.handle_ncid);
            // deallocateOutputStructure(&self->state.err);
            aout(self) << "\n________________FILE_ACCESS_ACTOR TIMING INFO RESULTS________________\n";
            aout(self) << "Total Read Duration = " << self->state.file_access_timing.getDuration("read_duration").value_or(-1.0) << " Seconds\n";
            aout(self) << "Total Write Duration = " << self->state.file_access_timing.getDuration("write_duration").value_or(-1.0) << " Seconds\n";
            
            self->quit();
            return std::make_tuple(self->state.file_access_timing.getDuration("read_duration").value_or(-1.0), 
                                   self->state.file_access_timing.getDuration("write_duration").value_or(-1.0));
        },

    };
}


void writeOutput(stateful_actor<file_access_state>* self, Output_Partition* partition) {
                
    int num_timesteps_to_write = partition->getNumStoredTimesteps();
    int start_gru = partition->getStartGRUIndex();
    int max_gru = partition->getMaxGRUIndex();
    
    writeOutput_fortran(self->state.handle_ncid, &num_timesteps_to_write,
        &start_gru, &max_gru, &self->state.err);
    
    partition->updateTimeSteps();

    int num_steps_before_next_write = partition->getNumStoredTimesteps();

    std::vector<caf::actor> hrus_to_update = partition->getReadyToWriteList();
    
    for (int i = 0; i < hrus_to_update.size(); i++) {
        self->send(hrus_to_update[i], num_steps_before_write_v, num_steps_before_next_write);
        self->send(hrus_to_update[i], run_hru_v);
    }

    partition->resetReadyToWriteList();
}

void readInitConditions(stateful_actor<file_access_state>* self) {
    int err;
    openInitCondFile(&self->state.init_cond_ncid, &err);
    readInitCond_prog(&self->state.init_cond_ncid, &self->state.start_gru, &self->state.num_gru, &err);
    readInitCond_bvar(&self->state.init_cond_ncid, &self->state.start_gru, &self->state.num_gru, &err);
    closeInitCondFile(&self->state.init_cond_ncid, &err); 
}

} // end namespace