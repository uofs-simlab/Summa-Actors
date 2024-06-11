#include "gru_actor.hpp"


using namespace caf;

behavior GruActor::make_behavior() {
  self_->println("GRU Actor Started");
  
  return {};
}

// behavior gru_actor(stateful_actor<gru_actor_state>* self, int netcdf_index, 
//                    int gru_job_index, int num_steps, 
//                    HRU_Actor_Settings hru_actor_settings,
//                    actor file_access_actor, actor parent) {
//   self->state.netcdf_index = netcdf_index;
//   self->state.gru_job_index = gru_job_index;
//   self->state.num_steps = num_steps;
//   self->state.hru_actor_settings = hru_actor_settings;
//   self->state.file_access_actor = file_access_actor;
//   self->state.parent = parent;

//   // Check for lateral flows
//   getNumHRU(self->state.gru_job_index, self->state.num_hrus);
//   aout(self) << "NUM HRUS: " << self->state.num_hrus << "\n";
//   self->state.hrus.resize(self->state.num_hrus);
//   self->state.gru_data = new_handle_gru_type(self->state.num_hrus);
//   int err = 0;
//   std::unique_ptr<char[]> message(new char[256]);
//   initGRU_fortran(self->state.gru_job_index, self->state.gru_data, err, 
//                   &message);
//   std::fill(message.get(), message.get() + 256, '\0'); // Clear message
//   setupGRU_fortran(self->state.gru_job_index, self->state.gru_data, err, 
//                    &message);
//   std::fill(message.get(), message.get() + 256, '\0'); // Clear message
//   readGRURestart_fortran(self->state.gru_job_index, self->state.gru_data, err, 
//                          &message);

//   aout(self) << "GRU Actor: HRUs Initialized\n";
//   self->send(self, update_hru_async_v);

//   return {
//     [=](update_hru_async) {
//       self->request(self->state.file_access_actor, caf::infinite,
//                     get_num_output_steps_v).await([=](int num_steps) {
//         self->state.num_steps_until_write = num_steps;
//         self->send(self->state.file_access_actor, access_forcing_v, 
//                    self->state.iFile, self);
//       });
//     },
//     [=](new_forcing_file, int num_forcing_steps_in_iFile, int iFile) {
//       int err;
//       std::unique_ptr<char[]> message(new char[256]);
//       self->state.iFile = iFile;
//       self->state.stepsInCurrentFFile = num_forcing_steps_in_iFile;
//       setTimeZoneOffsetGRU_fortran(self->state.iFile, self->state.gru_data, 
//                                    err, &message);
//       if (err != 0) {
//         aout(self) << "GRU_Actor: Error setting time zone offset\n";
//         self->quit();
//         return;
//       }
//       self->state.forcingStep = 1;
//       self->send(self, run_hru_v);
//     },

//     [=](num_steps_before_write, int num_steps) {
//       self->state.num_steps_until_write = num_steps;
//       self->state.output_structure_step_index = 1;
//     },
    
//     [=](run_hru) {
//       int err = 0;
//       std::unique_ptr<char[]> message(new char[256]);
//       while(self->state.num_steps_until_write > 0) {
//         if (self->state.forcingStep > self->state.stepsInCurrentFFile) {
//           aout(self) << "GRU Actor: New Forcing File\n";
//           self->send(self->state.file_access_actor, access_forcing_v, 
//                      self->state.iFile+1, self);
//           break;
//         }
//         self->state.num_steps_until_write--;
//         aout(self) << "GRU Actor: timestep=" << self->state.timestep << "\n";
//         readGRUForcing_fortran(self->state.gru_job_index, 
//                                self->state.forcingStep, 
//                                self->state.timestep, self->state.iFile, 
//                                self->state.gru_data, err, &message);
//         std::fill(message.get(), message.get() + 256, '\0'); // Clear message
//         runGRU_fortran(self->state.gru_job_index, self->state.timestep, 
//                        self->state.gru_data, self->state.dt_init_factor,
//                        err, &message);
//         std::fill(message.get(), message.get() + 256, '\0'); // Clear message
//         writeGRUOutput_fortran(self->state.gru_job_index, self->state.timestep, 
//                                self->state.output_structure_step_index, 
//                                self->state.gru_data, err, &message);

//         self->state.timestep++;
//         self->state.forcingStep++;
//         self->state.output_structure_step_index++;

//         if (self->state.timestep > self->state.num_steps) {
//           aout(self) << "GRU Actor: Done\n";
//           self->send(self, done_hru_v);
//           break;
//         }
//       }
//       // Our output structure is full
//       if (self->state.num_steps_until_write <= 0) {
//         aout(self) << "GRU Actor: Writing Output\n";
//         self->send(self->state.file_access_actor, write_output_v,
//                    self->state.gru_job_index, 1, self);
//       }
//     },
    
//     [=](done_hru) {
//       self->send(self->state.parent,done_hru_v,self->state.gru_job_index);
//       self->quit();
//       return;
//     },
//   };
// }