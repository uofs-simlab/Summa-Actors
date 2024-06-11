#include "gru_actor.hpp"


using namespace caf;

behavior GruActor::make_behavior() {
  getNumHRU(job_index_, num_hrus_);
  self_->println("GRU Actor Started with {} HRUs", num_hrus_);
  hrus_.resize(num_hrus_);
  gru_data_ = new_handle_gru_type(num_hrus_);
  int err = 0;
  std::unique_ptr<char[]> message(new char[256]);
  initGRU_fortran(job_index_, gru_data_, err, &message);
  std::fill(message.get(), message.get() + 256, '\0'); // Clear message
  setupGRU_fortran(job_index_, gru_data_, err, &message);
  std::fill(message.get(), message.get() + 256, '\0'); // Clear message
  readGRURestart_fortran(job_index_, gru_data_, err, &message);

  self_->println("GRU Actor: HRUs Initialized");
  self_->send(self_, update_hru_async_v);

  return {
    [this](update_hru_async) {
      self_->request(file_access_actor_, infinite, get_num_output_steps_v)
          .await([this](int num_steps) {
            num_steps_until_write_ = num_steps;
            self_->send(file_access_actor_, access_forcing_v, iFile_, self_);
          });
    },

    [this](new_forcing_file, int num_forc_steps, int iFile) {
      int err;
      std::unique_ptr<char[]> message(new char[256]);
      iFile_ = iFile;
      stepsInCurrentFFile_ = num_forc_steps;
      setTimeZoneOffsetGRU_fortran(iFile_, gru_data_, err, &message);
      if (err != 0) {
        self_->println("GRU Actor: Error setting time zone offset");
        self_->quit();
        return;
      }
      forcingStep_ = 1;
      self_->send(self_, run_hru_v);
    },

    [this](num_steps_before_write, int num_steps) {
      num_steps_until_write_ = num_steps;
      output_structure_step_index_ = 1;
    },
    
    [this](run_hru) {
      self_->println("GRU Actor: Running HRUs");
      int err = 0;
      std::unique_ptr<char[]> message(new char[256]);
      while (num_steps_until_write_ > 0) {
        if (forcingStep_ > stepsInCurrentFFile_) {
          self_->println("GRU Actor: New Forcing File");
          self_->send(file_access_actor_, access_forcing_v, iFile_ + 1, self_);
          break;
        }
        num_steps_until_write_--;
        self_->println("GRU Actor: timestep={}, forcingStep={}, iFile={}", 
                       timestep_, forcingStep_, iFile_);
        readGRUForcing_fortran(job_index_, timestep_, forcingStep_, iFile_, 
                               gru_data_, err, &message);
        std::fill(message.get(), message.get() + 256, '\0'); // Clear message
        runGRU_fortran(job_index_, timestep_, gru_data_, dt_init_factor_, 
                       err, &message);
        std::fill(message.get(), message.get() + 256, '\0'); // Clear message
        writeGRUOutput_fortran(job_index_, timestep_, 
                               output_structure_step_index_, gru_data_, err, 
                               &message);

        timestep_++;
        forcingStep_++;
        output_structure_step_index_++;

        if (timestep_ > num_steps_) {
          self_->println("GRU Actor: Done");
          self_->send(self_, done_hru_v);
          break;
        }

        // Our output structure is full
        if (num_steps_until_write_ <= 0) {
          self_->println("GRU Actor: Writing Output");
          self_->send(file_access_actor_, write_output_v, job_index_, 1, self_);
        }
      }
    },

    [this](done_hru) {
      self_->send(parent_, done_hru_v, job_index_);
      self_->quit();
      return;
    }
  };
}

  