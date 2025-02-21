#include "gru_actor.hpp"


using namespace caf;

behavior GruActor::make_behavior() {  
  int err = 0;
  f_getNumHruInGru(job_index_, num_hrus_);
  gru_data_ = std::unique_ptr<void, GruDeleter>(new_handle_gru_type(num_hrus_));
  
  std::unique_ptr<char[]> message(new char[256]);
  f_initGru(job_index_, gru_data_.get(), num_steps_output_buffer_, err,
            &message);
  if (err != 0) {
    self_->println("GRU Actor: Error initializing GRU -- {}", message.get());
    self_->quit();
    return {};
  }
  std::fill(message.get(), message.get() + 256, '\0');

  setupGRU_fortran(job_index_, gru_data_.get(), err, &message);
  if (err != 0) {
    self_->println("GRU Actor: Error setting up GRU -- {}", message.get());
    self_->quit();
    return {};
  }
  std::fill(message.get(), message.get() + 256, '\0');
  
  readGRURestart_fortran(job_index_, gru_data_.get(), err, &message);
  if (err != 0) {
    self_->println("GRU Actor: Error reading GRU restart -- {}", message.get());
    self_->quit();
    return {};
  }

  f_setGruTolerances(gru_data_.get(),hru_actor_settings_.be_steps_,hru_actor_settings_.rel_tol_,
      hru_actor_settings_.abs_tolWat_,hru_actor_settings_.abs_tolNrg_);

  data_assimilation_mode_ ? self_->become(data_assimilation_mode()) :
                            self_->become(async_mode());

  return {};
}


behavior GruActor::async_mode() {
  return {
    [this](update_hru_async) {
      self_->mail(get_num_output_steps_v, job_index_)
          .request(file_access_actor_, infinite)
          .await([this](int num_steps) {
            num_steps_until_write_ = num_steps;
            self_->mail(access_forcing_v, iFile_, self_).
                send(file_access_actor_);
      });
    },

    [this](new_forcing_file, int num_forc_steps, int iFile) {
      int err;
      std::unique_ptr<char[]> message(new char[256]);
      iFile_ = iFile;
      stepsInCurrentFFile_ = num_forc_steps;
      setTimeZoneOffsetGRU_fortran(iFile_, gru_data_.get(), err, &message);
      if (err != 0) {
        self_->println("GRU Actor: Error setting time zone offset");
        self_->quit();
        return;
      }
      forcingStep_ = 1;
      self_->mail(run_hru_v).send(self_);
    },

    [this](num_steps_before_write, int num_steps) {
      num_steps_until_write_ = num_steps;
      output_step_ = 1;
    },
    
    [this](run_hru) {
      int err = 0;
      std::unique_ptr<char[]> message(new char[256]);
      while (num_steps_until_write_ > 0) {
        if (forcingStep_ > stepsInCurrentFFile_) {
          self_->mail(access_forcing_v, iFile_ + 1, self_)
              .send(file_access_actor_);
          break;
        }
        num_steps_until_write_--;
        if (hru_actor_settings_.print_output_ && 
            timestep_ % hru_actor_settings_.output_frequency_ == 0) {
          self_->println("GRU Actor {}: timestep={}, forcingStep={}, iFile={}", 
                         job_index_, timestep_, forcingStep_, iFile_);
        }
        readGRUForcing_fortran(job_index_, timestep_, forcingStep_, iFile_, 
                               gru_data_.get(), err, &message);
        if (err != 0) {
          handleErr(err, message);
          return;
        }
        std::fill(message.get(), message.get() + 256, '\0'); // Clear message
        runGRU_fortran(job_index_, timestep_, gru_data_.get(), dt_init_factor_, 
                       err, &message);
        if (err != 0) {
          handleErr(err, message);
          return;
        }
        std::fill(message.get(), message.get() + 256, '\0'); // Clear message
        writeGRUOutput_fortran(job_index_, timestep_, output_step_,
                               gru_data_.get(), err, &message);
        if (err != 0) {
          handleErr(err, message);
          return;
        }

        timestep_++;
        forcingStep_++;
        output_step_++;

        if (timestep_ > num_steps_) {
          self_->mail(done_hru_v).send(self_);
          break;
        }
      }
      // Our output structure is full
      if (num_steps_until_write_ <= 0) {
        self_->mail(write_output_v, job_index_, self_)
            .send(file_access_actor_);
      }
    },

    [this](done_hru) {
      self_->mail(done_hru_v, job_index_).send(parent_);
      self_->quit();
      return;
    }
  };
}


behavior GruActor::data_assimilation_mode() {
  return {
    [this](update_timeZoneOffset, int iFile) {
      int err = 0;
      std::unique_ptr<char[]> message(new char[256]);
      iFile_ = iFile;
      setTimeZoneOffsetGRU_fortran(iFile_, gru_data_.get(), err, &message);
      if (err != 0) {
        self_->mail(err_atom_v, job_index_, timestep_, err, message.get())
            .send(parent_);
        self_->quit();
        return;
      }
    },

    [this](update_hru, int time_step, int forcing_step, int output_step) {
      int err = 0;
      std::unique_ptr<char[]> message(new char[256]);
      readGRUForcing_fortran(job_index_, time_step, forcing_step, iFile_, 
                             gru_data_.get(), err, &message);
      std::fill(message.get(), message.get() + 256, '\0'); // Clear message
      runGRU_fortran(job_index_, time_step, gru_data_.get(), dt_init_factor_, 
                     err, &message);
      if (err !=0 ) {
        self_->println("GRU Actor {}: Error running GRU -- {}", 
                       job_index_, message.get());
      }
      std::fill(message.get(), message.get() + 256, '\0'); // Clear message
      writeGRUOutput_fortran(job_index_, time_step, output_step, 
                             gru_data_.get(), err, &message);
      self_->mail(done_update_v).send(parent_);
    }
  };
}



// Utility Functions

void GruActor::handleErr(int err, std::unique_ptr<char[]>& message) {
  self_->println("GRU Actor {}-{}: Error running GRU at timestep {}", 
                 job_index_, netcdf_index_, timestep_);
  // int local_err = 0;
  // std::unique_ptr<char[]> local_message(new char[256]);
  // f_fillOutputWithErrs(job_index_, timestep_, output_step_, gru_data_.get(), 
  //                      local_err, &local_message);

  self_->println("GRU Actor: SUMMA error message -- {}", message.get());
  self_->mail(err_atom_v, job_index_, timestep_, err, message.get())
      .send(parent_);
  self_->quit();
}
  