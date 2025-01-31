#include "da_client_actor.hpp"

using namespace caf;

behavior DAClientActor::make_behavior() {
  
  if (!host_.empty()) {
    self_->println("DAClientActor: Starting");
    auto strong_server = self_->system().middleman().remote_actor(
        host_, settings_.distributed_settings_.port_);
    if (!strong_server) {
      self_->println("DAClientActor: Failed to connect to server");
      return {};
    }
    server_ = *strong_server;
    self_->monitor(server_, [this](const error& err){
      self_->println("DAClientActor: Server went down");
      exit(EXIT_FAILURE);
    });
    self_->println("DAClientActor: Connected to server");
  }

  // else server was passed in as an argument


  gethostname(hostname_, HOST_NAME_MAX);
  self_->mail(connect_atom_v, hostname_).send(server_);

  return {
    [this](NodeGruInfo node_gru_info) {
      self_->println("DAClientActor: Received Node GRU Info: \n Start_Gru = {}"
                     " : Num_Gru = {}\n", node_gru_info.node_start_gru_,
                     node_gru_info.node_num_gru_);
      
      node_gru_info_ = node_gru_info;

      // File Manager Initialization
      file_manager_ = std::make_unique<FileManager>(
          settings_.job_actor_settings_.file_manager_path_);
      auto err_msg = file_manager_->setTimesDirsAndFiles();
      if (!err_msg.empty()) {
        self_->println("DAClientActor: {}", err_msg);
        self_->quit();
        return;
      }

      // Global Fortran State Initialization
      global_fortran_state_ = std::make_unique<SummaGlobalData>();
      auto err = global_fortran_state_->defineGlobalData();
      if (err != 0) {
        self_->println("DAClientActor: Unable To Define Global Data");
        self_->quit();
        return;
      }

      // GruStruc Initialization
      gru_struc_ = std::make_unique<GruStruc>(
          node_gru_info.node_start_gru_, node_gru_info.node_num_gru_,
          settings_.job_actor_settings_.max_run_attempts_);
      if (gru_struc_->readDimension() != 0) {
        self_->println("DAClientActor: GruStruc ReadDimension() Failed");
        self_->quit();
        return;
      }
      if (gru_struc_->readIcondNlayers() != 0) {
        self_->println("DAClientActor: GruStruc ReadIcondNlayers() Failed");
        self_->quit();
        return;
      }

      // SummaInitStruc Initialization
      summa_init_struc_ = std::make_unique<SummaInitStruc>();
      if (summa_init_struc_->allocate(gru_struc_->getNumHru()) != 0) {
        self_->println("DAClientActor: SummaInitStruc allocate() Failed");
        self_->quit();
        return;
      }
      if (summa_init_struc_->summa_paramSetup() != 0) {
        self_->println("DAClientActor: SummaInitStruc paramSetup failed");
        self_->quit();
        return;
      }
      if (summa_init_struc_->summa_readRestart()!= 0) {
        self_->println("DAClientActor: SummaInitStruc readRestart failed");
        self_->quit();
        return;
      }
      // summa_init_struc_->getInitBEStepsIDATol(be_steps_, rel_tol_, abs_tolWat_, abs_tolNrg_);

      NumGRUInfo num_gru_info = NumGRUInfo(
          node_gru_info.node_start_gru_, node_gru_info.node_start_gru_,
          node_gru_info.node_num_gru_, node_gru_info.node_num_gru_,
          gru_struc_->getFileGru(), false);

      file_access_actor_ = self_->spawn(actor_from_state<FileAccessActor>, 
                                       num_gru_info, 
                                       settings_.fa_actor_settings_, self_);

      self_->mail(init_file_access_actor_v, gru_struc_->getFileGru(),
                  gru_struc_->getNumHru())
          .request(file_access_actor_, caf::infinite)
          .await([=](int num_steps) {
        if (num_steps < 0) {
          self_->println("DAClientActor: Error init_file_access_actor_v");
          self_->quit();
          return;
        }
        num_steps_ = num_steps;
        self_->println("DAClientActor: File Access Actor Initialized");

        spawnGruBatches();

        self_->mail(file_access_actor_ready_v, num_steps).send(server_);
      });
    },

    [this](access_forcing, int iFile) {
      self_->println("DAClientActor: Accessing Forcing File: {}", iFile);
      self_->mail(access_forcing_v, iFile, self_).send(file_access_actor_);
    },

    [this](new_forcing_file, int num_steps_iFile, int next_file) {
      self_->println("DAClientActor: New Forcing File: {} : Num Steps: {}",
                     next_file, num_steps_iFile);
      // forcing_step_ = 1;
      num_steps_ffile_ = num_steps_iFile;
      for (auto& gru : gru_struc_->getGruInfo()) {
        self_->mail(update_timeZoneOffset_v, next_file)
            .send(gru->getActorRef());
      }

      self_->mail(new_forcing_file_v, num_steps_iFile, next_file)
          .send(server_);
    },

    [this](update_hru, int timestep, int forcing_step, int output_step) {
      timestep_ = timestep;
      forcing_step_ = forcing_step;
      if (forcing_step > num_steps_ffile_) {
        self_->println("DAClientActor: Error forcing_step_ outside bounds: timestep {} -- "
                        "forcing_step: {}", timestep, forcing_step);
        self_->quit();
        return;
      }
      for (auto& gru : gru_struc_->getGruInfo()) {
        self_->mail(update_hru_v, timestep_, forcing_step_, output_step)
            .send(gru->getActorRef());
      }
    },

    [this](done_update) {
      num_gru_done_timestep_++;
      if (num_gru_done_timestep_ >= gru_struc_->getGruInfo().size()) {
        num_gru_done_timestep_ = 0;
        if (settings_.hru_actor_settings_.print_output_ && 
            timestep_ % settings_.hru_actor_settings_.output_frequency_ == 0) {
          self_->println(
            "DAClientActor: Done Update for timestep: {} -- forcing_step: {}", 
            timestep_, forcing_step_);
        }

        // write output
        int steps_to_write = 1;
        int start_gru = 1;
        self_->mail(write_output_v, 1)
            .send(file_access_actor_);
      }
    },
    [this](write_output, int err) {

      if (err != 0) {
        for (auto& gru : gru_struc_->getGruInfo()) {
          self_->mail(exit_msg_v).send(gru->getActorRef());
        }
        self_->send_exit(file_access_actor_, exit_reason::user_shutdown);
        self_->quit();
      } else {
        self_->mail(done_update_v).send(server_);
      }

    },
    [this](finalize) {
      self_->println("DAClientActor: Exiting Successfully");
      exit(EXIT_SUCCESS);
    }
  };
}


void DAClientActor::spawnGruBatches() {
  self_->println("DAClientActor: Spawning GRU Batch Actors");
  int batch_size;

  if (settings_.job_actor_settings_.batch_size_ < 0) {
    // Automatically determine batch size
    batch_size = std::ceil(static_cast<double>(gru_struc_->getNumGru()) / 
        static_cast<double>(std::thread::hardware_concurrency()));
  } else {
    // Use the user selected batch size
    batch_size = settings_.job_actor_settings_.batch_size_;
  }

  if (batch_size <= 1) {
    batch_size = 1;
  }

  self_->println("DAClientActor: Batch Size {}", batch_size);
  
  int remaining_hru_to_batch = node_gru_info_.node_num_gru_;
  int start_hru_global = node_gru_info_.node_start_gru_;
  int start_hru_local = 1;

  while (remaining_hru_to_batch > 0) {
    int current_batch_size = std::min(batch_size, remaining_hru_to_batch);
    auto gru_batch = self_->spawn(
        actor_from_state<GruBatchActor>, start_hru_global, start_hru_local,  
        current_batch_size, num_steps_, settings_.hru_actor_settings_,
        settings_.fa_actor_settings_.num_timesteps_in_output_buffer_, 
        file_access_actor_, self_);
    std::unique_ptr<GRU> gru_obj = std::make_unique<GRU>(
        start_hru_global, start_hru_local, gru_batch, 1, 1, 1.0e-10, 1.0e-10, 1.0e-10,
        settings_.job_actor_settings_.max_run_attempts_);
    gru_struc_->addGRU(std::move(gru_obj));
    remaining_hru_to_batch -= current_batch_size;
    start_hru_local += current_batch_size;
    start_hru_global += current_batch_size;
  }
  self_->println("DAClientActor: Assembled GRUs into Batches");
}