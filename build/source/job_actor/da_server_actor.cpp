#include "da_server_actor.hpp"


using namespace caf;

behavior DAServerActor::make_behavior() {
  
  // Publish Actor To port
  self_->println("DAServerActor: Starting");
  auto res = self_->system().middleman().publish(
      self_, settings_.distributed_settings_.port_);
  if (!res) {
    self_->println("DAServerActor: Failed to publish actor");
    return {};
  }
  self_->println("DAServerActor: Published actor");

  // Ensure that the number of Grus is valid
  file_manager_ = std::make_unique<FileManager>(
      settings_.job_actor_settings_.file_manager_path_);
  file_gru_ = file_manager_->getFileGru();
  if (file_gru_ < 0) {
    self_->println("ERROR--File Manager: Unable To Verify Number Of GRUs");
    self_->quit();
    return {};
  } else if (file_gru_ < start_gru_ + num_gru_) {
    self_->println("ERROR--File Manager: Number Of GRUs Exceeds File GRUs");
    self_->quit();
    return {};
  }

  // Set up the node ranges
  gru_struc_ = std::make_unique<GruStruc>(
      start_gru_, num_gru_, settings_.job_actor_settings_.max_run_attempts_);
  gru_struc_->setNodeGruInfo(settings_.distributed_settings_.num_nodes_);
  self_->println("DAServerActor: Node GRU Info: {}",
                 gru_struc_->getNodeGruInfoString());

  // Spawn the local node actor
  auto local_client = self_->spawn(actor_from_state<DAClientActor>, "", 
                                   settings_, self_);

  return {
    [this](connect_atom, const std::string& hostname) {
      auto client = actor_cast<actor>(self_->current_sender());
      self_->println("DAServerActor: Received connection request from {}",
                      hostname);
      self_->monitor(client, [this](const error& err) {
        if (!done_) {
          self_->println("DAServerActor: Client went down");
          self_->quit();
        } else {
          clients_exited_++;
          if (clients_exited_ == settings_.distributed_settings_.num_nodes_) {
            self_->println("DAServerActor: All clients exited");
            exit(EXIT_SUCCESS);
          }
        }
      });

      auto node_gru_info = gru_struc_->getNodeGruInfo(
          connected_clients_.size());
      connected_clients_.push_back(client);
      self_->mail(node_gru_info).send(client);
    },

    [this](file_access_actor_ready, int num_timesteps) {
      num_timesteps_ = num_timesteps;
      num_clients_ready_++;
      self_->println("DAServerActor: File Access Actor Ready: {}/{}",
                     num_clients_ready_, 
                     settings_.distributed_settings_.num_nodes_);
      if (num_clients_ready_ == settings_.distributed_settings_.num_nodes_) {
        num_clients_ready_ = 0;
        for (auto& client : connected_clients_) {
          self_->mail(access_forcing_v, iFile).send(client);
        }
      }
    },

    [this](new_forcing_file, int num_steps_iFile, int next_file) {
      self_->println("DAServerActor: Received new forcing file: {}",
                     next_file);
      num_steps_ffile_ = num_steps_iFile;
      iFile = next_file;
      forcing_step_ = 1;
      num_clients_ready_++;                     
      if (num_clients_ready_ == settings_.distributed_settings_.num_nodes_) {
        self_->println("DAServerActor: All clients ready running timestep");
        num_clients_ready_ = 0;
        self_->mail(update_hru_v).send(self_);
      }
    },

    [this](update_hru) {
      for (auto& client : connected_clients_) {
        self_->mail(update_hru_v, timestep_, forcing_step_).send(client);
      }
    },

    [this](done_update) {
      num_clients_ready_++;
      if (num_clients_ready_ == settings_.distributed_settings_.num_nodes_) {
        num_clients_ready_ = 0;
        self_->println("DAServerActor: All clients done updating {}",
                       timestep_);
        forcing_step_++;
        timestep_++;

        if (timestep_ > num_timesteps_) {
          self_->println("DAServerActor: Done");
          done_ = true;
          for (auto& client : connected_clients_) {
            self_->mail(finalize_v).send(client);
          }
        } else if (forcing_step_ > num_steps_ffile_) {
          self_->println("DAServerActor: Need new forcing file");
          for (auto& client : connected_clients_) {
            self_->mail(access_forcing_v, iFile + 1).send(client);
          }
        } else {
          self_->mail(update_hru_v).send(self_);
        }
      }
    }
  };
}