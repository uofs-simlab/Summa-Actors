#include "data_assimilation_server_actor.hpp"


using namespace caf;

behavior DataAssimilationServerActor::make_behavior() {
  
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

  return {
    [this](connect_atom, const std::string& hostname) {
      auto client = actor_cast<actor>(self_->current_sender());
      self_->println("DAServerActor: Received connection request from {}",
                      hostname);
      self_->monitor(client, [this](const error& err) {
        self_->println("DAServerActor: Client went down");
      });

      auto node_gru_info = gru_struc_->getNodeGruInfo(
          connected_clients_.size());
      connected_clients_.push_back(client);
      self_->mail(node_gru_info).send(client);
    },
  };
}