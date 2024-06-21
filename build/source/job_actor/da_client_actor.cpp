#include "da_client_actor.hpp"

using namespace caf;

behavior DAClientActor::make_behavior() {
  
  self_->println("DAClientActor: Starting");
  auto strong_server = self_->system().middleman().remote_actor(
      "localhost", settings_.distributed_settings_.port_);
  if (!strong_server) {
    self_->println("DAClientActor: Failed to connect to server");
    return {};
  }
  server_ = *strong_server;
  self_->monitor(server_, [this](const error& err){
    self_->println("DAClientActor: Server went down");
    self_->quit();
  });
  self_->println("DAClientActor: Connected to server");

  gethostname(hostname_, HOST_NAME_MAX);

  self_->mail(connect_atom_v, hostname_).send(server_);

  return {
    [this](NodeGruInfo node_gru_info) {
      self_->println("DAClientActor: Received Node GRU Info: \n Start_Gru = {}"
                     " : Num_Gru = {}\n", node_gru_info.node_start_gru_,
                     node_gru_info.node_num_gru_);
    },
  };
}