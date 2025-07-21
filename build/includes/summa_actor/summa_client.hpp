#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"
#include "batch.hpp"
#include "summa_actor.hpp"
#include "message_atoms.hpp"
#include <string>
#include <optional>
#include <unistd.h>
#include <limits.h>


class SummaClient {
  caf::event_based_actor* self_;

  caf::strong_actor_ptr current_server_ = nullptr;
  caf::actor current_server_actor_;
  std::vector<caf::strong_actor_ptr> servers_;
  
  std::string hostname_;
  caf::actor summa_actor_ref_;
  uint16_t port_;
  int batch_id_;
  int client_id_; // id held by server
  bool running_ = false; // initalized to false - flipped to true when client returns behavior summa_client


  // tuple is the actor ref and hostname of the backup server
  std::vector<std::tuple<caf::actor, std::string>> backup_servers_list_;

  Batch current_batch_;
  bool saved_batch_ = false;
  
  DistributedSettings distributed_settings_;

  // SummaActorSettings summa_actor_settings_;
  // FileAccessActorSettings file_access_actor_settings_;
  // JobActorSettings job_actor_settings_;
  // HRUActorSettings hru_actor_settings_;
  Settings settings_;
  public:
    SummaClient(caf::event_based_actor* self, DistributedSettings distributed_settings, Settings settings) 
    : self_(self), distributed_settings_(distributed_settings), settings_(settings) {};

    caf::behavior make_behavior();
};

//behavior summa_client(stateful_actor<summa_client_state>* self, DistributedSettings Distributed_Settings);

// void connecting(stateful_actor<summa_client_state>*, const std::string& host, uint16_t port);

// void findLeadServer(stateful_actor<summa_client_state>* self, strong_actor_ptr serv);


