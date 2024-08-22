#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "message_atoms.hpp"
#include "batch.hpp"
#include <string>
#include <optional>
#include <unistd.h>
#include <limits.h>
#include "openwq_global_data.hpp"

extern "C" {
  void openwq_start_step_fortran(int& timestep);
    void openwq_space_step_fortran(int& timestep);
    void openwq_space_step_end_fortran(int& timestep);


}

namespace caf {

struct openwq_actor_state {
  strong_actor_ptr current_server = nullptr;
  actor current_server_actor;
  std::vector<strong_actor_ptr> servers;
  
  std::string hostname;
  uint16_t port;
  int batch_id;
  int client_id; // id held by server
  bool running = false; // initalized to false - flipped to true when client returns behavior summa_client

  int start_gru;
  int num_gru;
  int num_hru;
  int ran_steps;


  std::unique_ptr<openWQGlobalData> global_openwq_state;
    std::vector<int> hru_timesteps;



  // tuple is the actor ref and hostname of the backup server
  std::vector<std::tuple<caf::actor, std::string>> backup_servers_list;

  Batch current_batch;
  bool saved_batch = false;
  
};

behavior openwq_actor(stateful_actor<openwq_actor_state>* self, int start_gru, int num_gru);


}