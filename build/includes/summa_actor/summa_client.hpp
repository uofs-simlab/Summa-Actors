#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"
#include "batch.hpp"
#include "summa_actor.hpp"
#include "summa_server.hpp"
#include "message_atoms.hpp"
#include <string>
#include <optional>
#include <unistd.h>
#include <limits.h>


class SummaClientActor {
  private:
    caf::event_based_actor* self_;
    Settings settings_;
    std::string server_hostname_;
    
    char hostname_[HOST_NAME_MAX];
    caf::actor server_ = nullptr;

    Batch current_batch_;
    caf::actor current_worker_;
    bool saved_batch_ = false;

    std::unordered_map<caf::actor, Client> connected_clients_;
    std::vector<BatchContainer> simulations_;

  public:
    SummaClientActor(caf::event_based_actor* self, std::string server_hostname, 
        Settings settings, caf::actor server = nullptr) :
        self_(self), server_hostname_(server_hostname), settings_(settings),
        server_(server) {};
    caf::behavior make_behavior();

    int connectToServer();
    void connectToClient();
    int publishClient();
    void handleDisconnect(caf::actor client_actor);
    std::string getServerSF(); // SF = Server File 
};