#pragma once

#include "caf/all.hpp"
#include "batch_manager.hpp"


class Client {
    private:
        int id;
        int batches_solved;
        bool connected;
        caf::actor client_actor;
        std::string host_name;
        Batch* current_batch;


    public:
        Client(int id, caf::actor client_actor, std::string host_name);

        caf::actor getActor();

};