#pragma once
#include "caf/all.hpp"
#include <optional>
#include "batch/batch.hpp"


class Batch;

class Client {
    private:
        caf::actor client_actor;
        std::string hostname;

        int id;
        int batches_solved;
        bool connected;

        std::optional<Batch*> current_batch;


    public:
        Client(int id, caf::actor client_actor, std::string hostname);
        // ####################################################################
        //                              Getters
        // ####################################################################
        caf::actor getActor();
        int getID();
        std::string getHostname();
        // ####################################################################
        //                              Setters
        // ####################################################################
        void setBatch(Batch *batch);
        // ####################################################################
        //                              Methods
        // ####################################################################
        std::string toString();

};