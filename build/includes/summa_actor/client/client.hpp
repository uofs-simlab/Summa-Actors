#pragma once
#include "caf/all.hpp"
#include <optional>
#include "batch/batch.hpp"

class Client {
    private:
        caf::actor client_actor;
        std::string hostname;

        int id;
        int batches_solved;
        std::optional<Batch> current_batch;


    public:
        Client(int id = -1, caf::actor client_actor = nullptr, std::string hostname = "");
        // ####################################################################
        //                              Getters
        // ####################################################################
        caf::actor getActor();
        int getID();
        std::string getHostname();
        std::optional<Batch> getBatch();
        // ####################################################################
        //                              Setters
        // ####################################################################
        void setBatch(Batch batch);
        // ####################################################################
        //                              Methods
        // ####################################################################
        std::string toString();

        // Serialization so CAF can send an object of this class to another actor
        template <class Inspector>
        friend bool inspect(Inspector& inspector, Client& client) {
            return inspector.object(client).fields(
                inspector.field("client_actor",client.client_actor),
                inspector.field("hostname",client.hostname),
                inspector.field("id",client.id),
                inspector.field("batches_solved",client.batches_solved),
                inspector.field("current_batch",client.current_batch));
            }

};