#pragma once

#include "caf/all.hpp"
#include "batch_manager.hpp"
#include <vector>


class Client {
    private:
        int id;
        int batches_solved;
        bool connected;
        caf::actor client_actor;
        std::string hostname;
        Batch* current_batch;


    public:
        Client(int id, caf::actor client_actor, std::string hostname);

        caf::actor getActor();

        int getID();

        std::string getHostname();

};


class Client_Container {
    private:
        int num_clients = 0;

        std::vector<Client> client_list;


    public:
        /**
         * @brief Construct a new Client_Container object
         */
        Client_Container();

        /**
         * @brief add a client to the client vector
         * increment the number of clients
         * 
         * @param client_actor connecting cleint actor_ref
         * @param hostname name of the host that client actor is connecting
         * from
         */
        void addClient(caf::actor client_actor, std::string hostname);

        int getClientID(caf::actor);

        Client removeClient_fromBack();

        std::string getHostname_ByClientID(int client_id);

        bool isEmpty();

};