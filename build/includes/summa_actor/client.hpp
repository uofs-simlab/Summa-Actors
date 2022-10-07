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
        int current_batch_id;


    public:
        Client(int id, caf::actor client_actor, std::string hostname);

        void updateCurrentBatchID(int batch_id);

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

        /**
         * @brief Update the current batch id the client is working on
         * 
         * @param client_id The id of the client we want to update the batch for
         * @param batch_id The id of the batch
         */
        void updateCurrentBatch(int client_id, int batch_id);

        /**
         * @brief Get the number of connected clients
         * 
         * @return int 
         */
        int getNumClients();

        /**
         * @brief Get the Client ID of a cleint from its actor ref
         * 
         * @param cleint_actor 
         * @return int 
         */
        int getClientID(caf::actor client_actor);

        /**
         * @brief Get a client from the client list
         * This is used when we need to get all of the 
         * clients but we do not want to remove them
         * from the client_list;
         * @param index 
         * @return Client 
         */
        Client getClient(int index);

        /**
         * @brief Removes a client from the back of the list
         * Used when we are finished and want to pop the clients
         * off the list to send them an exit message
         * @return Client 
         */
        Client removeClient_fromBack();

        /**
         * @brief Get the Hostname of a client by their ClientID
         * 
         * @param client_id 
         * @return std::string 
         */
        std::string getHostname_ByClientID(int client_id);

        /**
         * @brief Check if the client list is empty
         * 
         * @return true 
         * @return false 
         */
        bool isEmpty();

};