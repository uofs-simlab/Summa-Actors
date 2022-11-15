#pragma once
#include "caf/all.hpp"
#include <vector>
#include "batch/batch.hpp"
#include "client/client.hpp"



class Client_Container {
    private:
        std::vector<Client> client_list;
        int id_counter;

    public:
        Client_Container();
        // ####################################################################
        //                              Getters
        // ####################################################################
        int getNumClients();
        int getClientID(caf::actor client_actor);
        std::vector<Client> getConnectedClientList();
        std::vector<Client> getLostClientList();

        // ####################################################################
        //                              Setters
        // ####################################################################
        void setAssignedBatch(int client_id, bool boolean);
        void setBatchForClient(caf::actor client_ref, Batch *batch);
        // ####################################################################
        //                              Methods
        // ####################################################################
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

        /**
         * @brief Find the index of a client in the client_list
         * 
         * @param client_id 
         * @return int 
         */
        int findClientByID(int client_id);

        /**
        * Removes client from client container that has lost connection
        */
        void removeLostClient(int index);

        /**
         * Look for an idle client
         */
        std::optional<Client> findIdleClient();


        // find a client by its actor ref
        Client getClient(caf::actor_addr client_ref);

        /**
            Function that checks for lost clients
            Returns true if clients are lost and false if they are none
        */
        bool checkForLostClients();

        /**
         * Transfer all lost batches
        */
        // void reconcileLostBatches(Batch_Container* batch_container);

        std::string connectedClientsToString();

        std::string lostClientsToString();


        /**
         * Sends all connected clients a heartbeat message
        */
        // void sendAllClientsHeartbeat(stateful_actor<summa_server_state>* self);
};

