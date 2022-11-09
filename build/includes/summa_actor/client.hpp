#pragma once

#include "caf/all.hpp"
// #include "summa_server.hpp"
#include "batch_manager.hpp"
#include <vector>
#include <sstream>


class Client {
    private:
        // Identifying Characteristics 
        caf::actor client_actor;
        std::string hostname;

        int id;
        int batches_solved;
        bool connected;
        bool assigned_batch;
        int current_batch_id;

        int lost_potential_indicator = 0; // value to indicate the Potential that a client is lost.
        // The greater the lost_Potential_indicator the greater chances the client has been lost.


    public:
        Client(int id, caf::actor client_actor, std::string hostname);
        // ####################################################################
        //                              Getters
        // ####################################################################
        caf::actor getActor();
        int getLostPotentialIndicator();
        int getID();
        int getCurrentBatchID();
        std::string getHostname();
        bool getAssignedBatch();
        // ####################################################################
        //                              Setters
        // ####################################################################
        void updateCurrentBatchID(int batch_id);
        void setAssignedBatch(bool boolean);
        
        // methods
        /**
         * @brief Increments the lost_likely_hood indicator variable
         * this is done everytime a client is sent a heartbeat message
         * 
         * checks if the client is likely lost or not
         */
        void incrementLostPotential();

        /**
         * @brief Decrement the lost_likley_hood indicator variables
         * this is done everytime a client sends a heartbeat message back 
         * to the server
         */
        void decrementLostPotential();

        /**
         * Check if the clients lost_potential_indicator is over a certain
         * threshold
        */
        bool isLost(int threshold);


        std::string toString();

};


class Client_Container {
    private:
        int num_clients = 0;
        int lost_client_threshold; // value to determine if client is lost
        std::vector<Client> connected_client_list;
        std::vector<Client> lost_client_list;


    public:
        Client_Container(int lost_node_threshold);
        // ####################################################################
        //                              Getters
        // ####################################################################
        int getNumClients();
        int getClientID(caf::actor client_actor);
        Client getClient(int index);
        std::vector<Client> getConnectedClientList();
        std::vector<Client> getLostClientList();

        // ####################################################################
        //                              Setters
        // ####################################################################
        void setAssignedBatch(int client_id, bool boolean);

        // Methods
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
         * @brief Decrement the lost_likley_hood indicator variables
         * this is done everytime a client sends a heartbeat message back 
         * to the server
         */
        void decrementLostPotential(int client_id);

        void incrementLostPotential(int client_id);

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


        /**
            Function that checks for lost clients
            Returns true if clients are lost and false if they are none
        */
        bool checkForLostClients();

        /**
         * Transfer all lost batches
        */
        void reconcileLostBatches(Batch_Container* batch_container);

        std::string connectedClientsToString();

        std::string lostClientsToString();


        /**
         * Sends all connected clients a heartbeat message
        */
        // void sendAllClientsHeartbeat(stateful_actor<summa_server_state>* self);
};