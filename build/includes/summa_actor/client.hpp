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
        int lost_Potential_indicator; // value to indicate the Potential that a client is lost.
        // The greater the lost_Potential_indicator the greater chances the client has been lost.


    public:
        /**
         * @brief Construct a new Client object
         * 
         * @param id 
         * @param client_actor 
         * @param hostname 
         */
        Client(int id, caf::actor client_actor, std::string hostname);

        // Getters
        /**
         * @brief Returns the actor_reference of the client
         */
        caf::actor getActor();
               /**
         * @brief Get the value of the lost_Potential_indicator variable.
         * @return int 
         */
        int getLostPotentialIndicator();

        /**
         * @brief Returns the ID of the client
         */
        int getID();

        /**
         * @brief Get the Hostname of the client
         */
        std::string getHostname();

        // Setters
        /**
         * @brief Sets the batch_id of the batch the client is currently computing
         */
        void updateCurrentBatchID(int batch_id);

        // methods
        /**
         * @brief Increments the lost_likley_hood indicator variable
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

};

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

class Client_Container {
    private:
        int num_clients = 0;
        int lost_client_threshold = 3; // value to determine if client is lost
        std::vector<Client> client_list;


    public:
        /**
         * @brief Construct a new Client_Container object
         */
        Client_Container();

        // Getters
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
         * @brief Increments the lost_potential indicator variable
         * this is done everytime a client is sent a heartbeat message
         */
        bool checkForLostClient(int index);

        /**
         * @brief Decrement the lost_likley_hood indicator variables
         * this is done everytime a client sends a heartbeat message back 
         * to the server
         */
        void decrementLostPotential(int client_id);

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
};