#pragma once
#include "caf/all.hpp"
#include <vector>
#include "batch/batch.hpp"
#include "client/client.hpp"
#include <optional>


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
        std::vector<Client> getClientList();
        std::optional<Client> getClient(caf::actor_addr client_ref);

        // ####################################################################
        //                              Setters
        // ####################################################################
        
        void setBatchForClient(caf::actor client_ref, std::optional<Batch> batch);
        
        // ####################################################################
        //                              Methods
        // ####################################################################
        
        // add a new client to the client_list
        void addClient(caf::actor client_actor, std::string hostname);

        // remove a client from the client_list
        void removeClient(Client client);
        
        // return a client that is not solving a batch or return an empty optional    
        std::optional<Client> getIdleClient();

        

        // Check if the client list is empty
        bool isEmpty();
        
        // pops client at the end of the list
        Client removeClient_fromBack();

        // return printable string
        std::string toString();

        template <class Inspector>
        friend bool inspect(Inspector& inspector, Client_Container& client_container) {
            return inspector.object(client_container).fields(
                inspector.field("client_list", client_container.client_list),
                inspector.field("id_counter", client_container.id_counter));
        }
};

