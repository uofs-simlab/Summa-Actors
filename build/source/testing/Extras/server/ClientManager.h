#ifndef ClientManager_H_
#define ClientManager_H_

// #include "caf/all.h"
#include <vector>
#include "ClientReference.h"


class ClientManager {
    private:
       
        int id_counter; // value just increases when new client connects
        int num_clients;
    
    public:
    ClientManager(caf::actor server) {
        // First value in client list is always the server
        // The server will always be value 0
        this->id_counter = 0;
        this->num_clients = 0;
        ClientReference *serverReference = new ClientReference(id_counter, server);
        this->clientList.push_back(serverReference);
        this->id_counter++;
    }

    void addClient(caf::actor client) {
        ClientReference *ref = new ClientReference(id_counter, client);
        this->clientList.push_back(ref);
        this->id_counter++;
        this->num_clients++;
    }






    // ~ClientManager() {
    //     delete[] clientList;
    // }
};

#endif