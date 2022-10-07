#include "caf/all.hpp"
#include "client.hpp"


Client::Client(int id, caf::actor client_actor, std::string hostname) {
    this->id = id;
    this->client_actor = client_actor;
    this->hostname = hostname;
    this->connected = true;
}


caf::actor Client::getActor() {
    return this->client_actor;
}

void Client::updateCurrentBatchID(int batch_id) {
    this->current_batch_id = batch_id;
}

int Client::getID() {
    return this->id;
}

std::string Client::getHostname() {
    return this->hostname;
}


Client_Container::Client_Container() {}

void Client_Container::addClient(caf::actor client_actor, std::string hostname) {
    int client_id = this->num_clients;
    
    this->client_list.push_back(
        Client{client_id, client_actor, hostname});
    
    this->num_clients++;

}

int Client_Container::getNumClients() {
    return this->num_clients;
}

Client Client_Container::getClient(int index) {
    if (index > this->num_clients) {
        throw "Trying to access a client outside of the client_list";
    }

    return this->client_list[index];
}


int Client_Container::getClientID(caf::actor client_actor) {
    for (int i = 0; i < num_clients; i++) {
        if (client_actor == this->client_list[i].getActor()){
            return this->client_list[i].getID();
        }
    }
    return -1;
}

std::string Client_Container::getHostname_ByClientID(int client_id) {
    return this->client_list[client_id].getHostname();
}

bool Client_Container::isEmpty() {
    return this->client_list.empty();
}

Client Client_Container::removeClient_fromBack() {
    Client client = this->client_list.back();
    this->client_list.pop_back();
    return client;
}

void Client_Container::updateCurrentBatch(int client_id, int batch_id) {
    for (int i = 0; i < num_clients; i++) {
        if (client_id == this->client_list[i].getID()){
            this->client_list[i].updateCurrentBatchID(batch_id);
        }
    }
}



