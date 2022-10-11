#include "caf/all.hpp"
#include "client.hpp"


Client::Client(int id, caf::actor client_actor, std::string hostname) {
    this->id = id;
    this->client_actor = client_actor;
    this->hostname = hostname;
    this->connected = true;
}

// Getters
caf::actor Client::getActor() {
    return this->client_actor;
}

int Client::getLostPotentialIndicator() {
    return this->lost_Potential_indicator;
}

int Client::getID() {
    return this->id;
}

std::string Client::getHostname() {
    return this->hostname;
}

// Setters
void Client::updateCurrentBatchID(int batch_id) {
    this->current_batch_id = batch_id;
}

// Methods
void Client::incrementLostPotential() {
    this->lost_Potential_indicator++;
}

void Client::decrementLostPotential() {
    this->lost_Potential_indicator--;
}

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

Client_Container::Client_Container(int lost_node_threshold) {
    this->lost_client_threshold = lost_node_threshold;
}

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

// Needs to be used direclty after getClient so same index is used
bool Client_Container::checkForLostClient(int index) {
    this->client_list[index].incrementLostPotential();
    if (this->lost_client_threshold < this->client_list[index].getLostPotentialIndicator()) {
        return true;
    } else {
        return false;
    }
}

void Client_Container::decrementLostPotential(int client_id) {
    int index = findClientByID(client_id);
    this->client_list[index].decrementLostPotential();
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
    int index = findClientByID(client_id);
    this->client_list[index].updateCurrentBatchID(batch_id);;
}

int Client_Container::findClientByID(int client_id) {
    for(int i = 0; i < this->num_clients; i++) {
        if (client_id == this->client_list[i].getID()){
            return i;
        }
    }
    throw "Cannot Find Client";
}






