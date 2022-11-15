#include "client/client_container.hpp"


Client_Container::Client_Container() {
    this->id_counter = 0;
}
// ####################################################################
//                              Getters
// ####################################################################
// ####################################################################
//                              Setters
// ####################################################################
// ####################################################################
//                              Methods
// ####################################################################
void Client_Container::addClient(caf::actor client_actor, std::string hostname) {
    int client_id = this->id_counter;
    this->id_counter++;
    
    this->client_list.push_back(
        Client{client_id, client_actor, hostname});
}

void Client_Container::setBatchForClient(caf::actor client_ref, Batch *batch) {
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        if (client_ref == client->getActor()) {
            client->setBatch(batch);
            break;
        }
    }
}

int Client_Container::getNumClients() {
    return this->client_list.size();
}

std::vector<Client> Client_Container::getConnectedClientList() {
    return this->client_list;
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


Client Client_Container::getClient(caf::actor_addr client_ref) {
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        if(client_ref == client->getActor()) {
            return *client;
        }
    }
    throw "ERROR -- Client Not Found";
}

std::string Client_Container::connectedClientsToString() {
    std::stringstream out_string;
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        out_string << client->toString() << "\n";
    }
    return out_string.str();
}
