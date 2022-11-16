#include "client/client_container.hpp"


Client_Container::Client_Container() {
    this->id_counter = 0;
}
// ####################################################################
//                              Getters
// ####################################################################

int Client_Container::getNumClients() {
    return this->client_list.size();
}

std::vector<Client> Client_Container::getClientList() {
    return this->client_list;
}

Client Client_Container::getClient(caf::actor_addr client_ref) {
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        if(client_ref == client->getActor()) {
            return *client;
        }
    }
    throw "ERROR -- Client Not Found";
}

// ####################################################################
//                              Setters
// ####################################################################

void Client_Container::setBatchForClient(caf::actor client_ref, Batch batch) {
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        if (client_ref == client->getActor()) {
            client->setBatch(batch);
            break;
        }
    }
}


// ####################################################################
//                              Methods
// ####################################################################
void Client_Container::addClient(caf::actor client_actor, std::string hostname) {
    int client_id = this->id_counter;
    this->id_counter++;
    
    this->client_list.push_back(
        Client{client_id, client_actor, hostname});
}

bool Client_Container::isEmpty() {
    return this->client_list.empty();
}

Client Client_Container::removeClient_fromBack() {
    if (this->client_list.empty()) {
        throw "ERROR -- Client List is Empty";
    }
    Client client = this->client_list.back();
    this->client_list.pop_back();
    return client;
}

std::string Client_Container::toString() {
    std::stringstream out_string;
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        out_string << client->toString() << "\n";
    }
    return out_string.str();
}
