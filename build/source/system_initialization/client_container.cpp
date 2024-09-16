#include "client_container.hpp"


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

std::optional<Client> Client_Container::getClient(caf::actor_addr client_ref) {
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        if(client_ref == client->getActor()) {
            return *client;
        }
    }
    return {};
}

// ####################################################################
//                              Setters
// ####################################################################

void Client_Container::setBatchForClient(caf::actor client_ref, std::optional<Batch> batch) {
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        if (client_ref == client->getActor()) {
            if (batch.has_value()) {
                client->setBatch(batch.value());
            } else {
                client->setBatch({});
            }
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
        Client{client_actor, hostname});
}

// void Client_Container::removeClient(Client client) {
//     for(auto client_it = begin(this->client_list); client_it != end(this->client_list); ++client_it) {
//         if (client_it->getID() == client.getID()) {
//             this->client_list.erase(client_it);
//             break;
//         }
//     }
// }

std::optional<Client> Client_Container::getIdleClient() {
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        if (client->getBatch().has_value() == false) {
            return *client;
        }
    }
    return {};
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
