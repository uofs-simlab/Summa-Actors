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

// void Client_Container::setAssignedBatch(int client_id, bool boolean) {
//     int index = findClientByID(client_id);
//     this->client_list[index].setAssignedBatch(boolean);
// }

int Client_Container::getNumClients() {
    return this->client_list.size();
}

// Client Client_Container::getClient(int index) {
//     if (index > this->num_clients) {
//         throw "Trying to access a client outside of the client_list";
//     }

//     return this->client_list[index];
// }

std::vector<Client> Client_Container::getConnectedClientList() {
    return this->client_list;
}



// int Client_Container::getClientID(caf::actor client_actor) {
//     for (int i = 0; i < num_clients; i++) {
//         if (client_actor == this->client_list[i].getActor()){
//             return this->client_list[i].getID();
//         }
//     }
//     return -1;
// }

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


// std::optional<Client> Client_Container::findIdleClient() {
//     for(int i = 0; i < this->client_list; i++) {
//         if (!this->client_list[i].getAssignedBatch()) {
//             return this->client_list[i];
//         }
//     }

//     return {};
// }

Client Client_Container::getClient(caf::actor_addr client_ref) {
    std::cout << "Looking for client\n";
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        if(client_ref == client->getActor()) {
            std::cout << "Found Client\n";
            return *client;
        }
    }

    throw "ERROR -- Client Not Found";
}

// bool Client_Container::checkForLostClients() {
//     bool return_val = false;
//     for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
//         client->incrementLostPotential();
//         if (client->isLost(this->lost_client_threshold)) {
//             this->lost_client_list.push_back(*client);
//             this->client_list.erase(client);
            
//             return_val = true;
//         }
//     }
//     return return_val;
// }


// void Client_Container::reconcileLostBatches(Batch_Container* batch_container) {
//     for(auto client = begin(this->lost_client_list); client != end(this->lost_client_list); ++client) {
//         batch_container->updateBatchStatus_LostClient(client->getCurrentBatchID());
//     }

//     this->lost_client_list.clear();
// }

std::string Client_Container::connectedClientsToString() {
    std::stringstream out_string;
    for(auto client = begin(this->client_list); client != end(this->client_list); ++client) {
        out_string << client->toString() << "\n";
    }
    return out_string.str();
}
