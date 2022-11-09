#include "caf/all.hpp"
#include "client.hpp"


Client::Client(int id, caf::actor client_actor, std::string hostname) {
    this->id = id;
    this->client_actor = client_actor;
    this->hostname = hostname;
    this->connected = true;
    this->assigned_batch = false;
}

// Getters
caf::actor Client::getActor() {
    return this->client_actor;
}
int Client::getLostPotentialIndicator() {
    return this->lost_potential_indicator;
}
int Client::getID() {
    return this->id;
}
int Client::getCurrentBatchID() {
    return this->current_batch_id;
}
std::string Client::getHostname() {
    return this->hostname;
}
bool Client::getAssignedBatch() {
    return this->assigned_batch;
}
// Setters
void Client::updateCurrentBatchID(int batch_id) {
    this->current_batch_id = batch_id;
    this->assigned_batch = true;
}

void Client::setAssignedBatch(bool boolean) {
    this->assigned_batch = boolean;
}

// Methods
void Client::incrementLostPotential() {
    this->lost_potential_indicator++;
}

void Client::decrementLostPotential() {
    this->lost_potential_indicator--;
}

bool Client::isLost(int threshold) {
    return lost_potential_indicator > threshold;
}

std::string Client::toString() {
    std::stringstream out_string;
    
    out_string << "hostname: " << this->hostname << "\n" << 
                  "id: " << this->id << "\n" <<
                  "batches_solved: " << this->batches_solved << "\n" <<
                  "connected: " << this->connected << "\n" <<
                  "assigned_batch: " << this->assigned_batch << "\n" <<
                  "current_batch_id: " << this->current_batch_id << "\n" <<
                  "lost_potential_indicator: " << this->lost_potential_indicator << "\n"; 

    return out_string.str();
}

////////////////////////////////////////////////



Client_Container::Client_Container(int lost_node_threshold) {
    this->lost_client_threshold = lost_node_threshold;
}

void Client_Container::addClient(caf::actor client_actor, std::string hostname) {
    int client_id = this->num_clients;
    
    this->connected_client_list.push_back(
        Client{client_id, client_actor, hostname});
    
    this->num_clients++;

}

void Client_Container::setAssignedBatch(int client_id, bool boolean) {
    int index = findClientByID(client_id);
    this->connected_client_list[index].setAssignedBatch(boolean);
}

int Client_Container::getNumClients() {
    return this->num_clients;
}

Client Client_Container::getClient(int index) {
    if (index > this->num_clients) {
        throw "Trying to access a client outside of the connected_client_list";
    }

    return this->connected_client_list[index];
}

std::vector<Client> Client_Container::getConnectedClientList() {
    return this->connected_client_list;
}

void Client_Container::decrementLostPotential(int client_id) {
    int index = findClientByID(client_id);
    this->connected_client_list[index].decrementLostPotential();
}

void Client_Container::incrementLostPotential(int client_id) {
    int index = findClientByID(client_id);
    this->connected_client_list[index].incrementLostPotential();
}


int Client_Container::getClientID(caf::actor client_actor) {
    for (int i = 0; i < num_clients; i++) {
        if (client_actor == this->connected_client_list[i].getActor()){
            return this->connected_client_list[i].getID();
        }
    }
    return -1;
}

std::string Client_Container::getHostname_ByClientID(int client_id) {
    return this->connected_client_list[client_id].getHostname();
}

bool Client_Container::isEmpty() {
    return this->connected_client_list.empty();
}

Client Client_Container::removeClient_fromBack() {
    Client client = this->connected_client_list.back();
    this->connected_client_list.pop_back();
    return client;
}

void Client_Container::updateCurrentBatch(int client_id, int batch_id) {
    int index = findClientByID(client_id);
    this->connected_client_list[index].updateCurrentBatchID(batch_id);;
}

int Client_Container::findClientByID(int client_id) {
    for(int i = 0; i < this->num_clients; i++) {
        if (client_id == this->connected_client_list[i].getID()){
            return i;
        }
    }
    throw "Cannot Find Client";
}


std::optional<Client> Client_Container::findIdleClient() {
    for(int i = 0; i < this->num_clients; i++) {
        if (!this->connected_client_list[i].getAssignedBatch()) {
            return this->connected_client_list[i];
        }
    }

    return {};
}

bool Client_Container::checkForLostClients() {
    bool return_val = false;
    for(auto client = begin(this->connected_client_list); client != end(this->connected_client_list); ++client) {
        client->incrementLostPotential();
        if (client->isLost(this->lost_client_threshold)) {
            this->lost_client_list.push_back(*client);
            this->connected_client_list.erase(client);
            
            return_val = true;
        }
    }
    return return_val;
}


void Client_Container::reconcileLostBatches(Batch_Container* batch_container) {
    for(auto client = begin(this->lost_client_list); client != end(this->lost_client_list); ++client) {
        batch_container->updateBatchStatus_LostClient(client->getCurrentBatchID());
    }

    this->lost_client_list.clear();
}

std::string Client_Container::connectedClientsToString() {
    std::stringstream out_string;
    for(auto client = begin(this->connected_client_list); client != end(this->connected_client_list); ++client) {
        out_string << client->toString() << "\n";
    }
    return out_string.str();
}

std::string Client_Container::lostClientsToString() {
    std::stringstream out_string;
    for(auto client = begin(this->lost_client_list); client != end(this->lost_client_list); ++client) {
        out_string << client->toString() << "\n";
    }
    return out_string.str();
}





