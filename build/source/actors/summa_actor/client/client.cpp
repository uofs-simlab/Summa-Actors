#include "client/client.hpp"

Client::Client(int id, caf::actor client_actor, std::string hostname) {
    this->id = id;
    this->client_actor = client_actor;
    this->hostname = hostname;
    this->current_batch = {};
}

// ####################################################################
//                              Getters
// ####################################################################
caf::actor Client::getActor() {
    return this->client_actor;
}

int Client::getID() {
    return this->id;
}

std::string Client::getHostname() {
    return this->hostname;
}

std::optional<Batch> Client::getBatch() {
    return this->current_batch;
}

// ####################################################################
//                              Setters
// ####################################################################
void Client::setBatch(Batch batch) {
    this->current_batch = batch;
}

// ####################################################################
//                              Methods
// ####################################################################
std::string Client::toString() {
    std::stringstream out_string;
    
    out_string << "hostname: " << this->hostname << "\n" << 
                  "id: " << this->id << "\n" <<
                  "batches_solved: " << this->batches_solved << "\n";
    return out_string.str();
}