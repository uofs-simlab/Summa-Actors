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

int Client::getID() {
    return this->id;
}

std::string Client::getHostname() {
    return this->hostname;
}