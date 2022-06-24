#include "caf/all.hpp"
#include "client.hpp"


Client::Client(int id, caf::actor client_actor, std::string host_name) {
    this->id = id;
    this->client_actor = client_actor;
    this->host_name = host_name;
    this->connected = true;
}


caf::actor Client::getActor() {
    return this->client_actor;
}