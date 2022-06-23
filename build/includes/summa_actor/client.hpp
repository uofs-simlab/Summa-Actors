#pragma once

#include "caf/all.hpp"




class Client {
    private:
        int id;
        int batches_solved;
        bool connected;
        caf::actor client_actor;
        std::string host_name;


    public:

};