#ifndef ClientReference_H_
#define ClientReference_H_

// #include "caf/all.h"
#include <chrono>
#include <vector>

class ClientReference {
    private:
        int ID; // Will be the same as index in array
        caf::actor client;
        int currentGRU;
        std::chrono::time_point<std::chrono::system_clock> lastSeen;
        std::vector<int> GRUsCompleted;
        bool isConnected;
    
    public:
        ClientReference(int ID, caf::actor client) {
            this->ID = ID;
            this->client = client;
            this->lastSeen = std::chrono::high_resolution_clock::now();
            this->isConnected = true;
            this->currentGRU = 0;
        }

        void gruCompleted() {
            this->GRUsCompleted.push_back(this->currentGRU);
            this->currentGRU = 0;
        }

        void updateCurrentGRU(int newCurrentGRU) {
            this->currentGRU = newCurrentGRU;
        }

        bool isClientConnected() {
            return this->isConnected;
        }

        void disconnectClient() {
            this->isConnected = false;
        }

        void updateLastSeen() {
            this->lastSeen = std::chrono::high_resolution_clock::now();
        }

        std::chrono::time_point<std::chrono::system_clock> getLastSeen() {
            return this->lastSeen;
        }

        int getCurrentGRU() {
            return this->currentGRU;
        }


        


        // ~ClientReference() {
        //     delete[] GRUsCompleted;
        // }


};

#endif