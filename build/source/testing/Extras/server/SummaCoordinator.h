#ifndef SUMMACOORDINATOR_H_
#define SUMMACOORDINATOR_H_

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "GRUInfo.h"
#include "GRUStruc.h"
#include "ClientReference.h"
#include "string.h"
#include <unistd.h>
#include <iostream>
#include <fstream>
#include <list>
#include <chrono>
#include <vector>

using namespace caf;
void checkIfDisconnected(std::vector<ClientReference*> &clientList, std::vector<int*> &GRUsToRepeat, 
std::chrono::time_point<std::chrono::system_clock> lastConnectedCheck);


struct summa_coordinator_state {
    int indxCounter;
    int numGRUs;
    int numGRUsLeftToCompute;
    int startGRU;
    int counter;
    int numClientsConnected;

    int numSuccess;
    int numFailed;

    // ClientManager *clientManager;
    std::vector<ClientReference*> clientList;
    int clientIdCounter;

    std::list<caf::actor> clients;
    std::list<int> passed;
    std::vector<int> failed;
    std::vector<int> GRUsToRepeat;
    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;
    double duration;
    GRUStruc *gru_struc;

    std::chrono::time_point<std::chrono::system_clock> lastConnectedCheck;

};


#endif