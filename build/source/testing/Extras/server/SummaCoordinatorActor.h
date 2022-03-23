
#ifndef SUMMACOORDINATORACTOR_H_
#define SUMMACOORDINATORACTOR_H_

#include "SummaCoordinator.h"
#include "GRUInfo.h"
#include "../globalFunctions.h"
#include "../messageAtoms.h"
#include <iostream>
#include <fstream>
#include <algorithm>
#include <chrono>

using namespace caf;

#define maxCheckInTimeCoordinator 30 // 30 minutes


/*
* Function to check if client has not checked in for a while
* If not assume the client has unexpectedly disconnected
* ClientList = List of clients to check if they are connected
*/
void checkIfDisconnected(std::vector<ClientReference*> &clientList, std::vector<int> &GRUsToRepeat, 
    std::chrono::time_point<std::chrono::system_clock> &lastConnectedCheck) {
    // Check to see if any clients can be assumed to be disconnected
    if (isCheckInTime(lastConnectedCheck, maxCheckInTimeCoordinator)) {
        std::cout << "Checking if any clients are disconnected \n";
        // TRUE: We check to see if we have any nodes we have not
        // heard from in a while
        // for (int i = 1; i < clientList.size(); i++) {
        //for (auto iterator = clientList->begin(); iterator != clientList->end(); ++iterator) {
            // if (clientList[i]->isClientConnected()) {
            //     std::chrono::time_point<std::chrono::system_clock> lastSeen = clientList[i]->getLastSeen();

            //     if (std::chrono::duration_cast<std::chrono::minutes>
            //         (std::chrono::high_resolution_clock::now() - lastSeen).count() > maxCheckInTimeCoordinator) {
            //         // We Assume this client has been lost
            //         clientList[i]->disconnectClient();
            //         int gru = clientList[i]->getCurrentGRU();
            //         GRUsToRepeat.push_back(gru);
            //         std::cout << "Client with ID = " << i << " Has Unexpectedly Disconnected \n";
            //         std::cout << "GRU:" << gru << " Getting moved to the retry list \n";
            //         std::cout << GRUsToRepeat.size() << "\n";
            //         lastConnectedCheck = std::chrono::high_resolution_clock::now();

            //     }
            // }
        // }
    }
}

behavior summa_coordinator_actor(stateful_actor<summa_coordinator_state>* self, int indxStartGRU, int numGRUs) {
    /*********************************************************************************************************
     ******************************** ACTOR INITIALIZATION ***************************************************
     *********************************************************************************************************/
    self->state.indxCounter = indxStartGRU;
    self->state.startGRU = indxStartGRU;
    self->state.numGRUs = numGRUs;
    self->state.numGRUsLeftToCompute = numGRUs;  // A counter of the number of remaining GRUs left
    aout(self) << self->state.numGRUsLeftToCompute << std::endl;
    self->state.numClientsConnected = 0;
    self->state.numSuccess = 0;
    self->state.numFailed = 0;
    self->state.start = std::chrono::high_resolution_clock::now();
    self->state.lastConnectedCheck = std::chrono::high_resolution_clock::now();

    // add ourselves to be at reference 0
    self->state.clientIdCounter = 0;
    ClientReference *serverReference = new ClientReference(self->state.clientIdCounter, self);
    self->state.clientList.push_back(serverReference);
    self->state.clientIdCounter++;

    /*********************************************************************************************************
     ************************************ END ACTOR INITIALIZATION *******************************************
     *********************************************************************************************************/
    


    /*********************************************************************************************************
     *********************************** ACTOR MESSAGE HANDLERS **********************************************
     *********************************************************************************************************/
    return {
        /*
        * Client Initialization
        * client = client actor attempting to connect
        */
        [=](connect_to_coordinator, actor client, int id) {
            if (self->state.numGRUsLeftToCompute <= 0) {
                                aout(self) << "No GRUs left to compute. Telling Client to exit" << "\n";
                self->send(client, time_to_exit_v);  
            } else {
                // This is the first time we are seing this client
                // Add client to our list
                ClientReference *ref = new ClientReference(self->state.clientIdCounter, client);
                self->state.clientList.push_back(ref);

                aout(self) << "New Client Connected, This is client ID =" << self->state.clientIdCounter << "\n"; 
                ref->updateCurrentGRU(self->state.indxCounter);
                // Send the Client the GRU
                self->send(client, self->state.indxCounter);
            
                self->state.clientIdCounter++;
                self->state.indxCounter++;
                self->state.numClientsConnected++;
            }
        },
        /*
        * A Client has Requested a GRU to Compute
        * request_gru = message atom
        * client = the actor address of the client requesting the GRU
        * id = id of the client (faster array lookup)
        */
        [=](request_gru, actor client, int clientID) {
            self->state.clientList[clientID]->updateLastSeen();

            checkIfDisconnected(self->state.clientList, self->state.GRUsToRepeat, 
                self->state.lastConnectedCheck);
            aout(self) << self->state.lastConnectedCheck << "\n";


            // Check if we have any more GRUs to compute
            if (self->state.numGRUsLeftToCompute <= 0 && self->state.GRUsToRepeat.empty()) {
                /*
                * If No GRUs left to Compute: Set Client status to unconnected
                */
                if(self->state.clientList[clientID]->isClientConnected()) {
                    self->state.clientList[clientID]->disconnectClient();
                    aout(self) << "Telling Client To Exit" << std::endl;
                    self->send(client, time_to_exit_v);
                    self->state.numClientsConnected--;
                }
                 
                /*
                * If there are no clients left to disconnect. We can disconnect
                */
                if (self->state.numClientsConnected == 0) {
                    aout(self) << "EXITING" << std::endl;
                    self->state.end = std::chrono::high_resolution_clock::now();
                    self->state.duration = std::chrono::duration_cast<std::chrono::seconds>
                        (self->state.end - self->state.start).count();
                    
                    aout(self) << "TIME RAN: " << self->state.duration << std::endl;
                    self->quit();
                    return;
                }
            } else {
                /*
                * Else: There are still GRUs left to compute.
                */
                // check that this client is connected
                if (self->state.clientList[clientID]->isClientConnected()) {
                    aout(self) << "Size of GRUsToRepeat = " << self->state.GRUsToRepeat.size() << "\n";
                    // First check if we have any GRUs to repeat
                    if (self->state.GRUsToRepeat.empty()) {
                        aout(self) << "Sending " << self->state.indxCounter << std::endl;
                        self->state.clientList[clientID]->updateCurrentGRU(self->state.indxCounter);
                        self->send(client, self->state.indxCounter);
                        self->state.indxCounter++; 
                    } else {
                        int gru = self->state.GRUsToRepeat.back();
                        aout(self) << "Sending an Uncompleted GRU:" << gru << "\n";
                        self->state.clientList[clientID]->updateCurrentGRU(gru);
                        self->send(client, gru);
                        self->state.GRUsToRepeat.pop_back();
                    } 
                } else {
                    aout(self) << "ERROR: Client with ID = " << clientID << "is disconnected for an unknown reason" << "\n";
                }        
            }

        },

        [=](check_in, int clientID) {
            self->state.clientList[clientID]->updateLastSeen();
            aout(self) << "Client with ID = " << clientID << "Just chekced in \n";
        },

        [=](done_hru, int clientID, int refGRU, double totalDuration, double initDuration, 
            double forcingDuration, double runPhysicsDuration, double writeOutputDuration) {
            
            aout(self) << "HRU DONE:" << refGRU << std::endl;
            self->state.numSuccess++;
            self->state.clientList[clientID]->gruCompleted();
            self->state.clientList[clientID]->updateLastSeen();

            checkIfDisconnected(self->state.clientList, self->state.GRUsToRepeat, 
                self->state.lastConnectedCheck);
            //TODO: Add Gaurd that checks to see if the file exists, Ideally it should be moved after completion of a run
            // Write the output to a file
            std::ofstream timingInfo;
            timingInfo.open("Successful.csv", std::ios_base::app);
            timingInfo << refGRU << "," << totalDuration << "," << initDuration << "," << 
                forcingDuration << "," << runPhysicsDuration << "," << writeOutputDuration << "\n";
            timingInfo.close();
            self->state.numGRUsLeftToCompute--;
            aout(self) << "Passed = " << self->state.numSuccess << "\n";
            aout(self) << "Failed = " << self->state.numFailed << "\n";
            aout(self) << self->state.numGRUsLeftToCompute << " hrus left" << std::endl;
        },

        [=](run_failure, int refGRU, int timestep) {
            self->state.numFailed++;
            std::ofstream timingInfo;
            timingInfo.open("Failures.csv", std::ios_base::app);
            timingInfo << refGRU << "," << timestep << "\n";
            aout(self) << "Passed = " << self->state.numSuccess << "\n";
            aout(self) << "Failed = " << self->state.numFailed << "\n";
            aout(self) << self->state.numGRUsLeftToCompute << " hrus left" << std::endl;
            self->state.numGRUsLeftToCompute--;
        },
    };
}







#endif