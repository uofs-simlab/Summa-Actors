#ifndef GRUInfo_H_
#define GRUInfo_H_


#include "caf/all.hpp"
#include "../messageAtoms.h"

#define max_failures 3


enum State {Ready, Running, Done, Fail};
/**
* Information about the GRU and its status {Ready, Running, Done, Fail}, as well as other useful information
* that may be needed
**/
class GRUInfo {
    private:
        int refGRU;                     // The GRUID of the Gru being solved
        State state;                    // State this HRU is currently in
        caf::actor client;              // Client Actor in Charge of GRU solvers
        int duration;                   // in seconds
        int initDuration;               // in seconds    
        int forcingDuration;            // in seconds
        int runPhysicsDuration;         // in seconds
        int writeOutputDuration;        // in seconds
        int numFailures;


    public:
        GRUInfo(int refGRU) {
            this->refGRU = refGRU;
            this->state = Ready;
            this->numFailures = 0;
        };

        /**
        * Method Called when GRU is finished
        **/
        void GRU_Done(int duration, int initDuration, int forcingDuration, 
            int runPhysicsDuration, int writeOutputDuration) {
            
            // Update Timing Information for GRU
            this->duration = duration;
            this->initDuration = forcingDuration;
            this->forcingDuration = forcingDuration;
            this->runPhysicsDuration = runPhysicsDuration;
            this->writeOutputDuration = writeOutputDuration;
            // Update the state of the GRU
            this->state = Done;
        };

        // Checks to see if we hit the failure threshold
        int GRU_Start(caf::actor client) {
            this->client = client;
            if(this->numFailures == max_failures) {
                return -1;
            }
            return 0;
        };

        void GRU_Fail() {
            this->numFailures++;
        };

        int getRefGRU() {
            return this->refGRU;
        };

};

#endif