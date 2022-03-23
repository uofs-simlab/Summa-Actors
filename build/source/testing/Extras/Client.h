#ifndef SUMMACLIENT_H_
#define SUMMACLIENT_H_

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "string.h"
#include <unistd.h>

#include "../messageAtoms.h"

struct summa_client_state{
    int ID;     // ID for array access by coordinator actor
    strong_actor_ptr coordinator_actor;  // This is the Actor Coordinating jobs, it talks to jobs through a Jobs Client Actor
    strong_actor_ptr job_actor;          // This actors direct parent, reponsible for creating, and running GRU/HRU actors
    std::string fileManager;

    const int numRetryAttempts = 2; // number of attempts for an HRU to try and compute
    int currentAttempt; // the current attempt we are on
};

// ********************************************************************************
// ** Funciton Prototypes
// ********************************************************************************
behavior unconnected(stateful_actor<summa_client_state>*);
void connecting(stateful_actor<summa_client_state>*, const std::string& host, uint16_t port);
behavior running(stateful_actor<summa_client_state>*, const actor& summa_coordinator);

#endif
