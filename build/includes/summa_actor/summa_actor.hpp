#pragma once

#include "caf/all.hpp"
#include "caf/io/all.hpp"

#include <chrono>
#include <string>

namespace caf {

struct summa_actor_state {
    // Timing Information
    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;
    double duration;
    // Program Parameters
    int startGRU;           // starting GRU for the simulation
    int numGRU;             // number of GRUs to compute
    std::string configPath;// path to the fileManager.txt file
    // Information about the jobs
    int numFailed = 0;      // Number of jobs that have failed

    // Values Set By Summa_Actors_Settings.json
    int maxGRUPerJob; // maximum number of GRUs a job can compute at once
    int outputStrucSize; 

    caf::actor currentJob;  // Reference to the current job actor
    caf::actor parent;

};

behavior summa_actor(stateful_actor<summa_actor_state>* self, int startGRU, int numGRU, std::string configPath, actor parent);

void spawnJob(stateful_actor<summa_actor_state>* self);

void parseSettings(stateful_actor<summa_actor_state>* self, std::string configPath);



} // namespace caf