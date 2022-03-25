#ifndef SUMMAMANGER_H_
#define SUMMAMANGER_H_

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "JobActor.h"
#include <iostream>
#include <chrono>
#include <string>
#include "json.hpp"
#include <fstream>
#include "global.h"



struct summa_manager {
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

};

/**
 * @brief Function to spawn a job actor
 */
void spawnJob(stateful_actor<summa_manager>* self);

void parseSettings(stateful_actor<summa_manager>* self, std::string configPath);
#endif