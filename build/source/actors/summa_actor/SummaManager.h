#ifndef SUMMAMANGER_H_
#define SUMMAMANGER_H_

#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "../job_actor/JobActor.h"
#include "../global/json.hpp"
#include "../global/global.h"


#include <iostream>
#include <chrono>
#include <string>
#include <fstream>




behavior summa_actor(stateful_actor<summa_manager>* self, int startGRU, int numGRU, std::string configPath);

void spawnJob(stateful_actor<summa_manager>* self);

void parseSettings(stateful_actor<summa_manager>* self, std::string configPath);



#endif