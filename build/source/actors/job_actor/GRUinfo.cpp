#include "caf/all.hpp"
#include "GRUinfo.hpp"
#include <iostream>
#include <fstream>


GRUinfo::GRUinfo(int refGRU, int indxGRU, caf::actor gru, int dt_init, int maxAttempts) {
  this->refGRU = refGRU;
  this->indxGRU = indxGRU;
  this->GRU = gru;
  this->dt_init = dt_init;
  this->currentAttempt = 1;
  this->maxAttempts = maxAttempts;
  this->completed = false;
  this->failed = false;
}
GRUinfo::~GRUinfo(){};

// Getters
int GRUinfo::getRefGRU() {
  return this->refGRU;
}

int GRUinfo::getIndxGRU() {
  return this->indxGRU;
}

int GRUinfo::getDt_init() {
  return this->dt_init;
}

caf::actor GRUinfo::getActor() {
  return GRU;
}
// Setters
void GRUinfo::updateGRU(caf::actor gru) {
  this->GRU = gru;
}

void GRUinfo::updateFailed() {
  if (this->failed) {
    this->failed = false;
  } else {
    this->failed = true;
  }
}

void GRUinfo::updateCompletedToTrue(){
  this->completed = true;
}

void GRUinfo::updateDt_init() {
  this->dt_init = this->dt_init * 2;
}

void GRUinfo::updateCurrentAttempt() {
  this->currentAttempt++;
}

// Methods that return Booleans
bool GRUinfo::isMaxAttemptsReached() {
  return this->maxAttempts <= this->currentAttempt;
}

bool GRUinfo::isFailed() {
  return this->failed;
}

bool GRUinfo::isCompleted() {
  return this->completed;
}

void GRUinfo::doneRun(double runTime, double initDuration, double forcingDuration,
  double runPhysicsDuration, double writeOutputDuration) {
    this->runTime = runTime;
    this->initDuration = initDuration;
    this->forcingDuration = forcingDuration;
    this->runPhysicsDuration = runPhysicsDuration;
    this->writeOutputDuration = writeOutputDuration;
}

// Methods for writing statistics to a file
void GRUinfo::writeSuccess(std::string fileName, std::string hostname) {
  std::ofstream file;
  file.open(fileName, std::ios_base::app);
  file  << hostname << ","
        << this->refGRU << "," 
        << this->runTime << "," 
        << this->initDuration << "," 
        << this->forcingDuration << "," 
        << this->runPhysicsDuration << "," 
        << this->writeOutputDuration << "," 
        << this->dt_init << "," 
        << this->currentAttempt << "\n";
  file.close();
}

void GRUinfo::writeFail(std::string fileName) {
  std::ofstream file;
  file.open(fileName, std::ios_base::app);
  file << this->refGRU << ","
        << this->dt_init << ","
        << this->currentAttempt << "\n";
  file.close();
}

void GRUinfo::printOutput() {
  std::cout << "\nGRU = " << this->refGRU << "\n" <<
    "RunTime = " << this->runTime << "\n" << 
    "initDuration = " << this->initDuration << "\n" << 
    "forcingDuration = " << this->forcingDuration << "\n" <<
    "runPhysicsDuration = " << this->runPhysicsDuration << "\n" << 
    "writeOutputDuration = " << this->writeOutputDuration << "\n\n"; 
}