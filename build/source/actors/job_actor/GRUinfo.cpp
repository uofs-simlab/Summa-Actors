#include "caf/all.hpp"
#include "GRUinfo.hpp"
#include <iostream>
#include <fstream>



GRU::GRU(int global_gru_index, int local_gru_index, caf::actor gru_actor, int dt_init_factor, int max_attempt) {
  this->global_gru_index = global_gru_index;
  this->local_gru_index = local_gru_index;
  this->gru_actor = gru_actor;
  this->dt_init_factor = dt_init_factor;
  this->attempts_left = max_attempt;
  this->state = gru_state::running;
}
GRU::~GRU() {};

// Getters
int GRU::getGlobalGRUIndex() {
  return this->global_gru_index;
}

double GRU::getRunTime() {
  return this->run_time;
}

double GRU::getInitDuration() {
  return this->init_duration;
}

double GRU::getForcingDuration() {
  return this->forcing_duration;
}

double GRU::getRunPhysicsDuration() {
  return this->run_physics_duration;
}

double GRU::getWriteOutputDuration() {
  return this->write_output_duration;
}

double GRU::getAttemptsLeft() {
  return this->attempts_left;
}

gru_state GRU::getStatus() {
  return this->state;
}

// Setters
void GRU::setRunTime(double run_time) {
  this->run_time = run_time;
}
void GRU::setInitDuration(double init_duration) {
  this->init_duration = init_duration;
}
void GRU::setForcingDuration(double forcing_duration) {
  this->forcing_duration = forcing_duration;
}
void GRU::setRunPhysicsDuration(double run_physics_duration) {
  this->run_physics_duration = run_physics_duration;
}
void GRU::setWriteOutputDuration(double write_output_duration) {
  this->write_output_duration = write_output_duration;
}

void GRU::setSuccess() {
  this->state = gru_state::succeeded;
}





GRUinfo::GRUinfo(int refGRU, int indxGRU, caf::actor gru_actor, int dt_init_factor, int max_attempts) {
  this->refGRU = refGRU;
  this->indxGRU = indxGRU;
  this->GRU = gru_actor;
  this->dt_init = dt_init_factor;
  this->currentAttempt = 1;

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