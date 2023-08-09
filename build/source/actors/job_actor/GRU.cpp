#include "caf/all.hpp"
#include "GRU.hpp"
#include <iostream>
#include <fstream>



GRU::GRU(int global_gru_index, int local_gru_index, caf::actor gru_actor, 
         int dt_init_factor, double rel_tol, double abs_tol, int max_attempt) {
  this->global_gru_index = global_gru_index;
  this->local_gru_index = local_gru_index;
  this->gru_actor = gru_actor;
  this->dt_init_factor = dt_init_factor;
  this->rel_tol = rel_tol;
  this->abs_tol = abs_tol;
  this->attempts_left = max_attempt;
  this->state = gru_state::running;
}
GRU::~GRU() {};

// Getters
int GRU::getGlobalGRUIndex() {
  return this->global_gru_index;
}

int GRU::getLocalGRUIndex() {
  return this->local_gru_index;
}

caf::actor GRU::getGRUActor() {
  return this->gru_actor;
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

double GRU::getRelTol() {
  return this->rel_tol;
}

double GRU::getAbsTol() {
  return this->abs_tol;
}

double GRU::getAttemptsLeft() {
  return this->attempts_left;
}

gru_state GRU::getStatus() {
  return this->state;
}

bool GRU::isFailed() {
  return this->state == gru_state::failed;
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

void GRU::setRelTol(double rel_tol) {
  this->rel_tol = rel_tol;
}
void GRU::setAbsTol(double abs_tol) {
  this->abs_tol = abs_tol;
}

void GRU::setSuccess() {
  this->state = gru_state::succeeded;
}
void GRU::setFailed() {
  this->state = gru_state::failed;
}
void GRU::setRunning() {
  this->state = gru_state::running;
}

void GRU::decrementAttemptsLeft() {
  this->attempts_left--;
}

void GRU::setGRUActor(caf::actor gru_actor) {
  this->gru_actor = gru_actor;
}