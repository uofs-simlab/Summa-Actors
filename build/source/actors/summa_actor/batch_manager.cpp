#include "caf/all.hpp"
#include <vector>
#include "batch_manager.hpp"

Batch::Batch(int batch_id, int start_hru, int num_hru) {
    this->batch_id = batch_id;
    this->start_hru = start_hru;
    this->num_hru = num_hru;
    this->status = unassigned;
}



void Batch::printBatchInfo() {
    std::cout << "batch_id: " << this->batch_id << "\n";
    std::cout << "start_hru: " << this->start_hru << "\n";
    std::cout << "num_hru: " << this->num_hru << "\n";
}

batch_status Batch::getBatchStatus() {
    return this->status;
}

int Batch::getBatchID() {
    return this->batch_id;
}

int Batch::getStartHRU() {
    return this->start_hru;
}

int Batch::getNumHRU() {
    return this->num_hru;
}

void Batch::solvedBatch() {
    this->status = solved;
}

void Batch::assignedBatch() {
    this->status = assigned;
}

void Batch::updateRunTime(double run_time) {
    this->run_time = run_time;
}


