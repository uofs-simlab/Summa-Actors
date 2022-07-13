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

void Batch::solvedBatch(double run_time, double read_time, double write_time) {
    this->status = solved;
    this->run_time = run_time;
    this->read_time = read_time;
    this->write_time = write_time;
}

void Batch::assignedBatch(std::string hostname, caf::actor actor_ref) {
    this->status = assigned;
    this->assigned_host = hostname;
    this->assigned_actor = actor_ref;
}

void Batch::updateRunTime(double run_time) {
    this->run_time = run_time;
}

void Batch::writeBatchToFile(std::string file_name) {
    std::ofstream output_file;
    output_file.open(file_name, std::ios_base::app);
    output_file <<
        this->batch_id      << "," <<
        this->start_hru     << "," << 
        this->num_hru       << "," << 
        this->assigned_host << "," <<
        this->run_time      << "," << 
        this->read_time     << "," <<
        this->write_time    << "," <<
        this->status        << "\n";
    output_file.close();
}


