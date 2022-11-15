#include "batch/batch.hpp"

Batch::Batch(int batch_id, int start_hru, int num_hru){
    this->batch_id = batch_id;
    this->start_hru = start_hru;
    this->num_hru = num_hru;
    this->assigned_to_actor = false;
}

// Getters
int Batch::getBatchID() {
    return this->batch_id;
}

int Batch::getStartHRU() {
    return this->start_hru;
}

int Batch::getNumHRU() {
    return this->num_hru;
}

bool Batch::getBatchStatus() {
    return this->assigned_to_actor;
}

double Batch::getRunTime() {
    return this->run_time;
}

double Batch::getReadTime() {
    return this->read_time;
}

double Batch::getWriteTime() {
    return this->write_time;
}

// Setters
void Batch::updateRunTime(double run_time) {
    this->run_time = run_time;
}

void Batch::updateReadTime(double read_time) {
    this->read_time = read_time;
}

void Batch::updateWriteTime(double write_time) {
    this->write_time = write_time;
}

void Batch::updateAssignedActor(bool boolean) {
    this->assigned_to_actor = boolean;
}

// general methods
void Batch::assignToActor(std::string hostname, caf::actor assigned_actor) {
    this->hostname = hostname;
    this->assigned_actor = assigned_actor;
    this->assigned_to_actor = true;
}

void Batch::assignBatch(Client *client) {
    this->assigned_client = client;
}

void Batch::printBatchInfo() {
    std::cout << "batch_id: " << this->batch_id << "\n";
    std::cout << "start_hru: " << this->start_hru << "\n";
    std::cout << "num_hru: " << this->num_hru << "\n";
}

std::string Batch::toString() {
    std::stringstream out_string;

    out_string << "batch_id: " << this->batch_id << "\n" <<
                  "start_hru: " << this->start_hru << "\n" <<
                  "num_hru: " << this->num_hru << "\n" <<
                  "run_time: " << this->run_time << "\n" << 
                  "read_time: " << this->read_time << "\n" <<
                  "write_time: " << this->write_time << "\n" <<
                  "assigned_to_actor: " << this->assigned_to_actor << "\n" <<
                  "hostname: " << this->hostname << "\n";

    return out_string.str();
}

void Batch::writeBatchToFile(std::string file_name) {
    std::ofstream output_file;
    output_file.open(file_name, std::ios_base::app);
    output_file <<
        this->batch_id      << "," <<
        this->start_hru     << "," << 
        this->num_hru       << "," << 
        this->hostname      << "," <<
        this->run_time      << "," << 
        this->read_time     << "," <<
        this->write_time    << "\n";
    output_file.close();
}