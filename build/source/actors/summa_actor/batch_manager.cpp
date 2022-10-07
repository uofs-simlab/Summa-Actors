#include "caf/all.hpp"
#include <vector>
#include "batch_manager.hpp"


Batch_Container::Batch_Container(int total_hru_count, int num_hru_per_batch) {
    this->total_hru_count = total_hru_count;
    this->num_hru_per_batch = num_hru_per_batch;

    this->assembleBatches(this->total_hru_count, this->num_hru_per_batch);
}

void Batch_Container::assembleBatches(int total_hru_count, int num_hru_per_batch) {
    int remaining_hru_to_batch = total_hru_count;
    int batch_id = 0;
    int start_hru = 1;

    while(remaining_hru_to_batch > 0) {
        if (num_hru_per_batch > remaining_hru_to_batch) {
            this->batch_list.push_back(Batch(batch_id, start_hru, remaining_hru_to_batch));
            remaining_hru_to_batch = 0;
        } else {
            this->batch_list.push_back(Batch(batch_id, start_hru, num_hru_per_batch));
            
            remaining_hru_to_batch -= num_hru_per_batch;
            start_hru += num_hru_per_batch;
            batch_id += 1;
        }
    }
}

void Batch_Container::printBatches() {
    for (std::vector<int>::size_type i = 0; i < this->batch_list.size(); i++) {
        this->batch_list[i].printBatchInfo();
    }
}


std::optional<Batch> Batch_Container::assignBatch(std::string hostname, caf::actor actor_ref) {

    for (std::vector<int>::size_type i = 0; i < this->batch_list.size(); i++) {
        if (!this->batch_list[i].getBatchStatus()) {
            this->batch_list[i].assignToActor(hostname, actor_ref);
            return this->batch_list[i];
        }
    }
    return {};
}


// **************************
// Batch Class
// **************************

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

void Batch::assignToActor(std::string hostname, caf::actor assigned_actor) {
    this->hostname = hostname;
    this->assigned_actor = assigned_actor;
    this->assigned_to_actor = true;
}

void Batch::printBatchInfo() {
    std::cout << "batch_id: " << this->batch_id << "\n";
    std::cout << "start_hru: " << this->start_hru << "\n";
    std::cout << "num_hru: " << this->num_hru << "\n";
}

// batch_status Batch::getBatchStatus() {
//     return this->status;
// }

// int Batch::getBatchID() {
//     return this->batch_id;
// }

// int Batch::getStartHRU() {
//     return this->start_hru;
// }

// int Batch::getNumHRU() {
//     return this->num_hru;
// }

// void Batch::solvedBatch(double run_time, double read_time, double write_time) {
//     this->status = solved;
//     this->run_time = run_time;
//     this->read_time = read_time;
//     this->write_time = write_time;
// }

// void Batch::assignedBatch(std::string hostname, caf::actor actor_ref) {
//     this->status = assigned;
//     this->assigned_host = hostname;
//     this->assigned_actor = actor_ref;
// }

// void Batch::updateRunTime(double run_time) {
//     this->run_time = run_time;
// }

// void Batch::writeBatchToFile(std::string file_name) {
//     std::ofstream output_file;
//     output_file.open(file_name, std::ios_base::app);
//     output_file <<
//         this->batch_id      << "," <<
//         this->start_hru     << "," << 
//         this->num_hru       << "," << 
//         this->assigned_host << "," <<
//         this->run_time      << "," << 
//         this->read_time     << "," <<
//         this->write_time    << "," <<
//         this->status        << "\n";
//     output_file.close();
// }


