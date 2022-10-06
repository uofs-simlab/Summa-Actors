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
    int count_index = 0;
    int start_hru = 1;

    // while(remaining_hru_to_batch > 0) {
    //     if (num_hru_per_batch > remaining_hru_to_batch) {
    //         batch_list.push_back(Batch(count_index, start_hru, remaining_hru_to_batch));
    //         remaining_hru_to_batch = 0;
    //     } else {
    //         batch_list.push_back(Batch(count_index, start_hru, num_hru_per_batch));
            
    //         remaining_hru_to_batch -= self->state.distributed_settings.num_hru_per_batch;
    //         start_hru += self->state.distributed_settings.num_hru_per_batch;
    //         count_index += 1;
    //     }
    // }
    // return 0;
}



Batch::Batch(int batch_id, int start_hru, int num_hru){
    this->batch_id = batch_id;
    this->start_hru = start_hru;
    this->num_hru = num_hru;
    this->assigned_to_actor = false;
}


// Setters
int Batch::getBatchID() {
    return this->batch_id;
}

bool Batch::getBatchStatus() {
    return this->assigned_to_actor;
}


// void Batch::printBatchInfo() {
//     std::cout << "batch_id: " << this->batch_id << "\n";
//     std::cout << "start_hru: " << this->start_hru << "\n";
//     std::cout << "num_hru: " << this->num_hru << "\n";
// }

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


