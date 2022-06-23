#include "caf/all.hpp"
#include <vector>
#include "batch_manager.hpp"

Batch::Batch(int batch_id, int start_hru, int num_hru) {
    this->batch_id = batch_id;
    this->start_hru = start_hru;
    this->num_hru = num_hru;
}

void Batch::printBatchInfo() {
    std::cout << "batch_id: " << this->batch_id << "\n";
    std::cout << "start_hru: " << this->start_hru << "\n";
    std::cout << "num_hru: " << this->num_hru << "\n";
}