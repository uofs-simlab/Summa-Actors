#include "batch/batch_container.hpp"

Batch_Container::Batch_Container(int total_hru_count, int num_hru_per_batch) {
    this->total_hru_count = total_hru_count;
    this->num_hru_per_batch = num_hru_per_batch;
    this->assembleBatches(this->total_hru_count, this->num_hru_per_batch);
    this->batches_remaining = this->batch_list.size(); // batch_list set in assemble batches
}

int Batch_Container::getBatchesRemaining() {
    return this->batches_remaining;
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

void Batch_Container::updateBatchStatus_LostClient(int batch_id) {
    this->batch_list[batch_id].updateAssigned(false);
}

std::optional<Batch> Batch_Container::getUnsolvedBatch() {
    for (std::vector<int>::size_type i = 0; i < this->batch_list.size(); i++) {
        if (!this->batch_list[i].isAssigned() && !this->batch_list[i].isSolved()) {
            this->batch_list[i].updateAssigned(true);
            return this->batch_list[i];
        }
    }
    return {};
}

void Batch_Container::setBatchAssigned(Batch batch) {
    this->batch_list[batch.getBatchID()].updateAssigned(true);
}

void Batch_Container::setBatchUnassigned(Batch batch) {
    this->batch_list[batch.getBatchID()].updateAssigned(false);
}

void Batch_Container::updateBatch_success(Batch successful_batch, std::string output_csv, std::string hostname) {
    int batch_id = successful_batch.getBatchID();
    successful_batch.writeBatchToFile(output_csv, hostname);
    this->batch_list[batch_id].updateSolved(true);
    this->batches_remaining--;
}

void Batch_Container::updateBatch_success(Batch successful_batch) {
    int batch_id = successful_batch.getBatchID();
    this->batch_list[batch_id].updateSolved(true);
    this->batches_remaining--;
}


bool Batch_Container::hasUnsolvedBatches() {
    return this->batches_remaining > 0;
}
