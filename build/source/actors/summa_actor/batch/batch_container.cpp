#include "batch/batch_container.hpp"

Batch_Container::Batch_Container(int total_hru_count, int num_hru_per_batch) {
    this->total_hru_count = total_hru_count;
    this->num_hru_per_batch = num_hru_per_batch;

    this->assembleBatches(this->total_hru_count, this->num_hru_per_batch);
}

int Batch_Container::getBatchesRemaining() {
    return this->batch_list.size();
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
    std::optional<int> index = this->findBatch(batch_id);
    if (index.has_value()) {
        this->batch_list[index.value()].updateAssignedActor(false);
    } else {
        throw "updateBatchStatus_LostClient - Could not find batch with id";
    }
}


std::optional<Batch> Batch_Container::assignBatch(Client *client) {

    for (std::vector<int>::size_type i = 0; i < this->batch_list.size(); i++) {
        if (!this->batch_list[i].getBatchStatus()) {
            this->batch_list[i].assignBatch(client);
            return this->batch_list[i];
        }
    }
    return {};
}

void Batch_Container::updateBatch_success(Batch successful_batch, std::string output_csv) {
    this->solved_batches.push_back(successful_batch);

    successful_batch.writeBatchToFile(output_csv);
    
    std::optional<int> index_to_remove = this->findBatch(successful_batch.getBatchID());
    if (index_to_remove.has_value()) {
        this->batch_list.erase(this->batch_list.begin() + index_to_remove.value());
    } else {
        throw "No element in BatchList Matches the succesful_batch";
    }
}

std::optional<int> Batch_Container::findBatch(int batch_id) {

    for(std::vector<int>::size_type i = 0; i < this->batch_list.size(); i++) {
        if (this->batch_list[i].getBatchID() == batch_id) {
            return i;
        }
    }

    return {};
}