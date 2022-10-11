#include "batch_manager.hpp"


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


std::optional<Batch> Batch_Container::assignBatch(std::string hostname, caf::actor actor_ref) {

    for (std::vector<int>::size_type i = 0; i < this->batch_list.size(); i++) {
        if (!this->batch_list[i].getBatchStatus()) {
            this->batch_list[i].assignToActor(hostname, actor_ref);
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


