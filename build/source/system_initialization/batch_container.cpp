#include "batch_container.hpp"

// ####################################################################
//                              Constructors
// ####################################################################
BatchContainer::BatchContainer(std::string name, std::string file_manager, 
    int start_gru, int num_gru, int num_gru_per_batch, Settings settings) : 
    name_(name), file_manager_(file_manager), start_gru_(start_gru),
    num_gru_(num_gru), num_gru_per_batch_(num_gru_per_batch),
    settings_(settings) {
  
  std::string log_dir = "";
  if (!log_dir.empty() && log_dir.back() != '/') {
    log_dir += "/"; // Ensure log_dir_ is a directory
  }
  assembleBatches(log_dir);
  batches_remaining_ = batch_list_.size();

  // Initialize logger
  if (!log_dir.empty()) {
    logger_ = Logger(log_dir + "batch_container");
    logger_.log("----------------Batch List----------------");
    logger_.log(this->getBatchesAsString());
    logger_.log("------------------------------------------");
  } else {
    logger_ = Logger("");
  }
}

// ####################################################################
//                              Private Methods
// ####################################################################
void BatchContainer::assembleBatches(std::string log_dir) {
  int remaining_gru_to_batch = num_gru_;
  int batch_id = 0;
  int start_gru_local = start_gru_;

  while (remaining_gru_to_batch > 0) {
    int current_batch_size = std::min(num_gru_per_batch_, 
                                      remaining_gru_to_batch);
    batch_list_.push_back(Batch(name_, batch_id, file_manager_, start_gru_local, 
        current_batch_size));
    batch_list_[batch_id].setLogDir(log_dir);

    remaining_gru_to_batch -= current_batch_size;
    start_gru_local += current_batch_size;
    if (current_batch_size == num_gru_per_batch_)
      batch_id += 1;
  }
}

// ####################################################################
//                              Getters
// ####################################################################
std::string BatchContainer::getBatchesAsString() {
  std::string out_string = "";
  for (auto& batch : batch_list_) {
    out_string += batch.toString();
  }
  return out_string;
}

std::optional<Batch> BatchContainer::getUnsolvedBatch() {
  for (auto& batch : batch_list_) {
    if (!batch.isAssigned() && !batch.isSolved()) {
      batch.updateAssigned(true);
      logger_.log("Starting Batch " + std::to_string(batch.getBatchID()));
      return batch;
    }
  }
  logger_.log("ERROR--Batch_Container: No Unsolved Batches");
  return {};
}

std::string BatchContainer::getAllBatchInfoString() {
  std::string out_string = "";
  for (auto& batch : batch_list_) {
    out_string += "_____________________________\n";
    out_string += batch.toString();
    out_string += "_____________________________\n";
  }
  return out_string;
}

double BatchContainer::getTotalReadTime() {
  double total_read_time = 0.0;
  for (auto& batch : batch_list_) {
    total_read_time += batch.getReadTime();
  }
  return total_read_time;
}

double BatchContainer::getTotalWriteTime() {
  double total_write_time = 0.0;
  for (auto& batch : batch_list_) {
    total_write_time += batch.getWriteTime();
  }
  return total_write_time;
}

// ####################################################################
//                              Setters
// ####################################################################
void BatchContainer::setBatchAssigned(Batch batch) {
  batch_list_[batch.getBatchID()].updateAssigned(true);
}

void BatchContainer::setBatchUnassigned(Batch batch) {
  batch_list_[batch.getBatchID()].updateAssigned(false);
}

// ####################################################################
//                          Public Methods
// ####################################################################
std::string BatchContainer::toString() {
  std::string out_string = "BatchContainer: " + name_ + "\n";
    out_string += "File Manager: " + file_manager_ + "\n";
    out_string += "Start GRU: " + std::to_string(start_gru_) + "\n";
    out_string += "Num GRU: " + std::to_string(num_gru_) + "\n";
    out_string += "Num GRU Per Batch: "+std::to_string(num_gru_per_batch_)+"\n";
    out_string += "Batches Remaining: "+std::to_string(batches_remaining_)+"\n";
  return out_string;
}

void BatchContainer::updateBatch(Batch batch) {
  batch_list_[batch.getBatchID()] = batch;
  batch_list_[batch.getBatchID()].updateSolved(true);
  batches_remaining_--;
}

bool BatchContainer::hasUnsolvedBatches() { return batches_remaining_ > 0;}

void BatchContainer::createStateFile() {
  std::string state_file_ = name_ + "_state.csv";
  
  std::ofstream file;
  file.open(state_file_, std::ios::out);
  file << "batch_id,start_gru,num_gru,assigned,solved\n";
  
  for (auto& batch : batch_list_) {
    file << batch.getBatchID() << "," << batch.getStartGru() << "," 
         << batch.getNumGru() << "," << batch.isAssigned() << "," 
         << batch.isSolved() << "\n";
  }
  file.close();  
}





