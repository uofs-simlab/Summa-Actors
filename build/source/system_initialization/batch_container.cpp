#include "batch_container.hpp"

Batch_Container::Batch_Container(int start_hru, int total_hru_count, 
                                 int num_hru_per_batch,
                                 std::string log_dir) {
  start_hru_ = start_hru;
  total_hru_count_ = total_hru_count;
  num_hru_per_batch_ = num_hru_per_batch;
  if (!log_dir.empty() && log_dir.back() != '/') {
    log_dir += "/"; // Ensure log_dir_ is a directory
  }
  assembleBatches(log_dir);
  batches_remaining_ = batch_list_.size();
  logger_ = Logger(log_dir + "batch_container.log");
  logger_.log("----------------Batch List----------------");
  logger_.log(this->getBatchesAsString());
  logger_.log("------------------------------------------");
}

void Batch_Container::assembleBatches(std::string log_dir) {
  int remaining_hru_to_batch = total_hru_count_;
  int batch_id = 0;
  int start_hru_local = start_hru_;

  while (remaining_hru_to_batch > 0) {
    int current_batch_size = std::min(num_hru_per_batch_, 
                                      remaining_hru_to_batch);
    batch_list_.push_back(Batch(batch_id, start_hru_local, current_batch_size));
    batch_list_[batch_id].setLogDir(log_dir);

    remaining_hru_to_batch -= current_batch_size;
    start_hru_local += current_batch_size;
    if (current_batch_size == num_hru_per_batch_)
      batch_id += 1;
  }
}

void Batch_Container::updateBatchStats(int batch_id, double run_time, 
                                       double read_time, double write_time,
                                       int num_success, int num_failed) {
  batch_list_[batch_id].updateRunTime(run_time);
  batch_list_[batch_id].updateReadTime(read_time);
  batch_list_[batch_id].updateWriteTime(write_time);
  batch_list_[batch_id].updateSolved(true);
  batches_remaining_--;
  logger_.log("Batch " + std::to_string(batch_id) + " Solved");
  logger_.log("\tRun Time: " + std::to_string(run_time));
  logger_.log("\tRead Time: " + std::to_string(read_time));
  logger_.log("\tWrite Time: " + std::to_string(write_time));
  logger_.log("\tNum Success: " + std::to_string(num_success));
  logger_.log("\tNum Failed: " + std::to_string(num_failed));
  logger_.log("End");
}

void Batch_Container::printBatches() {
  for (auto& batch : batch_list_) {
    batch.printBatchInfo();
  }
}

std::string Batch_Container::getBatchesAsString() {
  std::string out_string = "";
  for (auto& batch : batch_list_) {
    out_string += batch.getBatchInfoString();
  }
  return out_string;
}

void Batch_Container::updateBatchStatus_LostClient(int batch_id) {
  batch_list_[batch_id].updateAssigned(false);
}

std::optional<Batch> Batch_Container::getUnsolvedBatch() {
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

void Batch_Container::setBatchAssigned(Batch batch) {
  batch_list_[batch.getBatchID()].updateAssigned(true);
}

void Batch_Container::setBatchUnassigned(Batch batch) {
  batch_list_[batch.getBatchID()].updateAssigned(false);
}

void Batch_Container::updateBatch_success(Batch successful_batch, 
                                          std::string output_csv, 
                                          std::string hostname) {
  successful_batch.writeBatchToFile(output_csv, hostname);
  batch_list_[successful_batch.getBatchID()].updateSolved(true);
  batches_remaining_--;
}





void Batch_Container::updateBatch_success(Batch successful_batch) {
  batch_list_[successful_batch.getBatchID()].updateSolved(true);
  batches_remaining_--;
}

bool Batch_Container::hasUnsolvedBatches() { return batches_remaining_ > 0;}


std::string Batch_Container::getAllBatchInfoString() {
  std::string out_string = "";
  for (auto& batch : batch_list_) {
    out_string += "_____________________________\n";
    out_string += batch.toString();
    out_string += "_____________________________\n";
  }
  return out_string;
}

double Batch_Container::getTotalReadTime() {
  double total_read_time = 0.0;
  for (auto& batch : batch_list_) {
    total_read_time += batch.getReadTime();
  }
  return total_read_time;
}

double Batch_Container::getTotalWriteTime() {
  double total_write_time = 0.0;
  for (auto& batch : batch_list_) {
    total_write_time += batch.getWriteTime();
  }
  return total_write_time;
}



