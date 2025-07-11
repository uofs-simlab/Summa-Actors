#include "dynamic_batch_container.hpp"

DynamicBatchContainer::DynamicBatchContainer(int start_hru,
                                             int total_hru_count,
                                             std::string log_dir)
  : BatchContainer(start_hru, 0, 0, log_dir),  // Do not pre-assemble batches
    next_start_hru_(start_hru),
    remaining_hrus_(total_hru_count),
    next_batch_id_(0) {}
std::optional<Batch> DynamicBatchContainer::getUnsolvedBatch() {
  // First try existing unsolved batches from base class
  auto fallback = BatchContainer::getUnsolvedBatch();
  if (fallback.has_value()) {
    return fallback;
  }
  return getUnsolvedBatch(default_size_);
}

std::optional<Batch> DynamicBatchContainer::getUnsolvedBatch(int size) {
  // Check again for existing unsolved ones
  auto fallback = BatchContainer::getUnsolvedBatch();
  if (fallback.has_value()) {
    return fallback;
  }

  if (remaining_hrus_ <= 0)
    return {};

  int take = std::min(size, remaining_hrus_);
  Batch new_batch(next_batch_id_, next_start_hru_, take);
  batch_list_.push_back(new_batch);
  batch_list_[next_batch_id_].updateAssigned(true);
  logger_->log("Dynamic: Starting batch " + std::to_string(next_batch_id_));
  ++next_batch_id_;
  next_start_hru_ += take;
  remaining_hrus_ -= take;
  return batch_list_.back();
}


int DynamicBatchContainer::getBatchesRemaining() const {
  return remaining_hrus_ > 0 ? 1 : 0;
}
