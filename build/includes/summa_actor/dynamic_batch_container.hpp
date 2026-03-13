#pragma once
#include "batch_container.hpp"
#include <optional>

/**
 * DynamicBatchContainer inherits from BatchContainer but generates batches on the fly.
 * It does not pre-assemble all batches: instead, each call to getUnsolvedBatch(size)
 * takes the next `size` HRUs and pushes a new Batch onto the underlying list.
 */
class DynamicBatchContainer : public BatchContainer {
public:
  DynamicBatchContainer(int start_hru = 1,
                        int total_hru_count = 0,
                        std::string log_dir = "");

  /// Default-size overload (1000 HRUs)
  std::optional<Batch> getUnsolvedBatch();

  /// Dynamically assemble and return the next batch of up to `size` HRUs.
  std::optional<Batch> getUnsolvedBatch(int size);

  /// Override to report dynamic remaining batch count
  int getBatchesRemaining() const;

private:
  int next_start_hru_;
  int remaining_hrus_;
  int next_batch_id_ = 0;
  static constexpr int default_size_ = 1000;
};
