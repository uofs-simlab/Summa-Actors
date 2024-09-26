#pragma once
#include "caf/all.hpp"
#include "client.hpp"
#include "logger.hpp"
#include "settings_functions.hpp"


class BatchContainer {
  private:
    // Inital Configuration 
    std::string name_;
    std::string file_manager_;
    int start_gru_;  
    int num_gru_;
    int num_gru_per_batch_;

    // Batch Info
    int batches_remaining_;
    std::vector<Batch> batch_list_;
    std::string state_file_;

    // Statistics
    int num_success_;
    int num_failed_;
    double run_time_;
    double read_time_;
    double write_time_;
    
    // Misc
    Logger logger_;
    Settings settings_;

    // Private Methods
    void assembleBatches(std::string log_dir);
  
  public:
    BatchContainer(std::string name = "", std::string file_manager = "", 
        int start_gru = 0, int num_gru = 0, int num_gru_per_batch = 0, 
        Settings settings = Settings());

    // Move constructor
    BatchContainer(BatchContainer&& other) noexcept :
        name_(std::move(other.name_)),
        file_manager_(std::move(other.file_manager_)),
        start_gru_(other.start_gru_),
        num_gru_(other.num_gru_),
        num_gru_per_batch_(other.num_gru_per_batch_),
        settings_(std::move(other.settings_)),
        logger_(std::move(other.logger_)),
        batch_list_(std::move(other.batch_list_)),
        batches_remaining_(other.batches_remaining_) {
          // Reset the moved-from object
          other.start_gru_ = 0;
          other.num_gru_ = 0;
          other.num_gru_per_batch_ = 0;
          other.batches_remaining_ = 0;
        }

    // Move assignment operator
    BatchContainer& operator=(BatchContainer&& other) noexcept {
      if (this != &other) {
        // Transfer resources
        name_ = std::move(other.name_);
        file_manager_ = std::move(other.file_manager_);
        start_gru_ = other.start_gru_;
        num_gru_ = other.num_gru_;
        num_gru_per_batch_ = other.num_gru_per_batch_;
        settings_ = std::move(other.settings_);
        logger_ = std::move(other.logger_);
        batch_list_ = std::move(other.batch_list_);
        batches_remaining_ = other.batches_remaining_;

        // Reset the moved-from object
        other.start_gru_ = 0;
        other.num_gru_ = 0;
        other.num_gru_per_batch_ = 0;
        other.batches_remaining_ = 0;
      }
      return *this;
    }

    // Implementation of the copy constructor
    BatchContainer(const BatchContainer& other) :
        name_(other.name_),
        file_manager_(other.file_manager_),
        start_gru_(other.start_gru_),
        num_gru_(other.num_gru_),
        num_gru_per_batch_(other.num_gru_per_batch_),
        settings_(other.settings_),
        logger_(other.logger_),
        batch_list_(other.batch_list_),
        batches_remaining_(other.batches_remaining_) {}

    // Implementation of the copy assignment operator
    BatchContainer& operator=(const BatchContainer& other) {
      if (this != &other) {
        name_ = other.name_;
        file_manager_ = other.file_manager_;
        start_gru_ = other.start_gru_;
        num_gru_ = other.num_gru_;
        num_gru_per_batch_ = other.num_gru_per_batch_;
        settings_ = other.settings_;
        logger_ = other.logger_;
        batch_list_ = other.batch_list_;
        batches_remaining_ = other.batches_remaining_;
      }
      return *this;
    }


    // ####################################################################
    //                              Getters
    // ####################################################################
    inline const std::string getName() { return name_; }
    inline const int getBatchesRemaining() {return batches_remaining_; }
    inline const int getTotalBatches() { return batch_list_.size(); }
    std::optional<Batch> getUnsolvedBatch();
    std::string getBatchesAsString();
    std::string getAllBatchInfoString();
    double getTotalReadTime();
    double getTotalWriteTime();
    // ####################################################################
    //                              Setters
    // ####################################################################
    void setBatchAssigned(Batch batch);
    void setBatchUnassigned(Batch batch);
    // ####################################################################
    //                              Methods
    // ####################################################################
    void createStateFile(); 
    std::string toString();
    void printBatches();    
    void updateBatch(Batch batch);
    bool hasUnsolvedBatches();

    template <class Inspector>
    friend bool inspect(Inspector& inspector, BatchContainer& bc) {
      return inspector.object(bc).fields(
          // Inital Configuration 
          inspector.field("name", bc.name_),
          inspector.field("file_manager", bc.file_manager_),
          inspector.field("start_gru", bc.start_gru_),
          inspector.field("num_gru", bc.num_gru_),
          inspector.field("num_gru_per_batch", bc.num_gru_per_batch_),
          // Batch Info
          inspector.field("batches_remaining", bc.batches_remaining_),
          inspector.field("batch_list", bc.batch_list_),
          // Statistics
          inspector.field("num_success", bc.num_success_),
          inspector.field("num_failed", bc.num_failed_),
          inspector.field("run_time", bc.run_time_),
          inspector.field("read_time", bc.read_time_),
          inspector.field("write_time", bc.write_time_),
          // Misc
          inspector.field("settings", bc.settings_),
          inspector.field("logger", bc.logger_));
    }
};