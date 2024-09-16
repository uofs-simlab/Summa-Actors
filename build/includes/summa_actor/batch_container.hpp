#pragma once
#include "caf/all.hpp"
#include "client.hpp"
#include "logger.hpp"
#include "settings_functions.hpp"


class BatchContainer {
  private:
    // Inital Configuration 
    int id_;
    std::string name_;
    std::string file_manager_;
    int start_gru_;  
    int num_gru_;
    int num_gru_per_batch_;

    // Batch Info
    int batches_remaining_;
    std::vector<Batch> batch_list_;
    void assembleBatches(std::string log_dir);

    // Statistics
    int num_success_;
    int num_failed_;
    double run_time_;
    double read_time_;
    double write_time_;
    
    // Misc
    std::unique_ptr<Logger> logger_;
    Settings settings_;

  public:
    BatchContainer(std::string name = "", std::string file_manager = "", 
        int start_gru = 0, int num_gru = 0, int num_gru_per_batch = 0, 
        Settings settings = Settings());

    std::string toString();
    void printBatches();
    std::string getBatchesAsString();
    

    inline int getBatchesRemaining() {return batches_remaining_;}
    inline int getTotalBatches() { return batch_list_.size();}
    std::optional<Batch> getUnsolvedBatch();


    void updateBatchStats(int batch_id, double run_time, double read_time, 
        double write_time, int num_success, int num_failed);

    // Update the batch status to solved and write the output to a file.
    void updateBatch_success(Batch successful_batch, std::string output_csv, std::string hostname);
    // Update the batch status but do not write the output to a file.
    void updateBatch_success(Batch successful_batch);
    // Update batch by id
    void updateBatch_success(int batch_id, double run_time, double read_time, 
        double write_time);

    // Update the batch to assigned = true
    void setBatchAssigned(Batch batch);
    // Update the batch to assigned = false
    void setBatchUnassigned(Batch batch);
    
    // Check if there are batches left to solve
    bool hasUnsolvedBatches();

    // TODO: Needs implementation
    void updateBatch_failure(Batch failed_batch);

    std::string getAllBatchInfoString();


    double getTotalReadTime();
    double getTotalWriteTime();


    /**
     * A client has found to be disconnected. Unassign all batches
     * that were assigned to the disconnected client. The client id 
     * is passed in as a parameter
     */
    void updatedBatch_disconnectedClient(int client_id);

    /**
     * Create the csv file for the completed batches.
     */
    void inititalizeCSVOutput(std::string csv_output_name);




    /**
     * @brief Find the batch with the batch_id parameter
     * update the batches assigned actor member variable to false
     * 
     */
    void updateBatchStatus_LostClient(int batch_id);

    template <class Inspector>
    friend bool inspect(Inspector& inspector, BatchContainer& bc) {
      return inspector.object(bc).fields(
          // Inital Configuration 
          inspector.field("id", bc.id_),
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