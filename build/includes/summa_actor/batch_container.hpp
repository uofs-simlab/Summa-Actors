#pragma once
#include "caf/all.hpp"
#include "client.hpp"
#include "logger.hpp"


class BatchContainer {
  private:
    int start_hru_;  
    int total_hru_count_;
    int num_hru_per_batch_;
    int batches_remaining_;
    std::vector<Batch> batch_list_;
    std::unique_ptr<Logger> logger_;
    
    void assembleBatches(std::string log_dir);
    
  public:
        
    // Initialize BatchContainer -- call assembleBatches() 
    BatchContainer(int start_hru = 1, int total_hru_count = 0, 
                   int num_hru_per_batch = 0, std::string log_dir = "");


    // returns the size of the batch list
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
     * @brief Print the batches from the batch list
     * 
     */
    void printBatches();
    std::string getBatchesAsString();

    /**
     * @brief Find the batch with the batch_id parameter
     * update the batches assigned actor member variable to false
     * 
     */
    void updateBatchStatus_LostClient(int batch_id);

    template <class Inspector>
    friend bool inspect(Inspector& inspector, BatchContainer& batch_container) {
        return inspector.object(batch_container).fields(
               inspector.field("total_hru_count", 
                               batch_container.total_hru_count_),
               inspector.field("num_hru_per_batch", 
                               batch_container.num_hru_per_batch_),
               inspector.field("batches_remaining", 
                               batch_container.batches_remaining_),
               inspector.field("batch_list", batch_container.batch_list_),
               inspector.field("logger", batch_container.logger_));
    }


};