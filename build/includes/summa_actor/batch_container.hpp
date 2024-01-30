#include "caf/all.hpp"
#pragma once
#include "client.hpp"


class Batch_Container {
  private:
    int start_hru_;  
    int total_hru_count_;
    int num_hru_per_batch_;
    int batches_remaining_;
    std::vector<Batch> batch_list_;
    
    // Assemble the total number of HRUs given by the user into batches.
    void assembleBatches();
    
  public:
        
    // Creating the batch_manager will also create the batches
    // with the two parameters that are passed in. 
    Batch_Container(int start_hru = 1, int total_hru_count = 0, 
                    int num_hru_per_batch = 0);

    // returns the size of the batch list
    int getBatchesRemaining();
    int getTotalBatches();
    
    // Find an unsolved batch, set it to assigned and return it.
    std::optional<Batch> getUnsolvedBatch();

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
    friend bool inspect(Inspector& inspector, Batch_Container& batch_container) {
        return inspector.object(batch_container).fields(
            inspector.field("total_hru_count", batch_container.total_hru_count_),
            inspector.field("num_hru_per_batch", batch_container.num_hru_per_batch_),
            inspector.field("batches_remaining", batch_container.batches_remaining_),
            inspector.field("batch_list", batch_container.batch_list_));
    }


};