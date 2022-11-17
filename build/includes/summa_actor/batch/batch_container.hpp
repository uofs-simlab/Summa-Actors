#include "caf/all.hpp"
#pragma once
#include "client/client.hpp"


class Batch_Container {
    private:
        int total_hru_count;
        int num_hru_per_batch;
        int batches_remaining;
        std::vector<Batch> batch_list;

        // Assemble the total number of HRUs given by the user into batches.
        void assembleBatches(int total_hru_count, int num_hru_per_batch);
    
    public:
        
        // Creating the batch_manager will also create the batches
        // with the two parameters that are passed in. 
        Batch_Container(int total_hru_count = 0, int num_hru_per_batch = 0);

        // returns the size of the batch list
        int getBatchesRemaining();
        
        // Find an unsolved batch, set it to assigned and return it.
        std::optional<Batch> getUnsolvedBatch();

        // Update the batch status to solved and write the output to a file.
        void updateBatch_success(Batch successful_batch, std::string output_csv);
        // Update the batch status but do not write the output to a file.
        void updateBatch_success(Batch successful_batch);

        // Update the batch to assigned = true
        void setBatchAssigned(Batch batch);
        // Update the batch to assigned = false
        void setBatchUnassigned(Batch batch);
        
        // Check if there are batches left to solve
        bool hasUnsolvedBatches();

        // TODO: Needs implementation
        void updateBatch_failure(Batch failed_batch);


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

        /**
         * @brief Find the batch with the batch_id parameter
         * update the batches assigned actor member variable to false
         * 
         */
        void updateBatchStatus_LostClient(int batch_id);

        template <class Inspector>
        friend bool inspect(Inspector& inspector, Batch_Container& batch_container) {
            return inspector.object(batch_container).fields(
                inspector.field("total_hru_count", batch_container.total_hru_count),
                inspector.field("num_hru_per_batch", batch_container.num_hru_per_batch),
                inspector.field("batches_remaining", batch_container.batches_remaining),
                inspector.field("batch_list", batch_container.batch_list));
        }
 

};