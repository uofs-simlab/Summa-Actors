#pragma once
#include "caf/all.hpp"
#include "client/client.hpp"


class Batch_Container {
    private:
        int total_hru_count;
        int num_hru_per_batch;
        int batches_remaining;
        std::vector<Batch> batch_list;

        // Assemble the total number of HRUs given by the user into batches.
        void assembleBatches(int total_hru_count, int num_hru_per_batch);

        // Find a batch by its id, 
        // return its index in the vector 
        std::optional<int> findBatch(int batch_id);
    
    public:
        
        // Creating the batch_manager will also create the batches
        // with the two parameters that are passed in. 
        Batch_Container(int total_hru_count = 0, int num_hru_per_batch = 0);

    
        // returns the size of the batch list
        int getBatchesRemaining();
        
        /**
         * Assign a batch to be solved by a client.
         * The hostname and the actor_ref of the client solving this batch
         * are added to the client for the servers awareness
         * The batch is then returned by this method and sent to the respective client
         */
        std::optional<Batch> assignBatch();

        /**
         * On a successful batch we take the batch given to us by the client 
         * and add it to our solved_batches list.
         *  
         * We can then remove the batch from the global batch list.
         */
        void updateBatch_success(Batch successful_batch, std::string output_csv);

        /**
         * A batch failure is returned to us by the client
         * This is for when a client failed to solve the batch.
         */
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
                inspector.field("batch_list", batch_container.batch_list));
        }
 

};