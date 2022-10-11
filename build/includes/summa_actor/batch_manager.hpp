#pragma once
#include "caf/all.hpp"
#include <vector>
#include <string>
#include <sstream>
#include <optional>

class Batch;

class Batch {
    private:
        int batch_id;
        int start_hru;
        int num_hru;

        double run_time;
        double read_time;
        double write_time;
        
        bool assigned_to_actor;
        std::string hostname;
        caf::actor assigned_actor;

     public:
        Batch(int batch_id = -1, int start_hru = -1, int num_hru = -1);

        // Gettiners
        int getBatchID();
        int getStartHRU();
        int getNumHRU();
        double getRunTime();
        double getReadTime();
        double getWriteTime();
        bool getBatchStatus();

        // Setters
        void updateRunTime(double run_time);
        void updateReadTime(double read_time);
        void updateWriteTime(double write_time);
        void updateAssignedActor(bool boolean);

        void printBatchInfo();
        void writeBatchToFile(std::string csv_output);

        std::string toString();
        /**
         * @brief Mark batch as assigned to an actor
         * Update the assigned_to_actor to True and
         * update the hostname and assigned_actor instance variables
         */
        void assignToActor(std::string hostname, caf::actor assigned_actor);


        template <class Inspector>
        friend bool inspect(Inspector& inspector, Batch& batch) {
            return inspector.object(batch).fields(
                        inspector.field("batch_id", batch.batch_id), 
                        inspector.field("start_hru", batch.start_hru),
                        inspector.field("num_hru", batch.num_hru),
                        inspector.field("run_time", batch.run_time),
                        inspector.field("read_time", batch.read_time),
                        inspector.field("write_time", batch.write_time),
                        inspector.field("status", batch.assigned_to_actor),
                        inspector.field("hostname", batch.hostname),
                        inspector.field("assigned_actor", batch.assigned_actor));
        }
};

class Batch_Container {
    private:
        int total_hru_count;
        int num_hru_per_batch;


        std::vector<Batch> batch_list;
        std::vector<Batch> solved_batches;
        std::vector<Batch> failed_batches;        
    
    
    public:
        /**
         * @brief Construct a new Batch_Container object
         * Creating the batch_manager will also create the batches
         * with the two parameters that are passed in. 
         */
        Batch_Container(int total_hru_count, int num_hru_per_batch);

        /**
         * @brief Get the Batches Remaining
         * returns the size of batch_list.
         */
        int getBatchesRemaining();
        
        /**
         * Assign a batch to be solved by a client.
         * The hostname and the actor_ref of the client solving this batch
         * are added to the client for the servers awareness
         * The batch is then returned by this method and sent to the respective client
         */
        std::optional<Batch> assignBatch(std::string hostname, caf::actor actor_ref);

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
         * @brief 
         * 
         */
        void updateBatchStatus_LostClient(int batch_id);


    
    private:
        /**
         * Assemble the total number of HRUs given by the user into batches.
         * The total number of hrus and the sizes of the batches are parameters
         * provided by the user.
         */
        void assembleBatches(int total_hru_count, int num_hru_per_batch);

        /**
         * @brief 
         * Find a batch by its id, 
         * return its index in the vector
         */
        std::optional<int> findBatch(int batch_id);





};