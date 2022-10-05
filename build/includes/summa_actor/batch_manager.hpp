#pragma once
#include "caf/all.hpp"
#include <vector>
#include <string>


enum batch_status {
    unassigned,
    assigned,
    solved,
    failed
};

struct Batch_Struct {
    int batch_id;
    int start_hru;
    int num_hru;

    double run_time;
    double read_time;
    double write_time;
    
    std::string assigned_host;
    caf::actor assigned_actor;
    bool assigned;
};

template<class Inspector>
bool inspect(Inspector& inspector, Batch_Struct& batch) {
    return inspector.object(batch).fields(
                inspector.field("batch_id", batch.batch_id),
                inspector.field("start_hru",batch.start_hru),
                inspector.field("num_hru",   batch.num_hru),
                inspector.field("run_time",  batch.run_time),
                inspector.field("write_time",batch.write_time),
                inspector.field("assigned_host", batch.assigned_host),
                inspector.field("assigned_actor", batch.assigned_actor),
                inspector.field("assigned", batch.assigned));
}

class Batch {
    private:
        int batch_id;
        int start_hru;
        int num_hru;

        double run_time;
        double read_time;
        double write_time;
        
        std::string assigned_host;
        caf::actor assigned_actor;
        batch_status status;


    public:
        Batch(int batch_id, int start_hru, int num_hru);

        void printBatchInfo();

        batch_status getBatchStatus();

        int getBatchID();

        int getStartHRU();

        int getNumHRU();

        void solvedBatch(double run_time, double read_time, double write_time);

        void assignedBatch(std::string hostname, caf::actor actor_ref);

        void updateRunTime(double run_time);

        void writeBatchToFile(std::string file_name);
};


class Batch_Manager {
    private:
        std::vector<Batch> batch_list;
        std::vector<Batch> solved_batches;
        std::vector<Batch> failed_batches;        
    
    
    public:
        /**
         * Assemble the total number of HRUs given by the user into batches.
         * The total number of hrus and the sizes of the batches are parameters
         * provided by the user.
         */
        void assembleBatches(int total_hru_count, int num_hru_per_batch);


        /**
         * Assign a batch to be solved by a client.
         * The hostname and the actor_ref of the client solving this batch
         * are added to the client for the servers awareness
         * The batch is then returned by this method and sent to the respective client
         */
        Batch assignBatch(std::string hostname, caf::actor actor_ref);


        /**
         * On a successful batch we take the batch given to us by the client 
         * and add it to our solved_batches list.
         *  
         * We can then remove the batch from the global batch list.
         */
        void batchSuccess(Batch successful_batch, std::string output_csv);


        /**
         * A batch failure is returned to us by the client
         * This is for when a client failed to solve the batch.
         */
        void batchFailure(Batch failed_batch);


        /**
         * A client has found to be disconnected. Unassign all batches
         * that were assigned to the disconnected client. The client id 
         * is passed in as a parameter
         */
        void disconnected_client(int client_id);



};