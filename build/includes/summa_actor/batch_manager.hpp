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

};