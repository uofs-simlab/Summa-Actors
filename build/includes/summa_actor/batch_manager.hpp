#pragma once
#include "caf/all.hpp"
#include <vector>


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
        caf::actor assigned_actor;
        batch_status status;


    public:
        Batch(int batch_id, int start_hru, int num_hru);

        void printBatchInfo();

        batch_status getBatchStatus();

        int getBatchID();

        int getStartHRU();

        int getNumHRU();

        void solvedBatch();

        void assignedBatch();

        void updateRunTime(double run_time);

};





class Batch_Manager {
    private:
        std::vector<Batch> batch_list;
        


    public:

};