#pragma once
#include "caf/all.hpp"
#include <vector>


class Batch {
    private:
        int batch_id;
        int start_hru;
        int num_hru;
        int run_time;
        int read_time;
        int write_time;
        caf::actor assigned_actor;


    public:
        Batch(int batch_id, int start_hru, int num_hru);

        void printBatchInfo();

};


class Batch_Manager {
    private:
        std::vector<Batch> batch_list;
        


    public:

};