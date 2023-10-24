#pragma once
#include "caf/all.hpp"
#include <string>

class Batch {
    private:
        int batch_id;
        int start_hru;
        int num_hru;

        double run_time;
        double read_time;
        double write_time;
        
        bool assigned_to_actor;
        bool solved;

     public:
        Batch(int batch_id = -1, int start_hru = -1, int num_hru = -1);
        
        // Getters
        int getBatchID();
        int getStartHRU();
        int getNumHRU();
        double getRunTime();
        double getReadTime();
        double getWriteTime();
        bool isAssigned();
        bool isSolved();
        // Setters
        void updateRunTime(double run_time);
        void updateReadTime(double read_time);
        void updateWriteTime(double write_time);
        void updateAssigned(bool boolean);
        void updateSolved(bool boolean);
        void printBatchInfo();
        void writeBatchToFile(std::string csv_output, std::string hostname);

        std::string toString();

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
                        inspector.field("assigned_to_actor", batch.assigned_to_actor),
                        inspector.field("solved", batch.solved));
        }
};