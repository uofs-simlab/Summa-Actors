#pragma once
#include "caf/all.hpp"
#include <string>
#include <iostream>
#include <fstream>

class Batch {
  private:
    int batch_id_;
    int start_hru_;
    int num_hru_;
    double run_time_;
    double read_time_;
    double write_time_;
    bool assigned_to_actor_;
    bool solved_;

    std::string log_dir_; // Directory to write log files

  public:
    Batch(int batch_id = -1, int start_hru = -1, int num_hru = -1);
        
    // Getters
    inline int getBatchID() const { return batch_id_; }
    inline int getStartHRU() const { return start_hru_;}
    inline int getNumHRU() const { return num_hru_;}
    inline std::string getLogDir() const { return log_dir_; }
    double getRunTime();
    double getReadTime();
    double getWriteTime();
    bool isAssigned();
    bool isSolved();
    // Setters
    inline void setLogDir(std::string log_dir) { log_dir_ = log_dir; }
    void updateRunTime(double run_time);
    void updateReadTime(double read_time);
    void updateWriteTime(double write_time);
    void updateAssigned(bool boolean);
    void updateSolved(bool boolean);
    void writeBatchToFile(std::string csv_output, std::string hostname);

    std::string toString();

    void assignToActor(std::string hostname, caf::actor assigned_actor);


    template <class Inspector>
    friend bool inspect(Inspector& inspector, Batch& batch) {
        return inspector.object(batch).fields(
               inspector.field("batch_id", batch.batch_id_), 
               inspector.field("start_hru", batch.start_hru_),
               inspector.field("num_hru", batch.num_hru_),
               inspector.field("run_time", batch.run_time_),
               inspector.field("read_time", batch.read_time_),
               inspector.field("write_time", batch.write_time_),
               inspector.field("assigned_to_actor", batch.assigned_to_actor_),
               inspector.field("solved", batch.solved_),
               inspector.field("log_dir", batch.log_dir_));
    }
};