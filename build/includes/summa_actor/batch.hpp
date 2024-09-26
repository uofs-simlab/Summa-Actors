#pragma once
#include "caf/all.hpp"
#include <string>
#include <iostream>
#include <fstream>


enum BatchStatus {
  UNASSIGNED,
  ASSIGNED,
  FAILED,
  SOLVED
};

class Batch {
  private:
    // Info
    std::string name_; // Name of the BatchContainer
    int batch_id_;
    
    // State
    std::string file_manager_;
    int start_gru_;
    int num_gru_;
    int num_failed_;
    int num_solved_;

    // Stats
    double run_time_;
    double read_time_;
    double write_time_;
    bool assigned_to_actor_;
    bool solved_;
    BatchStatus status_;

    std::string log_dir_; // Directory to write log files

  public:
    Batch(std::string name = "", int batch_id = -1, 
        std::string file_manager = "", int start_gru = -1, int num_gru = -1);
    
    // Getters
    inline std::string getName() const { return name_; }
    inline int getBatchID() const { return batch_id_; }
    inline std::string getFileManager() const { return file_manager_; }
    inline int getStartGru() const { return start_gru_;}
    inline int getNumGru() const { return num_gru_;}
    inline std::string getLogDir() const { return log_dir_; }
    inline int getNumFailed() const { return num_failed_; }
    inline int getNumSolved() const { return num_solved_; }
    inline double getRunTime() const { return run_time_; }
    inline double getReadTime() const { return read_time_; }
    inline double getWriteTime() const { return write_time_; }
    inline bool isAssigned() const { return assigned_to_actor_; }
    inline bool isSolved() const { return solved_; }
    // Setters
    inline void setLogDir(std::string log_dir) { log_dir_ = log_dir; }
    void setNumFailed(int num_failed) { num_failed_ = num_failed; }
    void setNumSolved(int num_solved) { num_solved_ = num_solved; }
    void updateRunTime(double run_time) { run_time_ = run_time; }
    void updateReadTime(double read_time) { read_time_ = read_time; }
    void updateWriteTime(double write_time) { write_time_ = write_time; }
    void updateAssigned(bool boolean) { assigned_to_actor_ = boolean; }
    void updateSolved(bool boolean) { solved_ = boolean; }
    void writeBatchToFile(std::string csv_output, std::string hostname);

    std::string toString() const;

    void assignToActor(std::string hostname, caf::actor assigned_actor);


    template <class Inspector>
    friend bool inspect(Inspector& inspector, Batch& batch) {
      return inspector.object(batch).fields(
             inspector.field("name", batch.name_),
             inspector.field("batch_id", batch.batch_id_), 
             inspector.field("file_manager", batch.file_manager_),
             inspector.field("start_gru", batch.start_gru_),
             inspector.field("num_gru", batch.num_gru_),
             inspector.field("run_time", batch.run_time_),
             inspector.field("read_time", batch.read_time_),
             inspector.field("write_time", batch.write_time_),
             inspector.field("assigned_to_actor", batch.assigned_to_actor_),
             inspector.field("solved", batch.solved_),
             inspector.field("log_dir", batch.log_dir_));
    }
};