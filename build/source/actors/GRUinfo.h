#ifndef GRUinfo_H_
#define GRUinfo_H_

#include "caf/all.hpp"
#include <iostream>
#include <fstream>


class GRUinfo {
  private:
    int refGRU; // This will be the same as the refGRU
    int indxGRU;
    caf::actor GRU;

    // Variable to update
    int dt_init;

    // Completed Information
    int currentAttempt;
    int maxAttempts;
    bool completed;
    bool failed;

    // Timing information for the GRU
    double runTime;
    double initDuration;
    double forcingDuration;
    double runPhysicsDuration;
    double writeOutputDuration;

  public:
    // Constructor
    GRUinfo(int refGRU, int indxGRU, caf::actor gru, int dt_init, int maxAttempts) {
      this->refGRU = refGRU;
      this->indxGRU = indxGRU;
      this->GRU = gru;
      this->dt_init = dt_init;
      this->currentAttempt = 1;
      this->maxAttempts = maxAttempts;
      this->completed = false;
      this->failed = false;
    }
    // Deconstructor
    ~GRUinfo(){};
    
    // Getters
    int getRefGRU() {
      return this->refGRU;
    }

    int getIndxGRU() {
      return this->indxGRU;
    }

    int getDt_init() {
      return this->dt_init;
    }

    caf::actor getActor() {
      return GRU;
    }
    // Setters
    void updateGRU(caf::actor gru) {
      this->GRU = gru;
    }

    void updateFailed() {
      if (this->failed) {
        this->failed = false;
      } else {
        this->failed = true;
      }
    }

    void updateCompletedToTrue(){
      this->completed = true;
    }

    void updateDt_init() {
      this->dt_init = this->dt_init * 2;
    }

    void updateCurrentAttempt() {
      this->currentAttempt++;
    }

    // Methods that return Booleans
    bool isMaxAttemptsReached() {
      return this->maxAttempts <= this->currentAttempt;
    }

    bool isFailed() {
      return this->failed;
    }

    bool isCompleted() {
      return this->completed;
    }

    void doneRun(double runTime, double initDuration, double forcingDuration,
      double runPhysicsDuration, double writeOutputDuration) {
        this->runTime = runTime;
        this->initDuration = initDuration;
        this->forcingDuration = forcingDuration;
        this->runPhysicsDuration = runPhysicsDuration;
        this->writeOutputDuration = writeOutputDuration;
    }

    // Methods for writing statistics to a file
    void writeSuccess(std::string fileName) {
      std::ofstream file;
      file.open(fileName, std::ios_base::app);
      file << this->refGRU << "," 
           << this->runTime << "," 
           << this->initDuration << "," 
           << this->forcingDuration << "," 
           << this->runPhysicsDuration << "," 
           << this->writeOutputDuration << "," 
           << this->dt_init << "," 
           << this->currentAttempt << "\n";
      file.close();
    }

    void writeFail(std::string fileName) {
      std::ofstream file;
      file.open(fileName, std::ios_base::app);
      file << this->refGRU << ","
           << this->dt_init << ","
           << this->currentAttempt << "\n";
      file.close();
    }



};
#endif