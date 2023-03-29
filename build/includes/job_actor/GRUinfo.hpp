#pragma once

#include "caf/all.hpp"
#include <iostream>
#include <fstream>

/*
 * Determine the state of the GRU
*/
enum class gru_state {
    running,
    failed,
    succeeded
};

auto success = [](const gru_state& state) -> int {
    return(state == gru_state::succeeded) ? 1 : 0;
};

class GRU {
  private:
    int global_gru_index; // The index of the GRU in the netcdf file
    int local_gru_index; // The index of the GRU within this job
    caf::actor gru_actor; // The actor for the GRU

    // Modifyable Parameters
    int dt_init_factor; // The initial dt for the GRU

    // Status Information
    int attempts_left; // The number of attempts left for the GRU to succeed
    gru_state state; // The state of the GRU

    // Timing Information
    double run_time = 0.0; // The total time to run the GRU
    double init_duration = 0.0; // The time to initialize the GRU
    double forcing_duration = 0.0; // The time to read the forcing data
    double run_physics_duration = 0.0; // The time to run the physics
    double write_output_duration = 0.0; // The time to write the output

    
  public:
    // Constructor
    GRU(int global_gru_index, int local_gru_index, caf::actor gru_actor, int dt_init_factor, int max_attempts);

    // Deconstructor
    ~GRU();

    // Getters
    int getGlobalGRUIndex();

    double getRunTime();
    double getInitDuration();
    double getForcingDuration();
    double getRunPhysicsDuration();
    double getWriteOutputDuration();

    double getAttemptsLeft();
    gru_state getStatus();


    // Setters
    void setRunTime(double run_time);
    void setInitDuration(double init_duration);
    void setForcingDuration(double forcing_duration);
    void setRunPhysicsDuration(double run_physics_duration);
    void setWriteOutputDuration(double write_output_duration);

    void setSuccess();

};



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
            GRUinfo(int refGRU, int indxGRU, caf::actor gru, int dt_init, int maxAttempts);

            // Deconstructor
            ~GRUinfo();


            int getRefGRU();

            int getIndxGRU();

            int getDt_init();

            caf::actor getActor();

            void updateGRU(caf::actor gru);

            void updateFailed();

            void updateCompletedToTrue();

            void updateDt_init();

            void updateCurrentAttempt();

            bool isMaxAttemptsReached();

            bool isFailed();

            bool isCompleted();

            void doneRun(double runTime, double initDuration, double forcingDuration,
                double runPhysicsDuration, double writeOutputDuration);

            void writeSuccess(std::string fileName, std::string hostname);

            void writeFail(std::string fileName);

            void printOutput();
};