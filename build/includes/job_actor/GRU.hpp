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

/**
 * Class that holds information about the running GRUs. This class is held by the job actor
 * The GRU/HRU actors that carry out the simulation are held by the GRU class 
*/
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
    int getLocalGRUIndex();
    caf::actor getGRUActor();

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
    void setFailed();

    void decrementAttemptsLeft();

};
