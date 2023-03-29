#pragma once

#include <chrono>

/**
 * Return the time between to time points
 */
double calculateTime(std::chrono::time_point<std::chrono::system_clock> start, 
    std::chrono::time_point<std::chrono::system_clock> end);

struct serializable_netcdf_gru_actor_info {
    double run_time;
    double init_duration;
    double forcing_duration;
    double run_physics_duration;
    double write_output_duration;

    int successful; // 0 = false, 1 = true
    int num_attempts;
};

template<class Inspector>
bool inspect(Inspector& f, serializable_netcdf_gru_actor_info& x) {
    return f.object(x).fields(f.field("run_time", x.run_time),
                              f.field("init_duration", x.init_duration),
                              f.field("forcing_duration", x.forcing_duration),
                              f.field("run_physics_duration", x.run_physics_duration),
                              f.field("write_output_duration", x.write_output_duration),
                              f.field("successful", x.successful),
                              f.field("num_attempts", x.num_attempts));
}



