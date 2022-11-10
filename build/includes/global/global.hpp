#pragma once

#include <chrono>

extern bool debug;

/**
 * Return the time between to time points
 */
double calculateTime(std::chrono::time_point<std::chrono::system_clock> start, 
    std::chrono::time_point<std::chrono::system_clock> end);



