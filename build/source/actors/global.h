#ifndef COMMONFUNCTIONS_H_
#define COMMONFUNCTIONS_H_

#include <chrono>


// Gobal Flag for Debuging only main function will change this
bool debug;

/**
 * Return the time between to time points
 */
double calculateTime(std::chrono::time_point<std::chrono::system_clock> start, 
    std::chrono::time_point<std::chrono::system_clock> end);



double calculateTime(std::chrono::time_point<std::chrono::system_clock> start, 
    std::chrono::time_point<std::chrono::system_clock> end) {
    
    return std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
}




#endif