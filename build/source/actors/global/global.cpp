#include "global.hpp"


double calculateTime(std::chrono::time_point<std::chrono::system_clock> start, 
    std::chrono::time_point<std::chrono::system_clock> end) {
    
    return std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
}
