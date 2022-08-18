#include "global.hpp"
#include <chrono>

struct iLookVarType {
    // These values should be one index less than the Fortran structure
    // This is so they align with C++ vectors that start at 0
    int scalarv = -9999;
    int wLength = -9999;
    int midSnow = -9999;
    int midSoil = -9999;
    int midToto = -9999;
    int ifcSnow = -9999;
    int ifcSoil = -9999;
    int ifcToto = -9999;
    int parSoil = -9999;
    int routing = -9999;
    int outstat = -9999;
    int unknown = -9999;
};




double calculateTime(std::chrono::time_point<std::chrono::system_clock> start, 
    std::chrono::time_point<std::chrono::system_clock> end) {
    
    return std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
}
