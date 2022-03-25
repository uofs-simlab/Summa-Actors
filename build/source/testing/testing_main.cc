#include "testCoordinator.h"
#include "../actors/commonFunctions.h"
#include <chrono>
#include <thread>

#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " failed on line " << __LINE__ << std::endl; }

using namespace caf;
behavior test_coordinator(stateful_actor<test_state>* self) {
    aout(self) << "Starting Test Actor\n";
    return {
    };
}

bool calcTimeTest(int sleepTime) {
    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;
    double duration;

    start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(sleepTime));
    end = std::chrono::high_resolution_clock::now();
    duration = calculateTime(start, end);
    if (duration != sleepTime) {
        std::cout << "Error: calculatTime, value is " << duration << std::endl; 
        return false;
    } else {
        return true;
    }
}

bool calcTimeAccumulate(int sleepTime1, int sleepTime2) {
    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;
    double duration;

    start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(sleepTime1));
    end = std::chrono::high_resolution_clock::now();
    duration = calculateTime(start, end);

    start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(sleepTime2));
    end = std::chrono::high_resolution_clock::now();
    duration += calculateTime(start, end);
    if (duration != sleepTime1 + sleepTime2) {
        std::cout << "Error: calculatTime, value is " << duration << std::endl; 
        return false;
    } else {
        return true;
    }
}

bool calcTimeAccumulate3Val(int sleepTime1, int sleepTime2, int sleepTime3) {
    std::chrono::time_point<std::chrono::system_clock> start;
    std::chrono::time_point<std::chrono::system_clock> end;
    double duration;

    start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(sleepTime1));
    end = std::chrono::high_resolution_clock::now();
    duration = calculateTime(start, end);

    start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(sleepTime2));
    end = std::chrono::high_resolution_clock::now();
    duration += calculateTime(start, end);

    start = std::chrono::high_resolution_clock::now();
    std::this_thread::sleep_for(std::chrono::seconds(sleepTime3));
    end = std::chrono::high_resolution_clock::now();
    duration += calculateTime(start, end);

    if (duration != sleepTime1 + sleepTime2 + sleepTime3) {
        std::cout << "Error: calculatTime, value is " << duration << std::endl; 
        return false;
    } else {
        return true;
    }
}


void test_calculateTime() {
    IS_TRUE(calcTimeTest(2));
    IS_TRUE(calcTimeTest(4));
    IS_TRUE(calcTimeTest(5));
    IS_TRUE(calcTimeAccumulate(2, 3));
    IS_TRUE(calcTimeAccumulate(3, 4));
    IS_TRUE(calcTimeAccumulate(5, 5));
    IS_TRUE(calcTimeAccumulate3Val(1, 2, 3));
    IS_TRUE(calcTimeAccumulate3Val(1, 1, 1));
    IS_TRUE(calcTimeAccumulate3Val(5, 2, 3));
}

void caf_main(caf::actor_system& sys) {
    caf::scoped_actor self{sys};
    aout(self) << "Starting Test \n";
    test_calculateTime();
}

CAF_MAIN()