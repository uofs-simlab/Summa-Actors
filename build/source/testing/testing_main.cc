#include "testCoordinator.h"
#include "../actors/global.h"
#include "../actors/OutputManager.h"
#include <chrono>
#include <iostream>
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
        std::cout << "Error: calculateTime, value is " << duration << std::endl; 
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


void testOutputManager(caf::actor_system& sys) {
    auto a1 = sys.spawn(test_coordinator);
    auto a2 = sys.spawn(test_coordinator);
    auto a3 = sys.spawn(test_coordinator);
    auto a4 = sys.spawn(test_coordinator);
    auto a5 = sys.spawn(test_coordinator);
    auto a6 = sys.spawn(test_coordinator);

    auto om = new ActorRefList(5);

    IS_TRUE(om->getCurrentSize() == 0);
    om->addActor(a1);
    IS_TRUE(om->getCurrentSize() == 1);
    om->addActor(a2);
    IS_TRUE(om->getCurrentSize() == 2);
    om->addActor(a3);
    IS_TRUE(om->getCurrentSize() == 3);
    om->addActor(a4);
    IS_TRUE(om->getCurrentSize() == 4);
    om->addActor(a5);
    IS_TRUE(om->getCurrentSize() == 5);
    try { 
        om->addActor(a6);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
        IS_TRUE(om->getCurrentSize() == 5)
    }

    IS_TRUE(om->isFull());

    auto a7 = om->popActor();
    IS_TRUE(a7 == a5);
    IS_TRUE(om->getCurrentSize() == 4);
    auto a8 = om->popActor();
    IS_TRUE(a8 == a4);
    IS_TRUE(om->getCurrentSize() == 3);
    auto a9 = om->popActor();
    IS_TRUE(a9 == a3);
    IS_TRUE(om->getCurrentSize() == 2);
    auto a10 = om->popActor();
    IS_TRUE(a10 == a2);
    IS_TRUE(om->getCurrentSize() == 1);
    auto a11 = om->popActor();
    IS_TRUE(a11 == a1);
    try {
        om->popActor();
    }  catch (const char* msg) {
        std::cerr << msg << std::endl;
        IS_TRUE(om->getCurrentSize() == 0)
    }






}




void caf_main(caf::actor_system& sys) {
    caf::scoped_actor self{sys};
    aout(self) << "Starting Test \n";
    // test_calculateTime();
    testOutputManager(sys);
}

CAF_MAIN()