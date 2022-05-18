#include "testCoordinator.h"
#include "../actors/global.h"
#include "../actors/OutputManager.h"
#include <chrono>
#include <iostream>
#include <thread>

#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " failed on line " << __LINE__ << std::endl; }

using namespace caf;
behavior test_coordinator(stateful_actor<test_state>* self) {
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


void testActorRefList(caf::actor_system& sys) {
    caf::scoped_actor self{sys};

    auto a1 = sys.spawn(test_coordinator);
    auto a2 = sys.spawn(test_coordinator);
    auto a3 = sys.spawn(test_coordinator);
    auto a4 = sys.spawn(test_coordinator);
    auto a5 = sys.spawn(test_coordinator);
    auto a6 = sys.spawn(test_coordinator);

    auto om = new ActorRefList(5);

    // Test Adding Actor To ActorRefList
    IS_TRUE(om->getCurrentSize() == 0);
    om->addActor(a1, 1, 9999);
    IS_TRUE(om->getCurrentSize() == 1);
    om->addActor(a2, 2, 3);
    IS_TRUE(om->getCurrentSize() == 2);
    om->addActor(a3, 3, 7);
    IS_TRUE(om->getCurrentSize() == 3);
    om->addActor(a4, 4, 9999);
    IS_TRUE(om->getCurrentSize() == 4);
    om->addActor(a5, 5, 8);
    IS_TRUE(om->getCurrentSize() == 5);

    // Try adding an actor to a full list
    try { 
        om->addActor(a6, 6, 9999);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
        IS_TRUE(om->getCurrentSize() == 5)
    }
    // Test isFull
    IS_TRUE(om->isFull());

    // Test removing actors from the list
    auto a7 = om->popActor();
    IS_TRUE(get<0>(a7) == a5 && get<1>(a7) == 8);
    IS_TRUE(om->getCurrentSize() == 4);
    auto a8 = om->popActor();
    IS_TRUE(get<0>(a8) == a4 && get<1>(a8) == 9999);
    IS_TRUE(om->getCurrentSize() == 3);
    auto a9 = om->popActor();
    IS_TRUE(get<0>(a9) == a3 && get<1>(a9) == 7);
    IS_TRUE(om->getCurrentSize() == 2);
    auto a10 = om->popActor();
    IS_TRUE(get<0>(a10) == a2 && get<1>(a10) == 3);
    IS_TRUE(om->getCurrentSize() == 1);
    auto a11 = om->popActor();
    IS_TRUE(get<0>(a11) == a1 && get<1>(a11) == 9999);
    try {
        om->popActor();
    }  catch (const char* msg) {
        std::cerr << msg << std::endl;
        IS_TRUE(om->getCurrentSize() == 0)
    }

    IS_TRUE(om->isEmpty())


    // Test Remove Failed
    aout(self) << "Testing Remove Failed" << std::endl;
    IS_TRUE(om->getCurrentSize() == 0);
    om->addActor(a1, 1, 9999);
    IS_TRUE(om->getCurrentSize() == 1);
    om->addActor(a2, 2, 3);
    IS_TRUE(om->getCurrentSize() == 2);
    om->addActor(a3, 3, 7);
    IS_TRUE(om->getCurrentSize() == 3);
    om->addActor(a4, 4, 9999);
    IS_TRUE(om->getCurrentSize() == 4);
    om->addActor(a5, 5, 8);
    IS_TRUE(om->getCurrentSize() == 5);

    // Test the removal of a failed actor from middle
    try {
        om->removeFailed(a3);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(om->getCurrentSize() == 4);
    IS_TRUE(om->getMaxSize() == 4);

    // Ensure the proper actor was removed
    a7 = om->popActor();
    IS_TRUE(get<0>(a7) == a5 && get<1>(a7) == 8);
    IS_TRUE(om->getCurrentSize() == 3);
    a8 = om->popActor();
    IS_TRUE(get<0>(a8) == a4 && get<1>(a8) == 9999);
    IS_TRUE(om->getCurrentSize() == 2);
    a10 = om->popActor();
    IS_TRUE(get<0>(a10) == a2 && get<1>(a10) == 3);
    IS_TRUE(om->getCurrentSize() == 1);
    a11 = om->popActor();
    IS_TRUE(get<0>(a11) == a1 && get<1>(a11) == 9999);

    IS_TRUE(om->isEmpty())

    delete om;
    om = new ActorRefList(5);

    // Remove Failed Actor from beginning of list
    IS_TRUE(om->getCurrentSize() == 0);
    om->addActor(a1, 1, 9999);
    IS_TRUE(om->getCurrentSize() == 1);
    om->addActor(a2, 2, 3);
    IS_TRUE(om->getCurrentSize() == 2);
    om->addActor(a3, 3, 7);
    IS_TRUE(om->getCurrentSize() == 3);
    om->addActor(a4, 4, 9999);
    IS_TRUE(om->getCurrentSize() == 4);
    om->addActor(a5, 5, 8);
    IS_TRUE(om->getCurrentSize() == 5);

    try {
        om->removeFailed(a1);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(om->getCurrentSize() == 4);
    IS_TRUE(om->getMaxSize() == 4);

    a8 = om->popActor();
    IS_TRUE(get<0>(a8) == a5 && get<1>(a8) == 8);
    IS_TRUE(om->getCurrentSize() == 3);
    a9 = om->popActor();
    IS_TRUE(get<0>(a9) == a4 && get<1>(a9) == 9999);
    IS_TRUE(om->getCurrentSize() == 2);
    a10 = om->popActor();
    IS_TRUE(get<0>(a10) == a3 && get<1>(a10) == 7);
    IS_TRUE(om->getCurrentSize() == 1);
    a11 = om->popActor();
    IS_TRUE(get<0>(a11) == a2 && get<1>(a11) == 3);

    IS_TRUE(om->isEmpty())
    delete om;
    om = new ActorRefList(5);

    // Remove Failed Actor from end of list
    IS_TRUE(om->getCurrentSize() == 0);
    om->addActor(a1, 1, 9999);
    IS_TRUE(om->getCurrentSize() == 1);
    om->addActor(a2, 2, 3);
    IS_TRUE(om->getCurrentSize() == 2);
    om->addActor(a3, 3, 7);
    IS_TRUE(om->getCurrentSize() == 3);
    om->addActor(a4, 4, 9999);
    IS_TRUE(om->getCurrentSize() == 4);
    om->addActor(a5, 5, 8);
    IS_TRUE(om->getCurrentSize() == 5);

    try {
        om->removeFailed(a5);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(om->getCurrentSize() == 4);
    IS_TRUE(om->getMaxSize() == 4);

    a7 = om->popActor();
    IS_TRUE(get<0>(a7) == a4 && get<1>(a7) == 9999);
    IS_TRUE(om->getCurrentSize() == 3);
    a8 = om->popActor();
    IS_TRUE(get<0>(a8) == a3 && get<1>(a8) == 7);
    IS_TRUE(om->getCurrentSize() == 2);
    a9 = om->popActor();
    IS_TRUE(get<0>(a9) == a2 && get<1>(a9) == 3);
    IS_TRUE(om->getCurrentSize() == 1);
    a10 = om->popActor();
    IS_TRUE(get<0>(a10) == a1 && get<1>(a10) == 9999);

    IS_TRUE(om->isEmpty())

    delete om;


}

void testOutputManager(caf::actor_system& sys) {
    caf::scoped_actor self{sys};

    auto a1 = sys.spawn(test_coordinator);
    auto a2 = sys.spawn(test_coordinator);
    auto a3 = sys.spawn(test_coordinator);
    auto a4 = sys.spawn(test_coordinator);
    auto a5 = sys.spawn(test_coordinator);
    auto a6 = sys.spawn(test_coordinator);
    auto a7 = sys.spawn(test_coordinator);
    auto a8 = sys.spawn(test_coordinator);
    auto a9 = sys.spawn(test_coordinator);
    auto a10 = sys.spawn(test_coordinator);

    auto OM = new OutputManager(5, 10);

    for (int i = 0; i < 5; i++) {
        IS_TRUE(OM->getSize(i) == 0)
    }
    try {
        OM->getSize(8);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }

    // Test adding actors and ensuring they go to the correct List
    OM->addActor(a1, 1, 1);
    IS_TRUE(OM->getSize(0) == 1);
    IS_TRUE(OM->getSize(1) == 0);
    IS_TRUE(OM->getSize(2) == 0);
    IS_TRUE(OM->getSize(3) == 0);
    IS_TRUE(OM->getSize(4) == 0);
    OM->addActor(a2, 2, 2);
    IS_TRUE(OM->getSize(0) == 2);
    IS_TRUE(OM->getSize(1) == 0);
    IS_TRUE(OM->getSize(2) == 0);
    IS_TRUE(OM->getSize(3) == 0);
    IS_TRUE(OM->getSize(4) == 0);
    OM->addActor(a3, 3, 3);
    IS_TRUE(OM->getSize(0) == 2);
    IS_TRUE(OM->getSize(1) == 1);
    IS_TRUE(OM->getSize(2) == 0);
    IS_TRUE(OM->getSize(3) == 0);
    IS_TRUE(OM->getSize(4) == 0);    
    OM->addActor(a4, 4, 4);
    IS_TRUE(OM->getSize(0) == 2);
    IS_TRUE(OM->getSize(1) == 2);
    IS_TRUE(OM->getSize(2) == 0);
    IS_TRUE(OM->getSize(3) == 0);
    IS_TRUE(OM->getSize(4) == 0);
    OM->addActor(a5, 5, 5);
    IS_TRUE(OM->getSize(0) == 2);
    IS_TRUE(OM->getSize(1) == 2);
    IS_TRUE(OM->getSize(2) == 1);
    IS_TRUE(OM->getSize(3) == 0);
    IS_TRUE(OM->getSize(4) == 0);
    OM->addActor(a6, 6, 6);
    IS_TRUE(OM->getSize(0) == 2);
    IS_TRUE(OM->getSize(1) == 2);
    IS_TRUE(OM->getSize(2) == 2);
    IS_TRUE(OM->getSize(3) == 0);
    IS_TRUE(OM->getSize(4) == 0);
    OM->addActor(a7, 7, 7);
    IS_TRUE(OM->getSize(0) == 2);
    IS_TRUE(OM->getSize(1) == 2);
    IS_TRUE(OM->getSize(2) == 2);
    IS_TRUE(OM->getSize(3) == 1);
    IS_TRUE(OM->getSize(4) == 0);
    OM->addActor(a8, 8, 8);
    IS_TRUE(OM->getSize(0) == 2);
    IS_TRUE(OM->getSize(1) == 2);
    IS_TRUE(OM->getSize(2) == 2);
    IS_TRUE(OM->getSize(3) == 2);
    IS_TRUE(OM->getSize(4) == 0);
    OM->addActor(a9, 9, 9);
    IS_TRUE(OM->getSize(0) == 2);
    IS_TRUE(OM->getSize(1) == 2);
    IS_TRUE(OM->getSize(2) == 2);
    IS_TRUE(OM->getSize(3) == 2);
    IS_TRUE(OM->getSize(4) == 1);
    IS_TRUE(!OM->isFull(4))

    OM->addActor(a10, 10, 10);
    IS_TRUE(OM->getSize(0) == 2);
    IS_TRUE(OM->getSize(1) == 2);
    IS_TRUE(OM->getSize(2) == 2);
    IS_TRUE(OM->getSize(3) == 2);
    IS_TRUE(OM->getSize(4) == 2);
    IS_TRUE(OM->isFull(0))
    IS_TRUE(OM->isFull(1))
    IS_TRUE(OM->isFull(2))
    IS_TRUE(OM->isFull(3))
    IS_TRUE(OM->isFull(4))

    // Test Poping Actors
    auto a11 = OM->popActor(0);
    IS_TRUE(get<0>(a11) == a2 && get<1>(a11) == 2);
    auto a14 = OM->popActor(0);
    IS_TRUE(get<0>(a14) ==  a1 && get<1>(a14) == 1);
    auto a12 = OM->popActor(1);
    IS_TRUE(get<0>(a12) == a4 && get<1>(a12) == 4);
    auto a13 = OM->popActor(4);
    IS_TRUE(get<0>(a13) == a10 && get<1>(a13) == 10);
    IS_TRUE(OM->getSize(0) == 0);
    IS_TRUE(OM->getSize(1) == 1);
    IS_TRUE(OM->getSize(2) == 2);
    IS_TRUE(OM->getSize(3) == 2);
    IS_TRUE(OM->getSize(4) == 1);

    OM->popActor(1);
    OM->popActor(2);
    OM->popActor(2);
    OM->popActor(3);
    OM->popActor(3);
    OM->popActor(4);

    IS_TRUE(OM->getSize(0) == 0);
    IS_TRUE(OM->getSize(1) == 0);
    IS_TRUE(OM->getSize(2) == 0);
    IS_TRUE(OM->getSize(3) == 0);
    IS_TRUE(OM->getSize(4) == 0);

    delete OM;

    // std::cout << "Creating Output Manager 2 with 3 vectors" << std::endl;

    auto OM2 = new OutputManager(3, 10);
    try {
        OM2->addActor(a1, 1, 1);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 1);
    IS_TRUE(OM2->getSize(1) == 0);
    IS_TRUE(OM2->getSize(2) == 0);
    try { 
        OM2->addActor(a2, 2, 2);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 2);
    IS_TRUE(OM2->getSize(1) == 0);
    IS_TRUE(OM2->getSize(2) == 0);
    try {
        OM2->addActor(a3, 3, 3);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 3);
    IS_TRUE(OM2->getSize(1) == 0);
    IS_TRUE(OM2->getSize(2) == 0);
    try {
        OM2->addActor(a4, 4, 4);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 3);
    IS_TRUE(OM2->getSize(1) == 1);
    IS_TRUE(OM2->getSize(2) == 0);
    try {
        OM2->addActor(a7, 7, 7);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 3);
    IS_TRUE(OM2->getSize(1) == 1);
    IS_TRUE(OM2->getSize(2) == 1);
    try {
        OM2->addActor(a8, 8, 8);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 3);
    IS_TRUE(OM2->getSize(1) == 1);
    IS_TRUE(OM2->getSize(2) == 2);
    try {
        OM2->addActor(a5, 5, 5);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 3);
    IS_TRUE(OM2->getSize(1) == 2);
    IS_TRUE(OM2->getSize(2) == 2);
    try {
        OM2->addActor(a6, 6, 6);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 3);
    IS_TRUE(OM2->getSize(1) == 3);
    IS_TRUE(OM2->getSize(2) == 2);
    try {
        OM2->addActor(a9, 9, 9);
    } catch (const char* msg) {
        std::cerr << msg << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 3);
    IS_TRUE(OM2->getSize(1) == 3);
    IS_TRUE(OM2->getSize(2) == 3);
    IS_TRUE(OM2->isFull(0));
    IS_TRUE(OM2->isFull(1));
    IS_TRUE(!OM2->isFull(2));
    try {
        OM2->addActor(a10, 10, 10);
    } catch (const char* msg) {
        std::cerr << msg << 10 << std::endl;
    }
    IS_TRUE(OM2->getSize(0) == 3);
    IS_TRUE(OM2->getSize(1) == 3);
    IS_TRUE(OM2->getSize(2) == 4);
    IS_TRUE(OM2->isFull(2));


    // Testing Remove Failed
    aout(self) << "testing Remove Failed from Output Structure \n";
    OM2->removeFailed(a1, 1);
    IS_TRUE(OM2->getSize(0) == 2);
    IS_TRUE(OM2->getSize(1) == 3);
    IS_TRUE(OM2->getSize(2) == 4);
    IS_TRUE(OM2->isFull(0));
    OM2->removeFailed(a5, 5);
    IS_TRUE(OM2->getSize(0) == 2);
    IS_TRUE(OM2->getSize(1) == 2);
    IS_TRUE(OM2->getSize(2) == 4);
    IS_TRUE(OM2->isFull(1));

    // Pop Actors

    a11 = OM2->popActor(0);
    IS_TRUE(get<0>(a11) == a3);
    a12 = OM2->popActor(0);
    IS_TRUE(get<0>(a12) == a2);
    IS_TRUE(OM2->isEmpty(0));

    OM2->addActor(a2, 2, 2);
    OM2->addActor(a3, 3, 3);
    IS_TRUE(OM2->isFull(0));





}



void caf_main(caf::actor_system& sys) {
    caf::scoped_actor self{sys};
    aout(self) << "Starting Test \n";
    // test_calculateTime();
    testActorRefList(sys);
    testOutputManager(sys);
}

CAF_MAIN()