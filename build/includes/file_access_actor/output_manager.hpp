#pragma once

#include "caf/all.hpp"
#include <vector>
#include <algorithm>


class ActorRefList {
    private:
        int numStepsToWrite; // We can save this value here so that we know how many steps to write
        int currentSize;
        unsigned int maxSize;
        int minIndex = -1; // minimum index of the actor being stored on this list
        int maxIndex = 0; // maximum index of the actor being stored on this list
        std::vector<std::tuple<caf::actor, int>> list;

    public:
        ActorRefList(int maxSize);
        ~ActorRefList();
        int getMaxIndex();
        int getMinIndex();
        int getCurrentSize();
        int getMaxSize();
        int getNumStepsToWrite();
        bool isFull();
        void addActor(caf::actor actor, int index, int returnMessage, int numStepsToWrite);
        std::tuple<caf::actor,int> popActor();
        bool isEmpty();
        void decrementMaxSize();
        void removeFailed(caf::actor actorRef);
};

class OutputManager {
    private:
        int numVectors;
        int avgSizeOfActorList;
        bool runningFailures;
        std::vector<ActorRefList*> list;
        std::vector<int> failedHRU;
        std::vector<int> failureReRun; // index used so we can add failedHRUs if they fail a second time
    public:
        OutputManager(int numVectors, int totalNumActors);
        ~OutputManager();
        int addActor(caf::actor actor, int index, int returnMessage, int numStepsToWrite);
        std::tuple<caf::actor,int> popActor(int index);
        int removeFailed(caf::actor actorRef, int index);
        int decrementMaxSize(int indexHRU);
        void restartFailures();
        int getNumStepsToWrite(int listIndex);
        bool isFull(int listIndex);
        bool isEmpty(int listIndex);
        int getSize(int listIndex);
        int getMinIndex(int listIndex);
        int getMaxIndex(int listIndex);
        void addFailed(int indxHRU);
};