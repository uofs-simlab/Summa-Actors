#include "output_manager.hpp"
#include "caf/all.hpp"

// ActorRefList
ActorRefList::ActorRefList(int maxSize){
    this->currentSize = 0;
    this->maxSize = maxSize;
}

ActorRefList::~ActorRefList(){}

int ActorRefList::getMaxIndex() {
    return this->maxIndex;
}

int ActorRefList::getMinIndex() {
    return this->minIndex;
}

int ActorRefList::getCurrentSize() {
    return this->currentSize;
}

int ActorRefList::getMaxSize() {
    return this->maxSize;
}

int ActorRefList::getNumStepsToWrite() {
    return this->numStepsToWrite;
}

bool ActorRefList::isFull() {
    return list.size() == this->maxSize;
}

void ActorRefList::addActor(caf::actor actor, int index, int returnMessage, int numStepsToWrite) {
    if (this->isFull()) {
        throw "List is full, cannot add actor to this list";
    }
    if (index > this->maxIndex) {
        this->maxIndex = index;
    } 
    if (index < this->minIndex || this->minIndex < 0) {
        this->minIndex = index;
    }
    this->numStepsToWrite = numStepsToWrite;
    this->currentSize++;
    list.push_back(std::make_tuple(actor, returnMessage));
}

std::tuple<caf::actor,int> ActorRefList::popActor() {
    if (list.empty()) {
        throw "List is empty, nothing to pop";
    }
    auto actor = list.back();
    list.pop_back();
    this->currentSize--;
    return actor;
}

bool ActorRefList::isEmpty() {
    return list.empty();
}

void ActorRefList::decrementMaxSize() {
    this->maxSize--;
}

void ActorRefList::removeFailed(caf::actor actorRef) {
    bool found = false;
    for(std::vector<std::tuple<caf::actor, int>>::iterator it = this->list.begin(); it != this->list.end(); it++) {
        if (std::get<0>(*it) == actorRef) {
            found = true;
            this->list.erase(it);
            this->currentSize--; this->maxSize--;
            break;
        }
    }

    if (!found) {
        throw "Element To Remove Not Found";
    }
}

// OutputManager
OutputManager::OutputManager(int numVectors, int totalNumActors){
    this->numVectors = numVectors;
    int sizeOfOneVector = totalNumActors / numVectors;
    this->avgSizeOfActorList = sizeOfOneVector;
    this->runningFailures = false;
    // Create the first n-1 vectors with the same size 
    for (int i = 0; i < numVectors - 1; i++) {
        auto refList = new ActorRefList(sizeOfOneVector);
        totalNumActors = totalNumActors - sizeOfOneVector;
        list.push_back(refList);
    }
    // Create the last vector with size however many actors are left
    auto refList = new ActorRefList(totalNumActors);
    list.push_back(refList);
}

OutputManager::~OutputManager(){};

int OutputManager::addActor(caf::actor actor, int index, int returnMessage, int numStepsToWrite) {
    int listIndex;
    if (this->runningFailures) {
        // find the index of the structure this HRU is in
        auto it = find(this->failureReRun.begin(), this->failureReRun.end(), index);

        if (it != this->failureReRun.end()) {
            listIndex = it - this->failureReRun.begin();
        } else {
            throw "Element Not Found in failureReRun list";
        }

        this->list[listIndex]->addActor(actor, index, returnMessage, numStepsToWrite);

    } else {
        // Index has to be subtracted by 1 because Fortran array starts at 1
        listIndex = (index - 1) / this->avgSizeOfActorList;
        if (listIndex > this->numVectors - 1) {
            listIndex =  this->numVectors - 1;
        }

        this->list[listIndex]->addActor(actor, index, returnMessage, numStepsToWrite);
    }

    return listIndex;
}

std::tuple<caf::actor,int> OutputManager::popActor(int index) {
    if (index > this->numVectors - 1 || index < 0) {
        throw "List Index Out Of Range";
    } else if (this->list[index]->isEmpty()) {
        throw "List is Empty, Nothing to pop";
    }

    return this->list[index]->popActor();
}

int OutputManager::removeFailed(caf::actor actorRef, int index) {
    // Find the list this actor is on
    int listIndex = (index - 1) / this->avgSizeOfActorList;
    if (listIndex > this->numVectors - 1) {
        listIndex =  this->numVectors - 1;
    }
    
    this->list[listIndex]->removeFailed(actorRef);

    return listIndex;
}

int OutputManager::decrementMaxSize(int indexHRU) {
    
    this->failedHRU.push_back(indexHRU);

    // Find the list this actor is on
    int listIndex = (indexHRU - 1) / this->avgSizeOfActorList;
    if (listIndex > this->numVectors - 1) {
        listIndex =  this->numVectors - 1;
    }

    this->list[listIndex]->decrementMaxSize();
    return listIndex;
}

void OutputManager::restartFailures() {
    this->list.clear();
    this->numVectors = this->failedHRU.size();
    for (unsigned int i = 0; i < this->failedHRU.size(); i++) {
        auto refList = new ActorRefList(1);
        this->list.push_back(refList);
    }

    this->failureReRun = this->failedHRU;
    this->failedHRU.clear();

    this->runningFailures = true;

}

int OutputManager::getNumStepsToWrite(int listIndex) {

    return this->list[listIndex]->getNumStepsToWrite();
}

bool OutputManager::isFull(int listIndex) {
    if (listIndex > this->numVectors - 1) {
        throw "List Index Out Of Range";
    }
    return this->list[listIndex]->isFull();
}

bool OutputManager::isEmpty(int listIndex) {
    return this->list[listIndex]->isEmpty();
}

int OutputManager::getSize(int listIndex) {
    if (listIndex > this->numVectors - 1) {
        throw "List Index Out Of Range";
    }
    return this->list[listIndex]->getCurrentSize();
}

int OutputManager::getMinIndex(int listIndex) {
    return this->list[listIndex]->getMinIndex();
}

int OutputManager::getMaxIndex(int listIndex) {
    return this->list[listIndex]->getMaxIndex();
}

void OutputManager::addFailed(int indxHRU) {
    this->failedHRU.push_back(indxHRU);
}