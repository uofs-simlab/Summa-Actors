#ifndef OutputManager_H_
#define OutputManager_H_

#include "caf/all.hpp"
#include <vector>
#include <algorithm>
/**
 * @brief Basic Container class to hold actor references. This has a size component for checking when it is full.
 * 
 */
class ActorRefList {
    private:
        int numStepsToWrite; // We can save this value here so that we know how many steps to write
        int currentSize;
        unsigned int maxSize;
        int minIndex = -1; // minimum index of the actor being stored on this list
        int maxIndex = 0; // maximum index of the actor being stored on this list
        std::vector<std::tuple<caf::actor, int>> list;
    
    public:
        // Constructor
        ActorRefList(int maxSize){
            this->currentSize = 0;
            this->maxSize = maxSize;
        }

        // Deconstructor
        ~ActorRefList(){};

        int getMaxIndex() {
            return this->maxIndex;
        }

        int getMinIndex() {
            return this->minIndex;
        }
        
        int getCurrentSize() {
            return this->currentSize;
        }

        int getMaxSize() {
            return this->maxSize;
        }

        int getNumStepsToWrite() {
            return this->numStepsToWrite;
        }
        
        bool isFull() {
            return list.size() == this->maxSize;
        }

        /**
        * Adds An Actor and its return message as a tuple to this->list
        * actor - the actor ref of the actor being added to this->list
        * returnMessage - Either 9999 (place holder and specifies to send a done_write_v message) or
        * this is the current forcingFileList index that allows the file_access actor to know the number 
        * of steps the HRU actor that needs to compute 
        */
        void addActor(caf::actor actor, int index, int returnMessage, int numStepsToWrite) {
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

        /**
        * Return a tuple of an actor and its returnMessage.
        * The return message is 9999 or the index of the forcingFile it needs to acces
        */
        std::tuple<caf::actor,int> popActor() {
            if (list.empty()) {
                throw "List is empty, nothing to pop";
            }
            auto actor = list.back();
            list.pop_back();
            this->currentSize--;
            return actor;
        }


        bool isEmpty() {
            return list.empty();
        }


        /**
         * When an actor fails we need to decrement the count
         * so that this list becomes full when there is a failure
         * 
         * indexHRU - index of the HRU causing the error
         */
        void decrementMaxSize() {
            this->maxSize--;
        }

        /**
        * Remove the failed HRU from the list
        *
        */
        void removeFailed(caf::actor actorRef) {
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
};


/**
 * @brief Class that manages which structure actors are held on
 * 
 */
class OutputManager {
    private:

        int numVectors;
        int avgSizeOfActorList;
        std::vector<ActorRefList*> list;



    public:
        // Constructor
        OutputManager(int numVectors, int totalNumActors){
            this->numVectors = numVectors;
            int sizeOfOneVector = totalNumActors / numVectors;
            this->avgSizeOfActorList = sizeOfOneVector;
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
        // Deconstructor
        ~OutputManager(){};

        /**
         * @brief Adds an actor to its respective list
         * 
         * @param actor Actor reference
         * @param index Actor Index
         * @param returnMessage Forcing File index or 9999
         * @return int The list index that actor is added to.
         */
        int addActor(caf::actor actor, int index, int returnMessage, int numStepsToWrite) {
            // Index has to be subtracted by 1 because Fortran array starts at 1
            int listIndex = (index - 1) / this->avgSizeOfActorList;
            if (listIndex > this->numVectors - 1) {
                listIndex =  this->numVectors - 1;
            }

            this->list[listIndex]->addActor(actor, index, returnMessage, numStepsToWrite);
            return listIndex;
        }

        /**
        * Remove tuple from list[index]
        *
        */
        std::tuple<caf::actor,int> popActor(int index) {
            if (index > this->numVectors - 1 || index < 0) {
                throw "List Index Out Of Range";
            } else if (this->list[index]->isEmpty()) {
                throw "List is Empty, Nothing to pop";
            }

            return this->list[index]->popActor();
        }


        /** When a failure occurs an actor most likley will not already be on this list
         * This method may and probably should not be used. Although needing to remove a
         * specific element from a list may be needed.
         * Remove the failed actor from the list
         * Return the index of the list we removed the actor from
         * This is so we can check if it is full
         */
        int removeFailed(caf::actor actorRef, int index) {
            // Find the list this actor is on
            int listIndex = (index - 1) / this->avgSizeOfActorList;
            if (listIndex > this->numVectors - 1) {
                listIndex =  this->numVectors - 1;
            }
            
            this->list[listIndex]->removeFailed(actorRef);

            return listIndex;

        }

        /**
         *
         */ 

        int decrementMaxSize(int indexGRU) {
            // Find the list this actor is on
            int listIndex = (indexGRU - 1) / this->avgSizeOfActorList;
            if (listIndex > this->numVectors - 1) {
                listIndex =  this->numVectors - 1;
            }

            this->list[listIndex]->decrementMaxSize();
            return listIndex;
        }

        /**
         * Get the number of steps to write from the correct listIndex
         */
        int getNumStepsToWrite(int listIndex) {

            return this->list[listIndex]->getNumStepsToWrite();
        }

        bool isFull(int listIndex) {
            if (listIndex > this->numVectors - 1) {
                throw "List Index Out Of Range";
            }
            return this->list[listIndex]->isFull();
        }

        bool isEmpty(int listIndex) {
            return this->list[listIndex]->isEmpty();
        }

        int getSize(int listIndex) {
            if (listIndex > this->numVectors - 1) {
                throw "List Index Out Of Range";
            }
            return this->list[listIndex]->getCurrentSize();
        }

        int getMinIndex(int listIndex) {
            return this->list[listIndex]->getMinIndex();
        }

        int getMaxIndex(int listIndex) {
            return this->list[listIndex]->getMaxIndex();
        }

};

#endif 