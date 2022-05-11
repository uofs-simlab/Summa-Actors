#ifndef OutputManager_H_
#define OutputManager_H_

#include "caf/all.hpp"
#include <vector>
/**
 * @brief Basic Container class to hold actor references. This has a size component for checking when it is full.
 * 
 */
class ActorRefList {
    private:
        int currentSize;
        int maxSize;
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
        
        bool isFull() {
            return list.size() == this->maxSize;
        }

        void addActor(caf::actor actor, int index, int returnMessage) {
            if (this->isFull()) {
                throw "List is full, cannot add actor to this list";
            }
            if (index > this->maxIndex) {
                this->maxIndex = index;
            } 
            if (index < this->minIndex || this->minIndex < 0) {
                this->minIndex = index;
            }

            this->currentSize++;
            list.push_back(std::make_tuple(actor, returnMessage));
        }

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
         * @return int The list index that actor is added to.
         */
        int addActor(caf::actor actor, int index, int returnMessage) {
            // Index has to be subtracted by 1 because Fortran array starts at 1
            int listIndex = (index - 1) / this->avgSizeOfActorList;
            if (listIndex > this->numVectors - 1) {
                listIndex =  this->numVectors - 1;
            }

            this->list[listIndex]->addActor(actor, index, returnMessage);
            return listIndex;
        }

        std::tuple<caf::actor,int> popActor(int index) {
            if (index > this->numVectors - 1 || index < 0) {
                throw "List Index Out Of Range";
            } else if (this->list[index]->isEmpty()) {
                throw "List is Empty, Nothing to pop";
            }

            return this->list[index]->popActor();

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