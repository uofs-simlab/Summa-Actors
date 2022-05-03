#ifndef OutputManager_H_
#define OutputManager_H_

#include "caf/all.hpp"
#include <vector>

class ActorRefList {
    private:
        int currentSize;
        int maxSize;
        std::vector<caf::actor> list;
    
    public:
        // Constructor
        ActorRefList(int maxSize){
            this->currentSize = 0;
            this->maxSize = maxSize;
        }

        // Deconstructor
        ~ActorRefList(){};
        
        int getCurrentSize() {
            return this->currentSize;
        }
        
        bool isFull() {
            return list.size() == this->maxSize;
        }

        void addActor(caf::actor actor) {
            if (this->isFull()) {
                throw "List is full, cannot add actor to this list";
            }
            this->currentSize++;
            list.push_back(actor);
        }

        caf::actor popActor() {
            if (list.empty()) {
                throw "List is empty, nothing to pop.";
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

        void addActor(caf::actor actor, int index) {
            // Index has to be subtracted by 1 because Fortran array starts at 1
            int listIndex = (index - 1) / this->avgSizeOfActorList;
            if (listIndex > this->numVectors - 1 || listIndex < 0) {
                throw "List Index Out Of Range";
            }

            this->list[listIndex]->addActor(actor);
        }

        caf::actor popActor(int index) {
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

        int getSize(int listIndex) {
            if (listIndex > this->numVectors - 1) {
                throw "List Index Out Of Range";
            }
            return this->list[listIndex]->getCurrentSize();
        }

};

#endif 