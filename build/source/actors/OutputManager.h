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

        


};

class OutputManager {
    private:

        int numVectors;

        std::vector<std::vector<caf::actor>> actorRefList;



    public:
        // Constructor
        OutputManager(){}
        // Deconstructor
        ~OutputManager(){};
};

#endif 