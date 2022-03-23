#ifndef GRUStruc_H_
#define GRUStruc_H_

#include "caf/all.hpp"
#include "vector"
#include "GRUInfo.h"


/**
* GRUStruc is an object that helps the coordinator keep track 
* of which GRUs need to run, have been completed, and or have 
* failed.
**/
class GRUStruc {
    private:
        std::vector<GRUInfo> GRUs_to_compute;
        std::vector<GRUInfo> GRUs_completed;
        std::vector<GRUInfo> GRUs_failed;
        int passed;
        int failed;
        int Left_to_compute;
    
    public:
        GRUStruc(int startGRU, int numGRUs) {
            for(int i = 0; i <= numGRUs; i++) {
                // put a start object in so refGRU = index in vector
                if (i == 0) {
                    auto gru = GRUInfo(i);
                    GRUs_to_compute.push_back(gru);
                } else {
                    auto gru = GRUInfo(startGRU + i -1);
                    GRUs_to_compute.push_back(gru);
                }
            }
        }

        void print_struc(int numGRUs) {
            for (int i = 0; i <= numGRUs; i++) {
                std::cout << "GRUInfo::" << GRUs_to_compute[i].getRefGRU() << std::endl;
            }
        }

        void addGRU(GRUInfo gru) {
            this->GRUs_to_compute.push_back(gru);
        }

        void GRUComplete(int refGRU) {

        }

};
#endif