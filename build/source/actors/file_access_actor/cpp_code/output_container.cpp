#include "output_container.hpp"


Output_Container::Output_Container(int max_hrus, int max_steps) {
    this->max_hrus = max_hrus;
    this->max_steps = max_steps;
    
}

Output_Container::~Output_Container(){};

void insertOutput(int hru_index, hru_output_handles hru_output) {
    
};