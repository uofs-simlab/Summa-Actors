#include "test.hpp"
#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " failed on line " << __LINE__ << std::endl; }

void TEST_CLIENT() {
    std::cout << "Testing Output Container\n";

    // Call constructor
    int num_hrus = 2;
    int num_steps = 2;
    Output_Container output_container = Output_Container(num_hrus, num_steps);

    // Test insertOutput
    hru_output_handles hru_output0;
    hru_output_handles hru_output1;
    hru_output_handles hru_output2;
    hru_output_handles hru_output3;


    output_container.insertOutput(1, hru_output2);
    output_container.insertOutput(1, hru_output3);
    output_container.insertOutput(2, hru_output0);
    output_container.insertOutput(2, hru_output3);
    std::cout << "\nTesting for out of bounds errors - for 0\n";
    output_container.insertOutput(0, hru_output0);
    output_container.insertOutput(0, hru_output1);
    std::cout << "\nTesting for out of bounds errors - for above max_hru\n";
    output_container.insertOutput(5, hru_output2);
    std::cout << "\nTesting for buffer full errors\n";
    output_container.insertOutput(1, hru_output3);
    


}