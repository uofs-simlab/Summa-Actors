#include "test.hpp"
#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " failed on line " << __LINE__ << std::endl; }

void TEST_CLIENT() {
    std::cout << "Testing Output Container\n";

    // Call constructor
    Output_Container output_container = Output_Container(3,4);

}