#include "test.hpp"
#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " failed on line " << __LINE__ << std::endl; }

void TEST_CLIENT() {
    std::cout << "Testing Output Container\n";

    // Call constructor
    int num_hrus = 2;
    int num_steps = 2;

    // Test insertOutput
    hru_output_handles hru_output0;
    hru_output_handles hru_output1;
    hru_output_handles hru_output2;
    hru_output_handles hru_output3;




    std::vector<output_partition> output_partitions_vectors;
    int num_partitions = 4;
    int num_gru = 50;
    int num_timestep = 50;
    initArrayOfOuputPartitions(output_partitions_vectors, num_partitions, num_gru, num_timestep);
    IS_TRUE(output_partitions_vectors.size() == 4);
    IS_TRUE(output_partitions_vectors[0].start_gru == 1);
    IS_TRUE(output_partitions_vectors[0].num_gru == 12);
    IS_TRUE(output_partitions_vectors[0].num_timesteps == 50);
    IS_TRUE(output_partitions_vectors[0].hru_info_and_data.size() == 12);
    IS_TRUE(output_partitions_vectors[1].start_gru == 13);
    IS_TRUE(output_partitions_vectors[1].num_gru == 12);
    IS_TRUE(output_partitions_vectors[1].num_timesteps == 50);
    IS_TRUE(output_partitions_vectors[1].hru_info_and_data.size() == 12);
    IS_TRUE(output_partitions_vectors[2].start_gru == 25);
    IS_TRUE(output_partitions_vectors[2].num_gru == 12);
    IS_TRUE(output_partitions_vectors[2].num_timesteps == 50);
    IS_TRUE(output_partitions_vectors[2].hru_info_and_data.size() == 12);
    IS_TRUE(output_partitions_vectors[3].start_gru == 37);
    IS_TRUE(output_partitions_vectors[3].num_gru == 14);
    IS_TRUE(output_partitions_vectors[3].num_timesteps == 50);
    IS_TRUE(output_partitions_vectors[3].hru_info_and_data.size() == 14);

    std::cout << "Testing Output Container: DONE TEST 1\n";

    output_partitions_vectors.clear();
    int num_partitions2 = 8;
    int num_gru2 = 80;
    int num_timestep2 = 80;
    initArrayOfOuputPartitions(output_partitions_vectors, num_partitions2, num_gru2, num_timestep2);
    IS_TRUE(output_partitions_vectors.size() == 8);
    IS_TRUE(output_partitions_vectors[0].start_gru == 1);
    IS_TRUE(output_partitions_vectors[0].num_gru == 10);
    IS_TRUE(output_partitions_vectors[0].num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[0].hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[1].start_gru == 11);
    IS_TRUE(output_partitions_vectors[1].num_gru == 10);
    IS_TRUE(output_partitions_vectors[1].num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[1].hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[2].start_gru == 21);
    IS_TRUE(output_partitions_vectors[2].num_gru == 10);
    IS_TRUE(output_partitions_vectors[2].num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[2].hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[3].start_gru == 31);
    IS_TRUE(output_partitions_vectors[3].num_gru == 10);
    IS_TRUE(output_partitions_vectors[3].num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[3].hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[4].start_gru == 41);
    IS_TRUE(output_partitions_vectors[4].num_gru == 10);
    IS_TRUE(output_partitions_vectors[4].num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[4].hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[5].start_gru == 51);
    IS_TRUE(output_partitions_vectors[5].num_gru == 10);
    IS_TRUE(output_partitions_vectors[5].num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[5].hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[6].start_gru == 61);
    IS_TRUE(output_partitions_vectors[6].num_gru == 10);
    IS_TRUE(output_partitions_vectors[6].num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[6].hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[7].start_gru == 71);
    IS_TRUE(output_partitions_vectors[7].num_gru == 10);
    IS_TRUE(output_partitions_vectors[7].num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[7].hru_info_and_data.size() == 10);


    std::cout << "Testing Output Container: DONE TEST 2\n";






}


void TEST_OUTPUT_CONTAINER() {


    // Output_Container output_container = Output_Container(num_hrus, num_steps);


    // output_container.insertOutput(1, hru_output2);
    // output_container.insertOutput(1, hru_output3);
    // output_container.insertOutput(2, hru_output0);
    // output_container.insertOutput(2, hru_output3);
    // std::cout << "\nTesting for out of bounds errors - for 0\n";
    // output_container.insertOutput(0, hru_output0);
    // output_container.insertOutput(0, hru_output1);
    // std::cout << "\nTesting for out of bounds errors - for above max_hru\n";
    // output_container.insertOutput(5, hru_output2);
    // std::cout << "\nTesting for buffer full errors\n";
    // output_container.insertOutput(1, hru_output3);
    
}