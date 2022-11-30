#include "test.hpp"
#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " \n **FAILED ON LINE " << __LINE__ << std::endl; }

void TEST_CLIENT() {
    std::cout << "Testing Output Container\n";
    std::vector<std::shared_ptr<hru_output_handles>> dummy_data;
    for (int i = 0; i < 50; i++) {
        dummy_data.push_back(std::make_shared<hru_output_handles>());
    }
    // // Test insertOutput
    // std::vector<hru_output_handles> dummy_data(50);

    std::vector<std::shared_ptr<output_partition>> output_partitions_vectors;
    int num_partitions = 4;
    int num_gru = 50;
    int num_timestep = 50;
    initArrayOfOuputPartitions(output_partitions_vectors, num_partitions, num_gru, num_timestep);
    IS_TRUE(output_partitions_vectors.size() == 4);
    IS_TRUE(output_partitions_vectors[0]->start_gru == 1);
    IS_TRUE(output_partitions_vectors[0]->num_gru == 12);
    IS_TRUE(output_partitions_vectors[0]->num_timesteps == 50);
    IS_TRUE(output_partitions_vectors[0]->hru_info_and_data.size() == 12);
    IS_TRUE(output_partitions_vectors[1]->start_gru == 13);
    IS_TRUE(output_partitions_vectors[1]->num_gru == 12);
    IS_TRUE(output_partitions_vectors[1]->num_timesteps == 50);
    IS_TRUE(output_partitions_vectors[1]->hru_info_and_data.size() == 12);
    IS_TRUE(output_partitions_vectors[2]->start_gru == 25);
    IS_TRUE(output_partitions_vectors[2]->num_gru == 12);
    IS_TRUE(output_partitions_vectors[2]->num_timesteps == 50);
    IS_TRUE(output_partitions_vectors[2]->hru_info_and_data.size() == 12);
    IS_TRUE(output_partitions_vectors[3]->start_gru == 37);
    IS_TRUE(output_partitions_vectors[3]->num_gru == 14);
    IS_TRUE(output_partitions_vectors[3]->num_timesteps == 50);
    IS_TRUE(output_partitions_vectors[3]->hru_info_and_data.size() == 14);
    std::cout << "Testing Output Container: DONE TEST 1\n";

    std::cout << "Testing Partitions indexes\n";
    int partition_size = 12;
    IS_TRUE(findPatritionIndex(partition_size, 1, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 2, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 3, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 4, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 5, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 6, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 7, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 8, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 9, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 10, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 11, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 12, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 13, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 14, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 15, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 16, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 17, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 18, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 19, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 20, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 21, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 22, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 23, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 24, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 25, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 26, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 27, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 28, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 29, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 30, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 31, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 32, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 33, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 34, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 35, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 36, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 37, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 38, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 39, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 40, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 41, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 42, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 43, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 44, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 45, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 46, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 47, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 48, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 49, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 50, num_partitions) == 3);
    std::cout << "Testing Partitions indexes: DONE TEST 1\n";

    std::cout << "Testing finding GRU index in a partition\n";
    // partition = 1
    int gru_index = 1;
    int start_gru = 1;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 0);
    gru_index = 12;
    start_gru = 1;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 11);
    gru_index = 13;
    start_gru = 13;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 0);
    gru_index = 24;
    start_gru = 13;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 11);
    gru_index = 25;
    start_gru = 25;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 0);
    gru_index = 36;
    start_gru = 25;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 11);
    gru_index = 37;
    start_gru = 37;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 0);
    gru_index = 50;
    start_gru = 37;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 13);



    output_partitions_vectors.clear();
    int num_partitions2 = 8;
    int num_gru2 = 80;
    int num_timestep2 = 80;
    initArrayOfOuputPartitions(output_partitions_vectors, num_partitions2, num_gru2, num_timestep2);
    IS_TRUE(output_partitions_vectors.size() == 8);
    IS_TRUE(output_partitions_vectors[0]->start_gru == 1);
    IS_TRUE(output_partitions_vectors[0]->num_gru == 10);
    IS_TRUE(output_partitions_vectors[0]->num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[0]->hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[1]->start_gru == 11);
    IS_TRUE(output_partitions_vectors[1]->num_gru == 10);
    IS_TRUE(output_partitions_vectors[1]->num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[1]->hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[2]->start_gru == 21);
    IS_TRUE(output_partitions_vectors[2]->num_gru == 10);
    IS_TRUE(output_partitions_vectors[2]->num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[2]->hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[3]->start_gru == 31);
    IS_TRUE(output_partitions_vectors[3]->num_gru == 10);
    IS_TRUE(output_partitions_vectors[3]->num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[3]->hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[4]->start_gru == 41);
    IS_TRUE(output_partitions_vectors[4]->num_gru == 10);
    IS_TRUE(output_partitions_vectors[4]->num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[5]->start_gru == 51);
    IS_TRUE(output_partitions_vectors[5]->num_gru == 10);
    IS_TRUE(output_partitions_vectors[5]->num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[5]->hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[6]->start_gru == 61);
    IS_TRUE(output_partitions_vectors[6]->num_gru == 10);
    IS_TRUE(output_partitions_vectors[6]->num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[6]->hru_info_and_data.size() == 10);
    IS_TRUE(output_partitions_vectors[7]->start_gru == 71);
    IS_TRUE(output_partitions_vectors[7]->num_gru == 10);
    IS_TRUE(output_partitions_vectors[7]->num_timesteps == 80);
    IS_TRUE(output_partitions_vectors[7]->hru_info_and_data.size() == 10);

    std::cout << "Testing Output Container: DONE TEST 2\n";
    partition_size = 10;
    num_partitions = 8;
    IS_TRUE(findPatritionIndex(partition_size, 1, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 10, num_partitions) == 0);
    IS_TRUE(findPatritionIndex(partition_size, 11, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 20, num_partitions) == 1);
    IS_TRUE(findPatritionIndex(partition_size, 21, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 30, num_partitions) == 2);
    IS_TRUE(findPatritionIndex(partition_size, 31, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 40, num_partitions) == 3);
    IS_TRUE(findPatritionIndex(partition_size, 41, num_partitions) == 4);
    IS_TRUE(findPatritionIndex(partition_size, 50, num_partitions) == 4);
    IS_TRUE(findPatritionIndex(partition_size, 51, num_partitions) == 5);
    IS_TRUE(findPatritionIndex(partition_size, 60, num_partitions) == 5);
    IS_TRUE(findPatritionIndex(partition_size, 61, num_partitions) == 6);
    IS_TRUE(findPatritionIndex(partition_size, 70, num_partitions) == 6);
    IS_TRUE(findPatritionIndex(partition_size, 71, num_partitions) == 7);
    IS_TRUE(findPatritionIndex(partition_size, 80, num_partitions) == 7);
    std::cout << "Testing Partitions indexes: DONE TEST 2\n";


    std::cout << "Testing findGRUIndexInPartition2: START\n";
    gru_index = 4;
    start_gru = 1;
    // partition index = 0
    
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 3);
    gru_index = 10;
    start_gru = 1;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 9);
    // partition index = 1
    gru_index = 11;
    start_gru = 11;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 0);
    gru_index = 20;
    start_gru = 11;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 9);
    // partition index = 2
    gru_index = 21;
    start_gru = 21;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 0);
    gru_index = 30;
    start_gru = 21;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 9);
    // partition index = 3
    gru_index = 31;
    start_gru = 31;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 0);
    // partition index = 7
    gru_index = 71;
    start_gru = 71;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 0);
    gru_index = 80;
    start_gru = 71;
    IS_TRUE(findGRUIndexInPartition(gru_index, start_gru) == 9);
    std::cout << "Testing findGRUIndexInPartition2: DONE\n";




    // Test adding output to the output structure
    std::cout << "Testing addOutputToOutputStructure: START\n";
    output_partitions_vectors.clear();
    num_partitions = 5;
    num_gru = 8;
    num_timestep = 2;
    caf::actor actor0; // index_gru = 1, hru_index = 1
    caf::actor actor1; // index_gru = 2, hru_index = 1
    caf::actor actor2; // index_gru = 3, hru_index = 1
    caf::actor actor3; // index_gru = 4, hru_index = 1
    caf::actor actor4; // index_gru = 5, hru_index = 1
    caf::actor actor5; // index_gru = 6, hru_index = 1
    caf::actor actor6; // index_gru = 7, hru_index = 1
    caf::actor actor7; // index_gru = 8, hru_index = 1
    initArrayOfOuputPartitions(output_partitions_vectors, num_partitions, num_gru, num_timestep);
    std::optional<int> return_value;
    gru_index = 1;
    int hru_index = 1;
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor0, gru_index, hru_index, dummy_data[0]).has_value() == false);
    IS_TRUE(output_partitions_vectors[0]->hru_info_and_data[0]->output_data.size() == 1);
    IS_TRUE(output_partitions_vectors[0]->hru_info_and_data[0]->index_gru == 1);
    IS_TRUE(output_partitions_vectors[0]->hru_info_and_data[0]->index_hru == 1);
    IS_TRUE(output_partitions_vectors[0]->hru_info_and_data[0]->hru_actor == actor0);
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor0, gru_index, hru_index, dummy_data[8]).value() == 0);

    gru_index = 2;
    hru_index = 1;
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor1, gru_index, hru_index, dummy_data[1]).has_value() == false);
    IS_TRUE(output_partitions_vectors[1]->hru_info_and_data[0]->output_data.size() == 1);
    IS_TRUE(output_partitions_vectors[1]->hru_info_and_data[0]->index_gru == 2);
    IS_TRUE(output_partitions_vectors[1]->hru_info_and_data[0]->index_hru == 1);
    IS_TRUE(output_partitions_vectors[1]->hru_info_and_data[0]->hru_actor == actor1);
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor1, gru_index, hru_index, dummy_data[9]).value() == 1);

    gru_index = 3;
    hru_index = 1;
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor2, gru_index, hru_index, dummy_data[2]).has_value() == false);
    IS_TRUE(output_partitions_vectors[2]->hru_info_and_data[0]->output_data.size() == 1);
    IS_TRUE(output_partitions_vectors[2]->hru_info_and_data[0]->index_gru == 3);
    IS_TRUE(output_partitions_vectors[2]->hru_info_and_data[0]->index_hru == 1);
    IS_TRUE(output_partitions_vectors[2]->hru_info_and_data[0]->hru_actor == actor2);
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor2, gru_index, hru_index, dummy_data[10]).value() == 2);

    gru_index = 4;
    hru_index = 1;
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor3, gru_index, hru_index, dummy_data[3]).has_value() == false);
    IS_TRUE(output_partitions_vectors[3]->hru_info_and_data[0]->output_data.size() == 1);
    IS_TRUE(output_partitions_vectors[3]->hru_info_and_data[0]->index_gru == 4);
    IS_TRUE(output_partitions_vectors[3]->hru_info_and_data[0]->index_hru == 1);
    IS_TRUE(output_partitions_vectors[3]->hru_info_and_data[0]->hru_actor == actor3);
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor3, gru_index, hru_index, dummy_data[11]).value() == 3);

    gru_index = 5;
    hru_index = 1;
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor4, gru_index, hru_index, dummy_data[4]).has_value() == false);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[0]->output_data.size() == 1);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[0]->index_gru == 5);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[0]->index_hru == 1);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[0]->hru_actor == actor4);
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor4, gru_index, hru_index, dummy_data[12]).has_value() == false);

    gru_index = 6;
    hru_index = 1;
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor5, gru_index, hru_index, dummy_data[5]).has_value() == false);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[1]->output_data.size() == 1);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[1]->index_gru == 6);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[1]->index_hru == 1);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[1]->hru_actor == actor5);
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor5, gru_index, hru_index, dummy_data[13]).has_value() == false);

    gru_index = 7;
    hru_index = 1;
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor6, gru_index, hru_index, dummy_data[6]).has_value() == false);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[2]->output_data.size() == 1);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[2]->index_gru == 7);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[2]->index_hru == 1);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[2]->hru_actor == actor6);
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor6, gru_index, hru_index, dummy_data[14]).has_value() == false);

    gru_index = 8;
    hru_index = 1;
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor7, gru_index, hru_index, dummy_data[7]).has_value() == false);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[3]->output_data.size() == 1);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[3]->index_gru == 8);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[3]->index_hru == 1);
    IS_TRUE(output_partitions_vectors[4]->hru_info_and_data[3]->hru_actor == actor7);
    IS_TRUE(addHRUOutput(output_partitions_vectors, actor7, gru_index, hru_index, dummy_data[15]).value() == 4);





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