#include "test.hpp"
#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " \n **FAILED ON LINE " << __LINE__ << std::endl; }

void TEST_CLIENT() {
    std::cout << "Testing Output Container\n";

    int num_partitions = 5;
    int num_grus = 10;
    int num_timesteps_simulation = 100;
    int num_stored_timesteps = 10;
    
    Output_Container *output_container = new Output_Container(
        num_partitions, num_grus, num_stored_timesteps, num_timesteps_simulation);
    
    // Test getNumPartitions
    IS_TRUE(output_container->getNumPartitions() == 5);
    
    // Test findPartition
    IS_TRUE(output_container->findPartition(1) == 0);
    IS_TRUE(output_container->findPartition(2) == 0);
    IS_TRUE(output_container->findPartition(3) == 1);
    IS_TRUE(output_container->findPartition(4) == 1);
    IS_TRUE(output_container->findPartition(5) == 2);
    IS_TRUE(output_container->findPartition(6) == 2);
    IS_TRUE(output_container->findPartition(7) == 3);
    IS_TRUE(output_container->findPartition(8) == 3);
    IS_TRUE(output_container->findPartition(9) == 4);
    IS_TRUE(output_container->findPartition(10) == 4);

    // Test getOutputPartition
    Output_Partition* output_partition_1 = output_container->getOutputPartition(1);
    Output_Partition* output_partition_2 = output_container->getOutputPartition(2);
    Output_Partition* output_partition_3 = output_container->getOutputPartition(3);
    Output_Partition* output_partition_4 = output_container->getOutputPartition(4);
    Output_Partition* output_partition_5 = output_container->getOutputPartition(5);
    Output_Partition* output_partition_6 = output_container->getOutputPartition(6);
    Output_Partition* output_partition_7 = output_container->getOutputPartition(7);
    Output_Partition* output_partition_8 = output_container->getOutputPartition(8);
    Output_Partition* output_partition_9 = output_container->getOutputPartition(9);
    Output_Partition* output_partition_10 = output_container->getOutputPartition(10);

    IS_TRUE(output_partition_1->getStartGRUIndex() == 1);
    IS_TRUE(output_partition_1->getMaxGRUIndex() == 2);
    IS_TRUE(output_partition_1->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_1->getNumStoredTimesteps()  == 10);
    IS_TRUE(output_partition_1->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_2->getStartGRUIndex() == 1);
    IS_TRUE(output_partition_2->getMaxGRUIndex() == 2);
    IS_TRUE(output_partition_2->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_2->getNumStoredTimesteps()  == 10);
    IS_TRUE(output_partition_2->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_3->getStartGRUIndex() == 3);
    IS_TRUE(output_partition_3->getMaxGRUIndex() == 4);
    IS_TRUE(output_partition_3->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_3->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_3->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_4->getStartGRUIndex() == 3);
    IS_TRUE(output_partition_4->getMaxGRUIndex() == 4);
    IS_TRUE(output_partition_4->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_4->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_4->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_5->getStartGRUIndex() == 5);
    IS_TRUE(output_partition_5->getMaxGRUIndex() == 6);
    IS_TRUE(output_partition_5->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_5->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_5->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_6->getStartGRUIndex() == 5);
    IS_TRUE(output_partition_6->getMaxGRUIndex() == 6);
    IS_TRUE(output_partition_6->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_6->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_6->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_7->getStartGRUIndex() == 7);
    IS_TRUE(output_partition_7->getMaxGRUIndex() == 8);
    IS_TRUE(output_partition_7->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_7->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_7->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_8->getStartGRUIndex() == 7);
    IS_TRUE(output_partition_8->getMaxGRUIndex() == 8);
    IS_TRUE(output_partition_8->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_8->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_8->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_9->getStartGRUIndex() == 9);
    IS_TRUE(output_partition_9->getMaxGRUIndex() == 10);
    IS_TRUE(output_partition_9->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_9->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_9->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_10->getStartGRUIndex() == 9);
    IS_TRUE(output_partition_10->getMaxGRUIndex() == 10);
    IS_TRUE(output_partition_10->getNumLocalGRUs() == 2);
    IS_TRUE(output_partition_10->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_10->getRemainingTimesteps() == 100);



    // Test ready to write
    caf::actor actor_1; caf::actor actor_2; caf::actor actor_3; caf::actor actor_4;
    caf::actor actor_5; caf::actor actor_6; caf::actor actor_7; caf::actor actor_8;
    caf::actor actor_9; caf::actor actor_10; caf::actor actor_11; caf::actor actor_12;
    caf::actor actor_13; caf::actor actor_14; caf::actor actor_15; caf::actor actor_16;
    caf::actor actor_17; caf::actor actor_18; caf::actor actor_19; caf::actor actor_20;

    IS_TRUE(!output_partition_1->isReadyToWrite());
    output_partition_1->setGRUReadyToWrite(actor_1);
    IS_TRUE(!output_partition_1->isReadyToWrite());
    output_partition_1->setGRUReadyToWrite(actor_2);
    IS_TRUE(output_partition_1->isReadyToWrite());

    output_partition_1->updateTimeSteps();
    IS_TRUE(output_partition_1->getRemainingTimesteps() == 90);
    IS_TRUE(output_partition_1->getNumStoredTimesteps() == 10);

    std::vector<caf::actor> actors = output_partition_1->getReadyToWriteList();
    IS_TRUE(actors.size() == 2);
    IS_TRUE(actors[0] == actor_1);
    IS_TRUE(actors[1] == actor_2);
    output_partition_1->resetReadyToWriteList();
    IS_TRUE(!output_partition_1->isReadyToWrite());

    output_container->~Output_Container();


    // Test Failures
    num_partitions = 3;
    num_grus = 20;
    num_timesteps_simulation = 100;
    num_stored_timesteps = 10;
    
    output_container = new Output_Container(
        num_partitions, num_grus, num_stored_timesteps, num_timesteps_simulation);

    output_partition_1 = output_container->getOutputPartition(3);
    IS_TRUE(output_partition_1->getStartGRUIndex() == 1);
    IS_TRUE(output_partition_1->getMaxGRUIndex() == 6);
    IS_TRUE(output_partition_1->getNumLocalGRUs() == 6);
    IS_TRUE(output_partition_1->getNumStoredTimesteps() == 10);

    IS_TRUE(!output_partition_1->isReadyToWrite());
    output_partition_1->setGRUReadyToWrite(actor_1);
    IS_TRUE(!output_partition_1->isReadyToWrite());
    output_partition_1->setGRUReadyToWrite(actor_2);
    IS_TRUE(!output_partition_1->isReadyToWrite());
    output_partition_1->addFailedGRUIndex(3);
    IS_TRUE(!output_partition_1->isReadyToWrite());
    output_partition_1->setGRUReadyToWrite(actor_4);
    IS_TRUE(!output_partition_1->isReadyToWrite());
    output_partition_1->setGRUReadyToWrite(actor_5);
    IS_TRUE(!output_partition_1->isReadyToWrite());
    output_partition_1->setGRUReadyToWrite(actor_6);
    IS_TRUE(output_partition_1->isReadyToWrite());

    output_partition_2 = output_container->getOutputPartition(8);
    output_partition_2->addFailedGRUIndex(7);
    output_partition_2->addFailedGRUIndex(8);
    output_partition_2->addFailedGRUIndex(12);

    output_partition_3 = output_container->getOutputPartition(13);
    output_partition_3->addFailedGRUIndex(14);
    output_partition_3->addFailedGRUIndex(17);


    output_container->reconstruct();

    std::vector<int> comparison_vector = {3, 7, 8, 12, 14, 17};
    std::vector<int> failed_gru_indices = output_container->getFailedGRUIndexList();
    IS_TRUE(failed_gru_indices.size() == 6);
    for (int i = 0; i < failed_gru_indices.size(); i++) {
        IS_TRUE(failed_gru_indices[i] == comparison_vector[i]);
    }

    output_partition_1 = output_container->getOutputPartition(3);
    output_partition_2 = output_container->getOutputPartition(7);
    output_partition_3 = output_container->getOutputPartition(8);
    output_partition_4 = output_container->getOutputPartition(12);
    output_partition_5 = output_container->getOutputPartition(14);
    output_partition_6 = output_container->getOutputPartition(17);

    IS_TRUE(output_partition_1->getStartGRUIndex() == 3);
    IS_TRUE(output_partition_1->getMaxGRUIndex() == 3);
    IS_TRUE(output_partition_1->getNumLocalGRUs() == 1);
    IS_TRUE(output_partition_1->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_1->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_2->getStartGRUIndex() == 7);
    IS_TRUE(output_partition_2->getMaxGRUIndex() == 7);
    IS_TRUE(output_partition_2->getNumLocalGRUs() == 1);
    IS_TRUE(output_partition_2->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_2->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_3->getStartGRUIndex() == 8);
    IS_TRUE(output_partition_3->getMaxGRUIndex() == 8);
    IS_TRUE(output_partition_3->getNumLocalGRUs() == 1);
    IS_TRUE(output_partition_3->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_3->getRemainingTimesteps() == 100);
    
    IS_TRUE(output_partition_4->getStartGRUIndex() == 12);
    IS_TRUE(output_partition_4->getMaxGRUIndex() == 12);
    IS_TRUE(output_partition_4->getNumLocalGRUs() == 1);
    IS_TRUE(output_partition_4->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_4->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_5->getStartGRUIndex() == 14);
    IS_TRUE(output_partition_5->getMaxGRUIndex() == 14);
    IS_TRUE(output_partition_5->getNumLocalGRUs() == 1);
    IS_TRUE(output_partition_5->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_5->getRemainingTimesteps() == 100);

    IS_TRUE(output_partition_6->getStartGRUIndex() == 17);
    IS_TRUE(output_partition_6->getMaxGRUIndex() == 17);
    IS_TRUE(output_partition_6->getNumLocalGRUs() == 1);
    IS_TRUE(output_partition_6->getNumStoredTimesteps() == 10);
    IS_TRUE(output_partition_6->getRemainingTimesteps() == 100);









    


}
