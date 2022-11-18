#include "test.hpp"

#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " failed on line " << __LINE__ << std::endl; }



void testDistributedSettings() {
    std::cout << "Testing Distributed Settings\n";
    
    // Test File Exists Function
    std::string failing_config_file = "somewhere/that/doesn't/exist.json\n";
    std::string config_file_1 = "config_files/Summa_Actors_Settings.json";
    int err;
    err = checkFileExists(failing_config_file);
    IS_TRUE(err == -1);
    err = checkFileExists(config_file_1);
    IS_TRUE(err == 0);

    Distributed_Settings distributed_settings = readDistributedSettings(config_file_1);
    IS_TRUE(distributed_settings.distributed_mode == true);
    IS_TRUE(distributed_settings.servers_list.size() == 2);
    IS_TRUE(distributed_settings.servers_list[0] == "localhost");
    IS_TRUE(distributed_settings.servers_list[1] == "some_other_host");
    IS_TRUE(distributed_settings.port == 1234);
    IS_TRUE(distributed_settings.total_hru_count == 12);
    IS_TRUE(distributed_settings.num_hru_per_batch == 2);
    std::cout << "Testing Distributed Settings Passed\n";

    Summa_Actor_Settings summa_actor_settings = readSummaActorSettings(config_file_1);
    IS_TRUE(summa_actor_settings.max_gru_per_job == 100);
    std::cout << "Testing Summa Actor Settings Passed\n";

    File_Access_Actor_Settings file_access_actor_settings = readFileAccessActorSettings(config_file_1);
    IS_TRUE(file_access_actor_settings.num_vectors_in_output_manager == 2);
    std::cout << "Testing File Access Actor Settings Passed\n";

    Job_Actor_Settings job_actor_settings = readJobActorSettings(config_file_1);
    IS_TRUE(job_actor_settings.file_manager_path == "/Summa-Actors-Settings-Output-Test-Fix/summa-output-fix/fileManager.txt")
    IS_TRUE(job_actor_settings.output_csv)
    IS_TRUE(job_actor_settings.csv_path == "/Summa-Actors/Laugh-Tests/test_cases/output/actors/celia1990/")
    std::cout << "Testing Job Actor Settings Passed\n";

    HRU_Actor_Settings hru_actor_settings = readHRUActorSettings(config_file_1);
    IS_TRUE(!hru_actor_settings.print_output)
    IS_TRUE(hru_actor_settings.output_frequency == 100)
    std::cout << "Testing HRU Actor Settings Passed\n";

}