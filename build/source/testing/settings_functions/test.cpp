#include "test.hpp"


#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " failed on line " << __LINE__ << std::endl; }



void testDistributedSettings() {
    std::cout << "Testing Distributed Settings\n";
    std::string failing_config_file = "somewhere/that/doesn't/exist.json\n";
    std::string config_file_1 = "config_files/Summa_Actors_Settings.json";
    int err;
    // Define Structures to hold settings
    Distributed_Settings distributed_settings;
    Summa_Actor_Settings summa_actor_settings;
    File_Access_Actor_Settings file_access_actor_settings;
    Job_Actor_Settings job_actor_settings;
    HRU_Actor_Settings hru_actor_settings;

    // Test with file that should fail
    err = read_settings_from_json(failing_config_file,
                                distributed_settings, 
                                summa_actor_settings, 
                                file_access_actor_settings,
                                job_actor_settings, 
                                hru_actor_settings);
    IS_TRUE(err == -1);

    // Test with real settings file
    err = read_settings_from_json(config_file_1,
                                distributed_settings, 
                                summa_actor_settings, 
                                file_access_actor_settings,
                                job_actor_settings, 
                                hru_actor_settings);

    IS_TRUE(distributed_settings.distributed_mode);
    IS_TRUE(distributed_settings.hostname == "7c75a36f6f33");
    IS_TRUE(distributed_settings.port == 4444);
    IS_TRUE(distributed_settings.total_hru_count == 12);
    IS_TRUE(distributed_settings.num_hru_per_batch == 2);
    IS_TRUE(distributed_settings.heartbeat_interval == 10);
    IS_TRUE(distributed_settings.lost_node_threshold == 3);
    std::vector<std::string> verification_vector = {"7c75a36", "7ca36"};
    IS_TRUE(distributed_settings.backup_servers == verification_vector);

    IS_TRUE(summa_actor_settings.max_gru_per_job == 100);
    
    IS_TRUE(file_access_actor_settings.num_vectors_in_output_manager == 1);
    
    IS_TRUE(job_actor_settings.file_manager_path == "/Summa-Actors-Settings-Output-Test-Fix/summa-output-fix/fileManager.txt")
    IS_TRUE(job_actor_settings.output_structure_size == 1)
    IS_TRUE(!job_actor_settings.output_csv)
    IS_TRUE(job_actor_settings.csv_path == "/Summa-Actors/Laugh-Tests/test_cases/output/actors/celia1990/")

    IS_TRUE(hru_actor_settings.print_output)
    IS_TRUE(hru_actor_settings.output_frequency == 100)


}