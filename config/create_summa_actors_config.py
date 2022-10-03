import json

def distributed_settings():
    distributed_settings = {
        "distributed-mode": False,
        "host":"localhost",
        "port":4444
    }   
    return distributed_settings

def simulation_settings():
    simulation_settings = {
        "total_hru_count": 0,
        "num_hru_per_batch": 0
    }
    return simulation_settings

def summa_actor_settings():
    summa_actor_settings = {
        "OutputStructureSize": 250,
        "maxGRUPerJob":        250
    }
    return summa_actor_settings

def file_access_actor_settings():
    file_access_actor_settings = {
        "num_vectors_in_output_manager": 1
    }
    return file_access_actor_settings

def job_actor_settings():
    job_actor_settings = {
        "FileManagerPath" : "",
        "outputCSV": False,
        "csvPath": ""
    }
    return job_actor_settings

def hru_actor_settings():
    hru_actor_settings = {
        "printOutput": True,
        "outputFrequency": 1
    }
    return hru_actor_settings





def create_config():
    settings_dict = {
        "DistributedSettings": {},
        "SimulationSettings" : {},
        "SummaActor"         : {},
        "FileAccessActor"    : {},
        "JobActor"           : {},
        "HRUActor"           : {}
    }
    settings_dict['DistributedSettings'] = distributed_settings()
    settings_dict['SimulationSettings'] = simulation_settings()
    settings_dict['SummaActor'] = summa_actor_settings()
    settings_dict['FileAccessActor'] = file_access_actor_settings()
    settings_dict['JobActor'] = job_actor_settings()
    settings_dict['HRUActor'] = hru_actor_settings()
    with open('Summa_Actors_Settings.json', 'w') as summa_actors_settings_file:
        json.dump(settings_dict, summa_actors_settings_file, indent=2)




create_config()