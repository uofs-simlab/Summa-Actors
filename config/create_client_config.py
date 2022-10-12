import json
import sys

def distributed_settings(hostname, port):
    distributed_settings = {
        "distributed-mode": True,
        "host":hostname,
        "port":port,
        "total_hru_count": 5,
        "num_hru_per_batch": 1,
        "heartbeat_interval": 20,
        "lost_node_threshold": 3
    }   
    return distributed_settings


settings_dict = {
    "Distributed_Settings": {}
}

hostname = sys.argv[1]
port = sys.argv[2]


settings_dict['Distributed_Settings'] = distributed_settings(hostname, int(port))
with open('Summa_Actors_Settings.json', 'w') as summa_actors_settings_file:
        json.dump(settings_dict, summa_actors_settings_file, indent=2)