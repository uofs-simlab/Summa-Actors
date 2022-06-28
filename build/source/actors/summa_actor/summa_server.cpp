#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "json.hpp"
#include <string>
#include "batch_manager.hpp"
#include "summa_server.hpp"
#include "message_atoms.hpp"


using json = nlohmann::json;


namespace caf {

behavior summa_server(stateful_actor<summa_server_state>* self, std::string config_path) {
    aout(self) << "Summa Server has Started \n";
    self->state.config_path = config_path;
    if (parseSettings(self, config_path) == -1) {
        aout(self) << "ERROR WITH JSON SETTINGS FILE!!\n";
        aout(self) << "Summa_Server_Actor Exiting\n";
        self->quit();
    } else {
        aout(self) << "-------------------------------------\n";
        aout(self) << "-----Summa_Server_Actor_Settings-----\n";
        aout(self) << "Total HRUs to compute = " << self->state.total_hru_count << "\n";
        aout(self) << "Number of HRUs per batch = " << self->state.num_hru_per_batch << "\n";
        aout(self) << "-------------------------------------\n";
    }

    aout(self) << "Assembling HRUs into Batches\n";
    if (assembleBatches(self) == -1) {
        aout(self) << "ERROR: assembleBatches\n";
    } else {
        aout(self) << "HRU Batches Assembled, Ready For Clients to Connect \n";

        for (std::vector<int>::size_type i = 0; i < self->state.batch_list.size(); i++) {
            self->state.batch_list[i]->printBatchInfo();
        }
    }

    return {
        [=](connect_to_server, actor client, std::string hostname) {
            aout(self) << "Actor trying to connect with hostname " << hostname << "\n";
            int client_id = self->state.client_list.size(); // So we can lookup the client in O(1) time 
            self->state.client_list.push_back(new Client(client_id, client, hostname));
            Batch *batch_to_send = getUnsolvedBatch(self);

            batch_to_send->assignedBatch();

            self->send(client, batch_v, batch_to_send->getBatchID(), batch_to_send->getStartHRU(), 
                batch_to_send->getNumHRU(), self->state.config_path);
        },

        [=](done_batch, actor client, double duration, int batch_id) {
            aout(self) << "Client has Solved Batch " << batch_id << "\n";
            self->state.batch_list[batch_id]->solvedBatch();

            Batch *batch_to_send = getUnsolvedBatch(self);

            if (batch_to_send == NULL) {
                aout(self) << "We Are Done - Telling Clients to exit \n";
                for (std::vector<int>::size_type i = 0; i < self->state.client_list.size(); i++) {
                    self->send(self->state.client_list[i]->getActor(), time_to_exit_v);
                }

            } else {
                self->send(client, batch_v, batch_to_send->getBatchID(), batch_to_send->getStartHRU(), 
                    batch_to_send->getNumHRU(), self->state.config_path);
            }

        }
    };
}

int parseSettings(stateful_actor<summa_server_state>* self, std::string config_path) {
    json settings;
    std::string SummaActorsSettings = "/Summa_Actors_Settings.json";
	std::ifstream settings_file(config_path + SummaActorsSettings);
    settings_file >> settings;
    settings_file.close();

    if (settings.find("SimulationSettings") != settings.end()) {
        json simulation_settings = settings["SimulationSettings"];
        
        if (simulation_settings.find("total_hru_count") != simulation_settings.end()) {
            self->state.total_hru_count = simulation_settings["total_hru_count"];
        } else {
            aout(self) << "ERROR Finding Total HRU Count - Exiting because the number " << 
                "of HRUs to compute is unknown\n";
            return -1;
        }

        if (simulation_settings.find("num_hru_per_batch") != simulation_settings.end()) {
            self->state.num_hru_per_batch = simulation_settings["num_hru_per_batch"];
        } else {
            aout(self) << "ERROR Finding initial value for the number of HRUs in a batch " <<
            "setting num_hru_per_batch to default value of 500\n";
            self->state.num_hru_per_batch = 500;
        }

        return 0;

    } else {
        aout(self) << "Error Finding SimulationSettings in JSON File - Exiting as Num";
        return -1;
    }
}

int assembleBatches(stateful_actor<summa_server_state>* self) {
    int remaining_hru_to_batch = self->state.total_hru_count;
    int count_index = 0; // this is like the offset for slurm bash scripts
    int start_hru = 1;

    while(remaining_hru_to_batch > 0) {
        if (self->state.num_hru_per_batch > remaining_hru_to_batch) {
            self->state.batch_list.push_back(new Batch(count_index, start_hru, 
                remaining_hru_to_batch));
            remaining_hru_to_batch = 0;
        } else {
            self->state.batch_list.push_back(new Batch(count_index, start_hru, 
                self->state.num_hru_per_batch));
            
            remaining_hru_to_batch -= self->state.num_hru_per_batch;
            start_hru += self->state.num_hru_per_batch;
            count_index += 1;
        }
    }

    return 0;
}

Batch* getUnsolvedBatch(stateful_actor<summa_server_state>* self) {

    // Find the first unassigned batch
    for (std::vector<int>::size_type i = 0; i < self->state.batch_list.size(); i++) {
        if (self->state.batch_list[i]->getBatchStatus() == unassigned) {
            return self->state.batch_list[i];
        }
    }

    return NULL;

}

} // end namespace
