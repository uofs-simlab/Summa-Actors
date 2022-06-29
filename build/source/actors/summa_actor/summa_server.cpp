#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include <string>
#include "batch_manager.hpp"
#include "summa_server.hpp"
#include "message_atoms.hpp"
#include "global.hpp"


namespace caf {

behavior summa_server(stateful_actor<summa_server_state>* self, std::string config_path) {
    aout(self) << "Summa Server has Started \n";
    self->state.config_path = config_path;

    self->state.total_hru_count = getSettings(self->state.config_path, "SimulationSettings", "total_hru_count", 
		self->state.total_hru_count).value_or(-1);
    if (self->state.total_hru_count == -1) {
        aout(self) << "ERROR: With total_hru_count - CHECK Summa_Actors_Settings.json\n";
    }
    self->state.num_hru_per_batch = getSettings(self->state.config_path, "SimulationSettings", "num_hru_per_batch", 
		self->state.num_hru_per_batch).value_or(-1);
    if (self->state.num_hru_per_batch == -1) {
        aout(self) << "ERROR: With num_hru_per_batch - CHECK Summa_Actors_Settings.json\n";
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
            aout(self) << "Client finished batch: " << batch_id << "\n";
            self->state.batch_list[batch_id]->solvedBatch();
            self->state.batches_solved++;
            self->state.batches_remaining = self->state.batch_list.size() - self->state.batches_solved;

            aout(self) << "\n****************************************\n";
            aout(self) << "Batches Solved = " << self->state.batches_solved << "\n";
            aout(self) << "Batches Remaining = " << self->state.batches_remaining << "\n";
            aout(self) << "****************************************\n";

            // Find a new batch
            Batch *batch_to_send = getUnsolvedBatch(self);

            if (batch_to_send == NULL) {
                aout(self) << "We Are Done - Telling Clients to exit \n";
                for (std::vector<int>::size_type i = 0; i < self->state.client_list.size(); i++) {
                    self->send(self->state.client_list[i]->getActor(), time_to_exit_v);
                }

                self->quit();
                return;

            } else {
                self->send(client, batch_v, batch_to_send->getBatchID(), batch_to_send->getStartHRU(), 
                    batch_to_send->getNumHRU(), self->state.config_path);
            }

        }
    };
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
