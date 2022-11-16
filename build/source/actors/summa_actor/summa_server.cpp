#include "summa_server.hpp"

namespace caf {

behavior summa_server_init(stateful_actor<summa_server_state>* self, Distributed_Settings distributed_settings, 
    Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings) {

    aout(self) << "Summa Server has Started \n";
    self->set_down_handler([=](const down_msg& dm) {
        aout(self) << "Lost A Client\n";
        Client client = self->state.client_container->getClient(dm.source);
        aout(self) << "Lost Client: " << client.getID() << "\n";
        // Batch batch = client.getCurrentBatch();
        // std::optional<Client> idle_client = findIdleClient
        // if (idle_client.has_value) then send client batch
        // else mark batch as not done
    });
    
    self->state.distributed_settings = distributed_settings;
    self->state.summa_actor_settings = summa_actor_settings; 
    self->state.file_access_actor_settings = file_access_actor_settings;
    self->state.job_actor_settings = job_actor_settings;
    self->state.hru_actor_settings = hru_actor_settings;

    self->state.client_container = new Client_Container();
    self->state.batch_container = new Batch_Container(
            self->state.distributed_settings.total_hru_count,
            self->state.distributed_settings.num_hru_per_batch);
    
    self->state.batch_container->printBatches();

    initializeCSVOutput(self->state.job_actor_settings.csv_path, self->state.csv_output_name);

    return summa_server(self);

}

behavior summa_server(stateful_actor<summa_server_state>* self) {

    aout(self) << "Server is Running \n";

    return {

        // A message from a client requesting to connect
        [=](connect_to_server, actor client_actor, std::string hostname) {
            
            aout(self) << "Actor trying to connect with hostname " << hostname << "\n";
            self->state.client_container->addClient(client_actor, hostname);
            self->monitor(client_actor);
            // Tell client they are connected
            self->send(client_actor, connect_to_server_v, 
                self->state.summa_actor_settings, 
                self->state.file_access_actor_settings, 
                self->state.job_actor_settings, 
                self->state.hru_actor_settings);


            Client client = self->state.client_container->getClient(client_actor.address());
            
            std::optional<Batch> batch = self->state.batch_container->getUnsolvedBatch();
            if (batch.has_value()) {
                self->state.client_container->setBatchForClient(client_actor, batch.value());
                aout(self) << "SENDING: " << batch.value().toString() << "\n";
                self->send(client_actor, batch.value());
            } else {
                aout(self) << "no more batches left to assign\n";
                aout(self) << "we are not done yet. Clients could Fail\n";
            }
 
        },

        [=](connect_as_backup, actor backup_server, std::string hostname) {
            aout(self) << "Received Connection Request From a backup server\n";
            self->monitor(backup_server);
            self->state.backup_servers_list.push_back(std::make_tuple(backup_server, hostname));
            self->send(backup_server, connect_as_backup_v); // confirm connection with sender
            // Now we need to send the backup actor our current state
            self->send(backup_server, update_with_current_state_v, *self->state.batch_container, *self->state.client_container);

        }, 

        [=](done_batch, actor client_actor, Batch& batch) {
            aout(self) << "Received Completed Batch From Client\n";
            aout(self) << batch.toString() << "\n\n";\
            self->state.batch_container->updateBatch_success(batch, self->state.csv_output_name);
            printRemainingBatches(self);

            std::optional<Batch> new_batch = self->state.batch_container->getUnsolvedBatch();
            
            if (new_batch.has_value()) {
                // send clients new batch and update backup servers
                self->state.client_container->setBatchForClient(client_actor, new_batch.value());
                self->send(client_actor, new_batch.value());
                for (auto& backup_server : self->state.backup_servers_list) {
                    caf::actor backup_server_actor = std::get<0>(backup_server);
                    self->send(backup_server_actor, done_batch_v, client_actor, batch);
                    self->send(backup_server_actor, new_assigned_batch_v, client_actor, new_batch.value());
                }
            } else {
                // We may be done
                if (!self->state.batch_container->hasUnsolvedBatches()) {
                    // We are done
                    self->become(summa_server_exit(self));    
                }
    
                // No Batches left to assign but waiting for all clients to finish
                aout(self) << "No batches left to assign - Waiting for All Clients to finish\n";
                self->state.client_container->setBatchForClient(client_actor, {});
                for (auto& backup_server : self->state.backup_servers_list) {
                    caf::actor backup_server_actor = std::get<0>(backup_server);
                    self->send(backup_server_actor, done_batch_v, client_actor, batch);
                    self->send(backup_server_actor, no_more_batches_v, client_actor);
                }
            }
        }, 
    };
}

behavior summa_server_exit(stateful_actor<summa_server_state>* self) {
    aout(self) << "SUMMA Simulation is complete\n";
    aout(self) << "Telling Clients to Exit\n";
    while(!self->state.client_container->isEmpty()) {
        Client client = self->state.client_container->removeClient_fromBack();
        caf::actor client_actor = client.getActor();
        self->send(client_actor, time_to_exit_v);
    }
    aout(self) << "Telling Backup Servers to Exit\n";
    for (auto& backup_server : self->state.backup_servers_list) {
        caf::actor backup_server_actor = std::get<0>(backup_server);
        self->send(backup_server_actor, time_to_exit_v);
    }
    self->quit();
    return {};
}


void initializeCSVOutput(std::string csv_output_path, std::string csv_output_name) {
    std::ofstream csv_output;
    std::string csv_file = csv_output_path += csv_output_name;
    csv_output.open(csv_file, std::ios_base::out);
    csv_output << 
        "Batch_ID,"  <<
        "Start_HRU," <<
        "Num_HRU,"   << 
        "Hostname,"  <<
        "Run_Time,"  <<
        "Read_Time," <<
        "Write_Time\n";
    csv_output.close();
}

void printRemainingBatches(stateful_actor<summa_server_state>* self) {
       aout(self) << "******************\n"  << "Batches Remaining: " << 
                  self->state.batch_container->getBatchesRemaining() << 
                  "\n******************\n\n";
}
} // end namespace
