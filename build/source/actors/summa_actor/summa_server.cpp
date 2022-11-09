#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include <string>
#include "batch_manager.hpp"
#include "summa_server.hpp"
#include "message_atoms.hpp"
#include "global.hpp"
#include <optional>
#include <iostream>
#include <thread>
#include <chrono>


namespace caf {

behavior summa_server(stateful_actor<summa_server_state>* self, Distributed_Settings distributed_settings, 
    Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings) {
        
    aout(self) << "Summa Server has Started \n";
    self->state.distributed_settings = distributed_settings;
    self->state.summa_actor_settings = summa_actor_settings; 
    self->state.file_access_actor_settings = file_access_actor_settings;
    self->state.job_actor_settings = job_actor_settings;
    self->state.hru_actor_settings = hru_actor_settings;

    self->state.client_container = new Client_Container(self->state.distributed_settings.lost_node_threshold);
    self->state.batch_container = new Batch_Container(
            self->state.distributed_settings.total_hru_count,
            self->state.distributed_settings.num_hru_per_batch);
    
    self->state.batch_container->printBatches();

    initializeCSVOutput(self->state.job_actor_settings.csv_path, self->state.csv_output_name);

     // Start the heartbeat actor after a client has connected
    self->state.health_check_reminder_actor = self->spawn(cleint_health_check_reminder);
    self->send(self->state.health_check_reminder_actor, 
        start_health_check_v, self, self->state.distributed_settings.heartbeat_interval);

    return {
        /**
         * @brief A message from a client requesting to connect
         * 
         * @param client the actor_ref of the client_actor 
         * (used to send messages to the client_actor)
         * @param hostname human readable hostname of the machine that the actor is running on
         */
        [=](connect_to_server, actor client_actor, std::string hostname) {

            aout(self) << "Actor trying to connect with hostname " << hostname << "\n";
            self->state.client_container->addClient(client_actor, hostname);

            // Tell client they are connected
            self->send(client_actor, connect_to_server_v, self->state.client_container->getClientID(client_actor), 
                self->state.summa_actor_settings, self->state.file_access_actor_settings, self->state.job_actor_settings, 
                self->state.hru_actor_settings);
            
            std::optional<Batch> batch = self->state.batch_container->assignBatch(hostname, client_actor);
            if (batch.has_value()) {
                self->state.client_container->updateCurrentBatch(
                    self->state.client_container->getClientID(client_actor),
                    batch.value().getBatchID());
                self->send(client_actor, batch.value());
            } else {
                aout(self) << "no more batches left to assign\n";
                aout(self) << "we are not done yet. Clients could Fail\n";
            }
 
        },

        /**
         * @brief Construct a new [=] object
         * 
         * @param client_actor 
         * @param client_id 
         * @param batch 
         */
        [=](done_batch, actor client_actor, int client_id, Batch& batch) {
            aout(self) << "Received Completed Batch From Client\n";
    
            aout(self) << batch.toString() << "\n\n";

            self->state.batch_container->updateBatch_success(batch, self->state.csv_output_name);
            aout(self) << "******************\n" << "Batches Remaining: " << 
                self->state.batch_container->getBatchesRemaining() << "\n******************\n\n";

            std::optional<Batch> new_batch = self->state.batch_container->assignBatch(
                self->state.client_container->getHostname_ByClientID(client_id), client_actor);
            
            if (new_batch.has_value()) {
                
                self->send(client_actor, new_batch.value());
            
            } else {
                
                if (self->state.batch_container->getBatchesRemaining() > 0) {
                    
                    aout(self) << "no more batches left to assign\n";
                    aout(self) << "Keeping Client connected because other clients could Fail\n";

                    self->state.client_container->setAssignedBatch(client_id, false);

                } else {
                    aout(self) << "Telling Clients To Exit\n"; 
                    while(!self->state.client_container->isEmpty()) {
                        Client client = self->state.client_container->removeClient_fromBack();

                        caf::actor client_actor =  client.getActor();
                        self->send(client_actor, time_to_exit_v);
                    }

                    aout(self) << "SERVER EXITING!!\n";
                    self->quit();
                }
            }
        },

        // check for lost clients, send all connected a message
        [=](check_on_clients) {
            // Loop Through All Clients To see if any are lost
            if (self->state.client_container->checkForLostClients()) {
                self->state.client_container->reconcileLostBatches(self->state.batch_container);
                std::optional<Client> client = self->state.client_container->findIdleClient();
                if(client.has_value()) {
                    std::optional<Batch> new_batch = self->state.batch_container->assignBatch(
                        self->state.client_container->getHostname_ByClientID(client.value().getID()), 
                        client.value().getActor());
            
                    if (new_batch.has_value()) {
            
                        self->send(client.value().getActor(), new_batch.value());
            
                    }
                }
            }
           sendClientsHeartbeat(self);

            self->send(self->state.health_check_reminder_actor, 
                start_health_check_v, self, self->state.distributed_settings.heartbeat_interval);
        },

        // Received heartbeat from client
        [=](heartbeat, int client_id) {
            aout(self) << "Received HeartBeat From: " << client_id << "\n";
            self->state.client_container->decrementLostPotential(client_id);
        },
    };
}

void sendClientsHeartbeat(stateful_actor<summa_server_state>* self) {
    std::vector<Client> connected_clients = self->state.client_container->getConnectedClientList();
    for(auto client = begin(connected_clients); client != end(connected_clients); ++client) {
        self->send(client->getActor(), heartbeat_v);
    }
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



behavior cleint_health_check_reminder(event_based_actor* self) {
    return {

        [=](start_health_check, caf::actor summa_server, int sleep_duration) {
            std::this_thread::sleep_for(std::chrono::seconds(sleep_duration));
            self->send(summa_server, check_on_clients_v);
        },
    };

}

} // end namespace
