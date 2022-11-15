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

     // Start the heartbeat actor after a client has connected
    // self->state.health_check_reminder_actor = self->spawn(client_health_check_reminder);
    // self->send(self->state.health_check_reminder_actor, 
    //     start_health_check_v, self, self->state.distributed_settings.heartbeat_interval);

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
                self->state.summa_actor_settings, self->state.file_access_actor_settings, self->state.job_actor_settings, 
                self->state.hru_actor_settings);


            Client client = self->state.client_container->getClient(client_actor.address());
            
            std::optional<Batch> batch = self->state.batch_container->assignBatch(&client);
            if (batch.has_value()) {
                self->state.client_container->setBatchForClient(client_actor, &batch.value());
                self->send(client_actor, batch.value());
            } else {
                aout(self) << "no more batches left to assign\n";
                aout(self) << "we are not done yet. Clients could Fail\n";
            }
 
        },

        [=](connect_as_backup, actor backup_server) {
            aout(self) << "Received Connection Request From a backup server\n";

        }, 

        [=](done_batch, actor client_actor, Batch& batch) {
            aout(self) << "Received Completed Batch From Client\n";

            aout(self) << batch.toString() << "\n\n";

            Client client = self->state.client_container->getClient(client_actor.address());

            self->state.batch_container->updateBatch_success(batch, self->state.csv_output_name);
            aout(self) << "******************\n" 
                       << "Batches Remaining: " << 
                       self->state.batch_container->getBatchesRemaining() << 
                       "\n******************\n\n";

            std::optional<Batch> new_batch = self->state.batch_container->assignBatch(&client);
            
            if (new_batch.has_value()) {
                
                self->send(client_actor, new_batch.value());
            
            } else {
                
                if (self->state.batch_container->getBatchesRemaining() > 0) {
                    
                    aout(self) << "no more batches left to assign\n";
                    aout(self) << "Keeping Client connected because other clients could Fail\n";
                    self->state.client_container->setBatchForClient(client_actor, {});
                    // self->state.client_container->setAssignedBatch(client_id, false);

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

void connecting(stateful_actor<summa_server_state>* self, const std::string& host, uint16_t port) {
    self->state.current_server = nullptr;

    auto mm = self->system().middleman().actor_handle();
    self->request(mm, infinite, connect_atom_v, host, port)
        .await(
            [=](const node_id&, strong_actor_ptr serv,
                const std::set<std::string>& ifs) {
                if (!serv) {
                    aout(self) << R"(*** no server found at ")" << host << R"(":)" << port
                     << std::endl;
                    return;
                }
                if (!ifs.empty()) {
                    aout(self) << R"(*** typed actor found at ")" << host << R"(":)"
                        << port << ", but expected an untyped actor " << std::endl;
                    return;
                }
                aout(self) << "*** successfully connected to server" << std::endl;
                self->state.current_server = serv;
                auto hdl = actor_cast<actor>(serv);
                self->monitor(hdl);
                },
            [=](const error& err) {
                aout(self) << R"(*** cannot connect to ")" << host << R"(":)" << port
                   << " => " << to_string(err) << std::endl;
        });
}



behavior client_health_check_reminder(event_based_actor* self) {
    return {

        [=](start_health_check, caf::actor summa_server, int sleep_duration) {
            std::this_thread::sleep_for(std::chrono::seconds(sleep_duration));
            self->send(summa_server, check_on_clients_v);
        },
    };

}

} // end namespace
