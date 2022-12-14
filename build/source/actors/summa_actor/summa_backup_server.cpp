#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "message_atoms.hpp"
#include "summa_backup_server.hpp"
#include "global.hpp"
#include <chrono>
#include <thread>
namespace caf {

behavior summa_backup_server_init(stateful_actor<summa_server_state>* self, Distributed_Settings distributed_settings, 
    Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings) {
    aout(self) << "Backup Server Started\n";
    char host[HOST_NAME_MAX];
    gethostname(host, HOST_NAME_MAX);
    self->state.hostname = host;
    
    self->state.distributed_settings = distributed_settings;
    self->state.summa_actor_settings = summa_actor_settings; 
    self->state.file_access_actor_settings = file_access_actor_settings;
    self->state.job_actor_settings = job_actor_settings;
    self->state.hru_actor_settings = hru_actor_settings;

    self->state.csv_file_path = self->state.job_actor_settings.csv_path;
    self->state.csv_file_path += self->state.csv_output_name;


    self->set_down_handler([=](const down_msg& dm){
        if (self->state.current_server_actor == self) {
            aout(self) << "\n ******DOWN HANDLER CALLED******\n";
            aout(self) << "Lost Connection With A Connected Actor\n";
            std::optional<Client> client = self->state.client_container.getClient(dm.source);
            if (client.has_value()) {
                resolveLostClient(self, client.value());
            } else {
                resolveLostBackupServer(self, dm);
            }
        } else {
            if(dm.source == self->state.current_server) {
                aout(self) << "*** Lost Connection to Server\n";
                // check if we should become the new server
                if (std::get<0>(self->state.backup_servers_list[0]) == self) {
                    aout(self) << "*** Becoming New Server\n";
                    self->state.backup_servers_list.erase(self->state.backup_servers_list.begin());
                    self->become(summa_server(self));
                } else {
                    aout(self) << "Still A backup - but need to connect to new server\n";
                    connecting_backup(self, std::get<1>(self->state.backup_servers_list[0]), (uint16_t) self->state.distributed_settings.port);
                }
            }
        }
    });
    return {
        // Called by main to init the process
        [=](connect_atom, const std::string& host, uint16_t port) {
            connecting_backup(self, host, port);
        },
    };
}


void connecting_backup(stateful_actor<summa_server_state>* self, const std::string& host, uint16_t port) {
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
                self->state.current_server_actor = hdl;
                self->monitor(hdl);
                self->become(summa_backup_server(self, hdl));

            },
            [=](const error& err) {
                aout(self) << R"(*** cannot connect to ")" << host << R"(":)" << port
                   << " => " << to_string(err) << std::endl;
        });
}



behavior summa_backup_server(stateful_actor<summa_server_state>* self, const actor& server_actor) {
    aout(self) << "\nsumma backup server has started\n";
    self->send(server_actor, connect_as_backup_v, self, self->state.hostname);

    return {
        
        [=] (is_lead_server, caf::actor client_actor) {
            self->send(client_actor, is_lead_server_v, false, self);
        },

        [=](connect_as_backup) {
            aout(self) << "\nWe are now connected to the lead server\n";
        },

        // get the list of batches and clients from the lead server
        [=](update_with_current_state, Batch_Container& batch_container, Client_Container& client_container) {
            aout(self) << "\nReceived the containers from the lead server\n";
            self->state.batch_container = batch_container;
            self->state.client_container = client_container;
        },

        // We have a new backup server that was added to the server
        [=](update_backup_server_list, std::vector<std::tuple<caf::actor, std::string>> backup_servers) {
            aout(self) << "\nReceived the backup server list from the lead server\n";
            aout(self) << "Backup Server List = " << backup_servers << std::endl;
            self->state.backup_servers_list = backup_servers;
        },
        
        // New Client has been received
        [=](new_client, caf::actor client_actor, std::string hostname) {
            aout(self) << "\nReceived a new client from the lead server\n";
            self->monitor(client_actor);
            self->state.client_container.addClient(client_actor, hostname);
        },

        // Lead server had client removed
        [=](client_removed, Client& client) {
            aout(self) << "\nReceived a client removed message from the lead server\n";
            self->state.client_container.removeClient(client);
        },

        // Client finished a batch and the lead server has sent an update
        [=](done_batch, actor client_actor, Batch& batch) {
            aout(self) << "\nBatch: " << batch.getBatchID() << " is done\n";
            self->state.batch_container.updateBatch_success(batch);
        },

        // Client has been assigned new batch by the lead server
        [=](new_assigned_batch, actor client_actor, Batch& batch) {
            aout(self) << "\nNew Batch: " << batch.getBatchID() << " has been assigned\n";
            self->state.batch_container.setBatchAssigned(batch);
            self->state.client_container.setBatchForClient(client_actor, batch);
        },

        // Lead server has no more batches to distribute
        [=](no_more_batches, actor client_actor) {
            aout(self) << "\nNo more batches to distribute\n";
            self->state.client_container.setBatchForClient(client_actor, {});
        },

        // Simulation has finished
        [=](time_to_exit) {
            aout(self) << "Received time to exit\n";
            self->quit();
        },
    };
}
}