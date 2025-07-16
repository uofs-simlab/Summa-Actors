#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "message_atoms.hpp"
#include "summa_backup_server.hpp"
#include <chrono>
#include <thread>
namespace caf {

behavior SummaServer::summa_backup_server_init() {
    self_->println("Backup Server Started\n");
    char host[HOST_NAME_MAX];
    gethostname(host, HOST_NAME_MAX);
    hostname_ = host;
    
    return {
        // Called by main to init the process
        [=](connect_atom, const std::string& host, uint16_t port) {
            connecting_backup(host, port);
        },
    };
}


void SummaServer::connecting_backup(const std::string& host, uint16_t port) {
    current_server_ = nullptr;

    auto mm = self_->system().middleman().actor_handle();
    self_->mail(connect_atom_v, host, port)
        .request(mm, infinite)
        .await(
            [=](const node_id&, strong_actor_ptr serv,
                const std::set<std::string>& ifs) {
                if (!serv) {
                    self_->println(R"(*** no server found at "{}":{})", host, port);
                    return;
                }
                if (!ifs.empty()) {
                    self_->println(R"(*** typed actor found at "{}":{}, but expected an untyped actor )",
                        host, port);
                    return;
                }
                self_->println("*** successfully connected to server");
                current_server_ = serv;
                auto hdl = actor_cast<actor>(serv);
                current_server_actor_ = hdl;
                self_->monitor(hdl, [this, source=hdl.address()](const error& err) {
                    if (current_server_actor_ == self_) {
                        self_->println("\n ******DOWN HANDLER CALLED******\n");
                        self_->println("Lost Connection With A Connected Actor\n");
                        std::optional<Client> client = client_container_.getClient(source);
                        if (client.has_value()) {
                            resolveLostClient(client.value());
                        } else {
                            resolveLostBackupServer(source);
                        }
                    } else {
                        if(source == current_server_) {
                            self_->println("*** Lost Connection to Server\n");
                            // check if we should become the new server
                            if (std::get<0>(backup_servers_list_[0]) == self_) {
                                self_->println("*** Becoming New Server\n");
                                backup_servers_list_.erase(backup_servers_list_.begin());
                                self_->become(summa_server());
                            } else {
                                self_->println("Still A backup - but need to connect to new server\n");
                                connecting_backup(std::get<1>(backup_servers_list_[0]), (uint16_t) settings_.distributed_settings_.port_);
                            }
                        }
                    }
                });

                self_->become(summa_backup_server());

            },
            [=](const error& err) {
                self_->println(R"(*** cannot connect to "{}":{})", host, port,
                   " => ", to_string(err));
        });
}



behavior SummaServer::summa_backup_server() {
    self_->println("\nsumma backup server has started\n");
    self_->mail(current_server_actor_, connect_as_backup_v, self_, hostname_).send(current_server_actor_);

    return {
        
        [=] (is_lead_server, caf::actor client_actor) {
            self_->mail(is_lead_server_v, false, self_).send(client_actor);
        },

        [=](connect_as_backup) {
            self_->println("\nWe are now connected to the lead server\n");
        },

        // get the list of batches and clients from the lead server
        [=](update_with_current_state, DynamicBatchContainer& batch_container, Client_Container& client_container) {
            self_->println("\nReceived the containers from the lead server\n");
            batch_container_ = batch_container;
            client_container_ = client_container;
        },

        // We have a new backup server that was added to the server
        [=](update_backup_server_list, std::vector<std::tuple<caf::actor, std::string>> backup_servers) {
            self_->println("\nReceived the backup server list from the lead server\n");
            self_->println("Backup Server List = {}", backup_servers);
            backup_servers_list_ = backup_servers;
        },
        
        // New Client has been received
        [=](new_client, caf::actor client_actor, std::string hostname) {
            self_->println("\nReceived a new client from the lead server\n");
            self_->monitor(client_actor, [this, source=client_actor.address()](const error& err) {
                    if (current_server_actor_ == self_) {
                        self_->println("\n ******DOWN HANDLER CALLED******\n");
                        self_->println("Lost Connection With A Connected Actor\n");
                        std::optional<Client> client = client_container_.getClient(source);
                        if (client.has_value()) {
                            resolveLostClient(client.value());
                        } else {
                            resolveLostBackupServer(source);
                        }
                    } else {
                        if(source == current_server_) {
                            self_->println("*** Lost Connection to Server\n");
                            // check if we should become the new server
                            if (std::get<0>(backup_servers_list_[0]) == self_) {
                                self_->println("*** Becoming New Server\n");
                                backup_servers_list_.erase(backup_servers_list_.begin());
                                self_->become(summa_server());
                            } else {
                                self_->println("Still A backup - but need to connect to new server\n");
                                connecting_backup(std::get<1>(backup_servers_list_[0]), (uint16_t) settings_.distributed_settings_.port_);
                            }
                        }
                    }
                });
            client_container_.addClient(client_actor, hostname);
        },

        // Lead server had client removed
        [=](client_removed, Client& client) {
            self_->println("\nReceived a client removed message from the lead server\n");
            client_container_.removeClient(client);
        },

        // Client finished a batch and the lead server has sent an update
        [=](done_batch, actor client_actor, Batch& batch) {
            self_->println("\nBatch: {} is done", batch.getBatchID());
            batch_container_.updateBatch_success(batch);
        },

        // Client has been assigned new batch by the lead server
        [=](new_assigned_batch, actor client_actor, Batch& batch) {
            self_->println("\nNew Batch: {} has been assigned", batch.getBatchID());
            batch_container_.setBatchAssigned(batch);
            client_container_.setBatchForClient(client_actor, batch);
        },

        // Lead server has no more batches to distribute
        [=](no_more_batches, actor client_actor) {
            self_->println("\nNo more batches to distribute\n");
            client_container_.setBatchForClient(client_actor, {});
        },

        // Simulation has finished
        [=](time_to_exit) {
            self_->println("Received time to exit\n");
            self_->quit();
        },
    };
}
}
