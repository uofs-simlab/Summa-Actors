#include "summa_client.hpp"
using namespace caf;

behavior SummaClient::make_behavior() {
  running_ = true;
  char host[HOST_NAME_MAX];
  gethostname(host, HOST_NAME_MAX);
  hostname_ = host;

  for (auto host : distributed_settings_.servers_list_) {
    auto server = self_->system().middleman().remote_actor(host, 
                                           distributed_settings_.port_);
    if (!server) {
      self_->println("Failed To Connect To Server\n"); 
      return {};
    }
    self_->println("Connected to Server\n");
    // self->state.servers.push_back(server);
    current_server_actor_ = *server;
    current_server_ = actor_cast<strong_actor_ptr>(*server);
  }

  self_->mail(connect_to_server_v, self_, 
             hostname_).send(current_server_actor_);
    return {
        // Response from the server on successful connection
        [=](connect_to_server, 
            Settings settings,
            // SummaActorSettings summa_actor_settings, 
            // FileAccessActorSettings file_access_actor_settings,
            // JobActorSettings job_actor_settings, 
            // HRUActorSettings hru_actor_settings,
            std::vector<std::tuple<caf::actor, std::string>> backup_servers) {
            
            self_->println("Successfully Connected to Server Actor \n"); 
            // summa_actor_settings_ = summa_actor_settings;
            // file_access_actor_settings_ = file_access_actor_settings;
            // job_actor_settings_ = job_actor_settings;
            // hru_actor_settings_ = hru_actor_settings;
            // backup_servers_list_ = backup_servers;
            settings_ = settings;
        },

        [=] (connect_atom, const std::string& host, uint16_t port) {
            self_->println("Received a connect request while running\n");
            // connecting(self, host, port);
        },

        [=] (is_lead_server, bool is_server, actor server_actor) {
            if (is_server) {
                self_->println("This is the lead server");
                // TODO update monitor here
                self_->monitor(server_actor, [this, serv=server_actor](const error& err) {
                    if (serv == current_server_) {
                        self_->println("*** Lost Connection to Server");
                        current_server_ = nullptr;
                        // try to connect to new server
                        if (backup_servers_list_.size() > 0) {
                        self_->println("Trying to connect to backup server\n");
                        std::this_thread::sleep_for(std::chrono::seconds(3)); 
                        // TODO: Not obvious where the code goes from here. 
                        // connecting(self, std::get<1>(self->state.backup_servers_list[0]), 
                        //            self->state.port);

                        } else {
                            self_->println("No backup servers available");

                        }
                    }
                });
                current_server_actor_ = server_actor;
                for(auto& server : servers_) {
                    if(actor_cast<actor>(server) == server_actor ) {
                        self_->println("Found Match\n");
                        current_server_ = server;
                        if (saved_batch_) {
                            saved_batch_ = false;
                            self_->mail(done_batch_v, self_, current_batch_).send(current_server_actor_);
                        }
                    }
                }
                servers_.clear();
                self_->mail(connect_to_server_v, self_, hostname_,getBatchSize()).send(current_server_actor_);
            } else {
                self_->println("This is not the lead server");
            }
        },

        [=](update_backup_server_list, std::vector<std::tuple<caf::actor, std::string>> backup_servers) {
            self_->println("Received the backup server list from the server\n");
            backup_servers_list_ = backup_servers;
        },

        // Received batch from server to compute
        [=](Batch& batch) {
            current_batch_ = batch;
            self_->println("\nReceived batch to compute\n");
            self_->println("BatchID = ", current_batch_.getBatchID(), "\n");
            self_->println("StartHRU = ", current_batch_.getStartHRU(), "\n");
            self_->println("NumHRU = ", current_batch_.getNumHRU(), "\n");

            summa_actor_ref_ = self_->spawn(actor_from_state<SummaActor>, 
                current_batch_.getStartHRU(), 
                current_batch_.getNumHRU(), 
                settings_,
                // summa_actor_settings_,
                // file_access_actor_settings_,
                // job_actor_settings_,
                // hru_actor_settings_,
                self_);
        },
        
        // Received completed batch information from the summa_actor 
        [=](done_batch, double run_time, double read_time, double write_time) {
            self_->println("Summa_Actor has finished, sending message to the server for another batch\n");
            self_->println("run_time = ", run_time, "\n");
            self_->println("read_time = ", read_time, "\n");
            self_->println("write_time = ", write_time, "\n");

            current_batch_.updateRunTime(run_time);
            current_batch_.updateReadTime(read_time);
            current_batch_.updateWriteTime(write_time);

            if(current_server_ == nullptr) {
                self_->println("Saving batch until we find a new lead server\n");
                saved_batch_ = true;
            } else {
                self_->mail(done_batch_v, self_, current_batch_,getBatchSize()).send(current_server_actor_);
            }
        },

        [=](time_to_exit) {
            self_->println("Client Exiting\n");
            self_->quit();
        }
        
    };
}
