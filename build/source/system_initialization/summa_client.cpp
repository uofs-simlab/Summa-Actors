#include "summa_client.hpp"


using namespace caf;

behavior SummaClientActor::make_behavior() {

  if (!server_hostname_.empty()) {
    self_->println("SummaClientActor Starting...");
    auto strong_server = self_->system().middleman().remote_actor(
        server_hostname_, settings_.distributed_settings_.port_);
    if (!strong_server) {
      self_->println("Failed to connect to server");
      return {};
    }
    server_ = actor_cast<actor>(*strong_server);
    self_->monitor(server_, [this](const error& err) {
      self_->println("Lost Connection to Server");
      exit(EXIT_FAILURE);
    });

    self_->println("SummaClientActor: Connected to Server");

  }
  
  gethostname(hostname_, HOST_NAME_MAX);
  self_->mail(connect_atom_v, hostname_).send(server_);

  return {
    [=](Batch& batch) {
      current_batch_ = batch;
      self_->println("\nReceived batch to compute\n");

      self_->spawn(actor_from_state<SummaActor>, current_batch_.getStartHRU(),
          current_batch_.getNumHRU(), settings_, self_);
    }
  };
}





// namespace caf {

// behavior summa_client(stateful_actor<summa_client_state>* self,
//                       Distributed_Settings distributed_settings) {
//   self->state.running = true;
//   self->state.distributed_settings = distributed_settings;
//   char host[HOST_NAME_MAX];
//   gethostname(host, HOST_NAME_MAX);
//   self->state.hostname = host;

//   self->set_down_handler([=](const down_msg& dm){
//     if(dm.source == self->state.current_server) {
//       aout(self) << "*** Lost Connection to Server" << std::endl;
//       self->state.current_server = nullptr;
//       // try to connect to new server
//       if (self->state.backup_servers_list.size() > 0) {
//         aout(self) << "Trying to connect to backup server\n";
//         std::this_thread::sleep_for(std::chrono::seconds(3)); 
//         // TODO: Not obvious where the code goes from here. 
//         // connecting(self, std::get<1>(self->state.backup_servers_list[0]), 
//         //            self->state.port);

//       } else {
//         aout(self) << "No backup servers available" << std::endl;
//       }
//     }
//   });

//   for (auto host : distributed_settings.servers_list) {
//     auto server = self->system().middleman().remote_actor(host, 
//                                            distributed_settings.port);
//     if (!server) {
//       aout(self) << "Failed To Connect To Server\n"; 
//       return {};
//     }
//     aout(self) << "Connected to Server\n";
//     // self->state.servers.push_back(server);
//     self->state.current_server_actor = *server;
//     self->state.current_server = actor_cast<strong_actor_ptr>(*server);
//   }

//   self->send(self->state.current_server_actor, connect_to_server_v, self, 
//              self->state.hostname);
//     return {
//         // Response from the server on successful connection
//         [=](connect_to_server, 
//             Summa_Actor_Settings summa_actor_settings, 
//             File_Access_Actor_Settings file_access_actor_settings,
//             Job_Actor_Settings job_actor_settings, 
//             HRU_Actor_Settings hru_actor_settings,
//             std::vector<std::tuple<caf::actor, std::string>> backup_servers) {
            
//             aout(self) << "Successfully Connected to Server Actor \n"; 
//             self->state.summa_actor_settings = summa_actor_settings;
//             self->state.file_access_actor_settings = file_access_actor_settings;
//             self->state.job_actor_settings = job_actor_settings;
//             self->state.hru_actor_settings = hru_actor_settings;
//             self->state.backup_servers_list = backup_servers;
//         },

//         [=] (connect_atom, const std::string& host, uint16_t port) {
//             aout(self) << "Received a connect request while running\n";
//             // connecting(self, host, port);
//         },

//         [=] (is_lead_server, bool is_server, actor server_actor) {
//             if (is_server) {
//                 aout(self) << "This is the lead server" << std::endl;
//                 self->monitor(server_actor);
//                 self->state.current_server_actor = server_actor;
//                 for(auto& server : self->state.servers) {
//                     if(actor_cast<actor>(server) == server_actor ) {
//                         aout(self) << "Found Match\n";
//                         self->state.current_server = server;
//                         if (self->state.saved_batch) {
//                             self->state.saved_batch = false;
//                             self->send(self->state.current_server_actor, done_batch_v, self, self->state.current_batch);
//                         }
//                     }
//                 }
//                 self->state.servers.clear();
//                 self->send(self->state.current_server_actor, connect_to_server_v, self, self->state.hostname);
//             } else {
//                 aout(self) << "This is not the lead server" << std::endl;
//             }
//         },

//         [=](update_backup_server_list, std::vector<std::tuple<caf::actor, std::string>> backup_servers) {
//             aout(self) << "Received the backup server list from the server\n";
//             self->state.backup_servers_list = backup_servers;
//         },

//         // Received batch from server to compute
//         [=](Batch& batch) {
//             self->state.current_batch = batch;
//             aout(self) << "\nReceived batch to compute\n";
//             aout(self) << "BatchID = " << self->state.current_batch.getBatchID() << "\n";
//             aout(self) << "StartHRU = " << self->state.current_batch.getStartHRU() << "\n";
//             aout(self) << "NumHRU = " << self->state.current_batch.getNumHRU() << "\n";

//             self->state.summa_actor_ref = self->spawn(summa_actor, 
//                 self->state.current_batch.getStartHRU(), 
//                 self->state.current_batch.getNumHRU(), 
//                 self->state.summa_actor_settings,
//                 self->state.file_access_actor_settings,
//                 self->state.job_actor_settings,
//                 self->state.hru_actor_settings,
//                 self);
//         },
        
//         // Received completed batch information from the summa_actor 
//         [=](done_batch, double run_time, double read_time, double write_time) {
//             aout(self) << "Summa_Actor has finished, sending message to the server for another batch\n";
//             aout(self) << "run_time = " << run_time << "\n";
//             aout(self) << "read_time = " << read_time << "\n";
//             aout(self) << "write_time = " << write_time << "\n";

//             self->state.current_batch.updateRunTime(run_time);
//             self->state.current_batch.updateReadTime(read_time);
//             self->state.current_batch.updateWriteTime(write_time);

//             if(self->state.current_server == nullptr) {
//                 aout(self) << "Saving batch until we find a new lead server\n";
//                 self->state.saved_batch = true;
//             } else {
//                 self->send(self->state.current_server_actor, done_batch_v, self, self->state.current_batch);
//             }
//         },

//         [=](time_to_exit) {
//             aout(self) << "Client Exiting\n";
//             self->quit();
//         }
        
//     };
// }
// }