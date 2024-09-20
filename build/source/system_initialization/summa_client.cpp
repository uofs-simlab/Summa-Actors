#include "summa_client.hpp"
#include <thread>

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
      server_ = nullptr;

      auto elem = connected_clients_.begin();
      elem = std::next(elem);

      if (elem->second.getActor().address() == self_->address()) {
        self_->println("I AM THE SERVER NOW");
        auto test_server = self_->spawn(actor_from_state<SummaServerActor>, 
            settings_);
        self_->mail(reinit_v).send(test_server);
        // auto res = self_->system().middleman().publish(
        //     self_, settings_.distributed_settings_.port_);
        // if (!res) {
        //   self_->println("SummaServerActor: Failed to publish actor on port {}",
        //                 settings_.distributed_settings_.port_);
        // }
        // self_->println("SummaServerActor Started on port {}", 
        //               settings_.distributed_settings_.port_);
      } else {
        // Sleep for a few seconds then attempt to connect
        // std::this_thread::sleep_for(std::chrono::seconds(5));
        // self_->println("Attempting to reconnect to server");
        // auto strong_server = self_->system().middleman().remote_actor(
        //     elem->second.getHostname(), settings_.distributed_settings_.port_);
        // if (!strong_server) {
        //   self_->println("Failed to connect to server");
        //   return;
        // }
        // server_ = actor_cast<actor>(*strong_server);
        // self_->mail("Hello").send(server_);
      }
      // self_->println("Trying to designate a new server");
      // self_->println("How about {} \n\twith hostname {}", 
      //     to_string(elem->second.getActor().address()), 
      //     elem->second.getHostname());

      // self_->mail("Hello").send(elem->second.getActor());
      

    });

    self_->println("SummaClientActor: Connected to Server");
    gethostname(hostname_, HOST_NAME_MAX);
  } else {
    std::strncpy(hostname_, "local", sizeof(hostname_) - 1); // Use std::strncpy to copy the string literal
    hostname_[sizeof(hostname_) - 1] = '\0'; // Ensure null-termination
  }
  
  self_->mail(connect_atom_v, hostname_).send(server_);

  return {
    [=](Batch& batch) {
      current_batch_ = batch;
      self_->println("\nReceived batch to compute\n");
      settings_.job_actor_settings_.file_manager_path_ = batch.getFileManager();
      self_->println("File Manager: {}", 
          settings_.job_actor_settings_.file_manager_path_);
      self_->spawn(actor_from_state<SummaActor>, current_batch_.getStartHRU(),
          current_batch_.getNumHRU(), settings_, self_);
    },

    [=](done_batch, double run_time, double read_time, double write_time) {
      self_->println("SummaClientActor: SummaActor finished batch");
      self_->println("\tStats: run_time = {}, read_time = {}, write_time = {}",
                     run_time, read_time, write_time);
      current_batch_.updateRunTime(run_time);
      current_batch_.updateReadTime(read_time);
      current_batch_.updateWriteTime(write_time);
      if(server_ == nullptr) {
        self_->println("Saving batch until we find a new lead server\n");
        saved_batch_ = true;
      } else {
        self_->mail(done_batch_v, current_batch_).send(server_);
      }
    },

    // Update Connected Clients List
    [=](std::unordered_map<caf::actor, Client> connected_clients) {
      self_->println("SummaClientActor: Updating Connected Clients List");
      connected_clients_ = connected_clients;
    },

    // Update Batch List
    [=](std::vector<BatchContainer> simulations) {
      self_->println("SummaClientActor: Updating Batch List");
      simulations_ = std::move(simulations);
    },

    [=](time_to_exit) {
      self_->println("Client Exiting");
      self_->quit();
    },

    // Test function
    [=](std::string msg) {
      auto client_actor = actor_cast<actor>(self_->current_sender());
      if (client_actor.address() == self_->address()) {
        self_->println("Message was from myself");
      }
      self_->println("SummaClientActor: Received Message: {}", msg);
    }


  };
}