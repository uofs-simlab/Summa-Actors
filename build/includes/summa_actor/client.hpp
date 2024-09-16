#pragma once
#include "caf/all.hpp"
#include <optional>
#include "batch.hpp"

class Client {
  private:
    // Info
    std::string hostname_;
    caf::actor client_actor_;
    
    // State
    std::optional<Batch> current_batch_;
    
    // Stats
    int batches_solved_;

  public:
    Client(caf::actor client_actor = nullptr, std::string hostname = "");
    // ####################################################################
    //                              Getters
    // ####################################################################
    inline caf::actor getActor() { return client_actor_; }
    inline std::string getHostname() { return hostname_; }
    inline std::optional<Batch> getBatch() { return current_batch_; }
    
    // ####################################################################
    //                              Setters
    // ####################################################################
    void setBatch(std::optional<Batch> batch) { current_batch_ = batch; }
    
    // ####################################################################
    //                              Methods
    // ####################################################################
    std::string toString();

    // ####################################################################
    //                           Serialization
    // ####################################################################
    template <class Inspector>
    friend bool inspect(Inspector& inspector, Client& client) {
        return inspector.object(client).fields(
               // Info
               inspector.field("hostname",client.hostname_),
               inspector.field("client_actor",client.client_actor_),
               // State
               inspector.field("current_batch",client.current_batch_),
               // Stats
               inspector.field("batches_solved",client.batches_solved_));
        }

};