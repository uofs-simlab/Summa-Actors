#include "client.hpp"

Client::Client(caf::actor client_actor, std::string hostname) {
  client_actor_ = client_actor;
  hostname_ = hostname;
  current_batch_ = {};
}

// ####################################################################
//                              Methods
// ####################################################################
std::string Client::toString() {
  std::string out_string = "Actor Addr: " + to_string(client_actor_.address()) +
                           "\n\tHostname: " + hostname_;
  if (current_batch_.has_value()) {
    out_string += "\n\tCurrent Batch: " + current_batch_.value().toString();
  } else {
    out_string += "\n\tCurrent Batch: None";
  }
  return out_string;
}