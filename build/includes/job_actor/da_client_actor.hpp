#pragma
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"
#include "gru_struc.hpp"
#include "message_atoms.hpp"

// For HOST_NAME_MAX
#include <limits.h>
#include <unistd.h>
#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX 255
#endif

class DAClientActor {
  private:
    // Part of the constructor
    caf::event_based_actor* self_;
    Settings settings_;

    // member variables
    caf::actor server_;
    char hostname_[HOST_NAME_MAX];
    NodeGruInfo node_gru_info_;
  public:
    DAClientActor(caf::event_based_actor* self, Settings settings) 
                  : self_(self), settings_(settings) {};
    caf::behavior make_behavior();

    // methods
    void set_node_gru_info(NodeGruInfo node_gru_info) {
      node_gru_info_ = node_gru_info;
    }
};