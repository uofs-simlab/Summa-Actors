#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"
#include "gru_struc.hpp"
#include "message_atoms.hpp"
#include "summa_init_struc.hpp"
#include "file_manager.hpp"
#include "summa_global_data.hpp"
#include "file_access_actor.hpp"
#include "gru_batch_actor.hpp"
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
    std::string host_;
    Settings settings_;

    // member variables
    caf::actor server_;
    char hostname_[HOST_NAME_MAX];
    NodeGruInfo node_gru_info_;
    std::unique_ptr<FileManager> file_manager_;
    std::unique_ptr<SummaGlobalData> global_fortran_state_;
    std::unique_ptr<GruStruc> gru_struc_;
    std::unique_ptr<SummaInitStruc> summa_init_struc_;

    caf::actor file_access_actor_;
    
    int num_steps_;
    int forcing_step_ = 1;
    int num_steps_ffile_ = 0;
    int timestep_ = 1;
    int num_gru_done_timestep_ = 0;

  public:
    DAClientActor(caf::event_based_actor* self, std::string host, 
                  Settings settings, caf::actor server = nullptr) 
                  : self_(self), host_(host), settings_(settings),
                    server_(server) {};
    caf::behavior make_behavior();

    // methods
    void set_node_gru_info(NodeGruInfo node_gru_info) {
      node_gru_info_ = node_gru_info;
    }

    void spawnGruBatches();
};