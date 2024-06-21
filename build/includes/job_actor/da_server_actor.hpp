#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "settings_functions.hpp"
#include "file_manager.hpp"
#include "gru_struc.hpp"
#include "message_atoms.hpp"

class DAServerActor {
  private:
    // Part of the constructor
    caf::event_based_actor* self_;
    int start_gru_;
    int num_gru_;
    Settings settings_;

    // member variables
    std::unique_ptr<FileManager> file_manager_;
    int file_gru_; // total number of grus in attributes.nc
    std::unique_ptr<GruStruc> gru_struc_;
    std::vector<caf::actor> connected_clients_;
    int num_clients_ready_ = 0;

    int iFile = 1;
    int num_steps_ffile_ = 0;
    int num_timesteps_ = 0;
    int timestep_ = 1;
    int forcing_step_ = 1;

    bool done_ = false;
    int clients_exited_ = 0;

  public:
    DAServerActor(caf::event_based_actor* self, int start_gru, int num_gru_, 
                  Settings settings) 
                  : self_(self), start_gru_(start_gru), num_gru_(num_gru_), 
                    settings_(settings) {};
    caf::behavior make_behavior();
};