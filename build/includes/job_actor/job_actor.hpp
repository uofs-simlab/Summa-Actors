#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "num_gru_info.hpp"
#include "gru_struc.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include "json.hpp"
// #include "hru_actor.hpp"
// #include "hru_batch_actor.hpp"
#include "message_atoms.hpp"
#include "file_access_actor.hpp"
#include <unistd.h>
#include <limits.h>
#include <cmath>
#include <vector>
#include <tuple>
#include "summa_init_struc.hpp"
#include "gru_actor.hpp"
#include "logger.hpp"


class JobActor {
  caf::event_based_actor* self_;

  char hostname_[HOST_NAME_MAX];

  TimingInfo timing_info_;
  std::unique_ptr<Logger> logger_;
  std::unique_ptr<ErrorLogger> err_logger_;
  std::unique_ptr<SuccessLogger> success_logger_;

  // Actor References
  caf::actor file_access_actor_;
  caf::actor parent_;

  Batch batch_;
  std::unique_ptr<GruStruc> gru_struc_;
  std::unique_ptr<SummaInitStruc> summa_init_struc_;
  NumGRUInfo num_gru_info_;

  // Settings
  JobActorSettings job_actor_settings_;
  FileAccessActorSettings fa_actor_settings_;
  HRUActorSettings hru_actor_settings_; 

  // Misc
  int num_steps_ = 0;
  int iFile_ = 1;
  int steps_in_ffile_ = 0;
  int forcing_step_ = 1;
  int timestep_ = 1;
  int num_gru_done_timestep_ = 0;
  
  public:
    JobActor(caf::event_based_actor* self, Batch batch, 
             JobActorSettings job_settings, FileAccessActorSettings fa_settings,
             HRUActorSettings hru_settings, caf::actor parent) 
             : self_(self), batch_(batch), job_actor_settings_(job_settings),
               fa_actor_settings_(fa_settings), 
               hru_actor_settings_(hru_settings), parent_(parent) {};
    
    caf::behavior make_behavior(); // Initial Behavior
    caf::behavior data_assimilation_mode();
    caf::behavior async_mode();

    void spawnGRUActors();
    void handleFinishedGRU(int job_index);
    void finalizeJob();
    // Error Handling Functions
    void handleGRUError(int err_code, int job_index, int timestep, 
                        std::string& err_msg);
    void handleFileAccessError(int err_code, std::string& err_msg);
};


/*********************************************
 * Job Actor Data Structures
 *********************************************/
// Holds information about the GRUs
struct GRU_Container {
  std::vector<GRU*> gru_list;
  std::chrono::time_point<std::chrono::system_clock> gru_start_time; // Vector of start times for each GRU
  int num_gru_done = 0; 
  int num_gru_failed = 0; // number of grus that are waiting to be restarted
  int num_gru_in_run_domain = 0; // number of grus we are currently solving for
  int run_attempts_left = 1; // current run attempt for all grus
};


/*********************************************
 * Job Actor state variables
 *********************************************/
struct job_state {
  TimingInfo job_timing;
  std::unique_ptr<Logger> logger;
  std::unique_ptr<ErrorLogger> err_logger;
  std::unique_ptr<SuccessLogger> success_logger;
  // Actor References
  caf::actor file_access_actor; // actor reference for the file_access_actor
  caf::actor parent;            // actor reference to the top-level SummaActor

  Batch batch; // Information about the number of HRUs and starting point 

  // TODO: gru_struc can contain the num_gru_info and be the gru_container
  std::unique_ptr<GruStruc> gru_struc; 
  NumGRUInfo num_gru_info;
  GRU_Container gru_container;

  std::unique_ptr<SummaInitStruc> summa_init_struc;

  // Variables for GRU monitoring
  int dt_init_start_factor = 1; // Initial Factor for dt_init (coupled_em)
  int num_gru_done = 0;         // The number of GRUs that have completed
  int num_gru_failed = 0;       // Number of GRUs that have failed

  
  std::string hostname;

  
  FileAccessActorSettings file_access_actor_settings;
  JobActorSettings job_actor_settings; 
  HRUActorSettings hru_actor_settings;

  // Forcing information
  int iFile = 1; // index of current forcing file from forcing file list
  int stepsInCurrentFFile;
  int forcingStep = 1;
  int timestep = 1;
  int num_gru_done_timestep = 0;
  int num_steps = 0;
};


struct distributed_job_state {
  TimingInfo job_timing;

  int file_gru;
  int start_gru;
  int num_gru;

  Batch batch;
  
  NumGRUInfo num_gru_info;
  std::vector<NumGRUInfo> node_num_gru_info;
  
  DistributedSettings distributed_settings;
  JobActorSettings job_actor_settings; 
  HRUActorSettings hru_actor_settings;
  FileAccessActorSettings file_access_actor_settings;

  std::vector<caf::actor> connected_nodes;

  std::vector<std::vector<double>> gru_times_per_node;
  std::vector<double> node_walltimes;

  std::chrono::time_point<std::chrono::system_clock> load_balance_start_time;
  std::chrono::time_point<std::chrono::system_clock> load_balance_end_time;
  double load_balance_time = 0.0;

  
  // <hru_actor, node_actor>
  std::unordered_map<caf::actor, caf::actor> hru_to_node_map;
  std::unordered_map<caf::actor, double> hru_walltimes;
  std::unordered_map<caf::actor, double> node_walltimes_map;
  std::unordered_map<caf::actor, std::unordered_map<caf::actor, double>> 
      node_to_hru_map;

  std::vector<std::pair<caf::actor, hru>> hrus_to_balance;

  std::unordered_map<caf::actor, std::vector<std::pair<caf::actor, hru>>> 
      node_to_hru_to_balance_map;
  std::unordered_map<caf::actor, int> node_to_hru_to_balance_map_size; 

  int num_hrus_to_swap = 0; // We want to swap %25 of the HRUs

  // Forcing information
  int iFile = 1; // index of current forcing file from forcing file list
  int stepsInCurrentFFile;
  int forcingStep = 1;
  int timestep = 1;
  int num_gru_done_timestep = 0;
  int num_steps = 0;


  // Misc message counter
  int messages_returned = 0;
  int hru_batch_maps_received = 0;

  // Misc counter
  int num_times_load_balanced = 0;
  int num_serialize_messages_sent = 0;
  int num_serialize_messages_received = 0;
};
