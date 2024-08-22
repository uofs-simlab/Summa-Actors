#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "num_gru_info.hpp"
#include "GRU.hpp"
#include "gru_struc.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"
#include "json.hpp"
#include "hru_actor.hpp"
#include "hru_batch_actor.hpp"
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

  // settings for all child actors (save in case we need to recover)
  File_Access_Actor_Settings file_access_actor_settings;
  Job_Actor_Settings job_actor_settings; 
  HRU_Actor_Settings hru_actor_settings;

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
  
  Distributed_Settings distributed_settings;
  Job_Actor_Settings job_actor_settings; 
  HRU_Actor_Settings hru_actor_settings;
  File_Access_Actor_Settings file_access_actor_settings;

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

/** The Job Actor Behaviors */
caf::behavior job_actor(caf::stateful_actor<job_state>* self, Batch batch,
                        File_Access_Actor_Settings file_access_actor_settings, 
                        Job_Actor_Settings job_actor_settings, 
                        HRU_Actor_Settings hru_actor_settings, 
                        caf::actor parent, caf::actor openwq);

caf::behavior data_assimilation_mode(caf::stateful_actor<job_state>* self);
caf::behavior async_mode(caf::stateful_actor<job_state>* self); 

/** The Job Actor For Internode Communication */
caf::behavior distributed_job_actor(
    caf::stateful_actor<distributed_job_state>* self, int start_gru, 
    int num_gru, Distributed_Settings distributed_settings,
    File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, 
    HRU_Actor_Settings hru_actor_settings);


/*********************************************
 * Functions for the Job Actor (job_utils.cpp)
 *********************************************/
// Spawn HRU Actors Individually
void spawnHRUActors(caf::stateful_actor<job_state>* self);
// Spawn HRU Batch Actors
void spawnHRUBatches(caf::stateful_actor<job_state>* self);

void handleFinishedGRU(caf::stateful_actor<job_state>* self,
                       int local_gru_index);

void finalizeJob(caf::stateful_actor<job_state>* self);

void handleGRUError(caf::stateful_actor<job_state>* self, int err_code, 
                    int index, int timestep, std::string& err_msg);

void handleFileAccessError(caf::stateful_actor<job_state>* self, int err_code, 
                           std::string& err_msg);