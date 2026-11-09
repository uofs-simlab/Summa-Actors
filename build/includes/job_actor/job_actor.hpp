#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"

#include "num_gru_info.hpp"
#include "gru_struc.hpp"
#include "timing_info.hpp"
#include "settings_functions.hpp"  // defines JobActorSettings, HRUActorSettings, FileAccessActorSettings, ToleranceSettings
#include "json.hpp"
#include "gru_batch_actor.hpp"
#include "message_atoms.hpp"
#include "file_access_actor.hpp"
#include "summa_init_struc.hpp"
#include "gru_actor.hpp"
#include "logger.hpp"

#include <cmath>
#include <tuple>
#include <vector>
#include <string>
#include <unordered_map>

// For HOST_NAME_MAX
#include <limits.h>
#include <unistd.h>
#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX 255
#endif

class JobActor {
  // CAF self handle
  caf::event_based_actor* self_;

  // Host id for logs
  char hostname_[HOST_NAME_MAX];

  // Timing + logging
  TimingInfo timing_info_;
  bool enable_logging_ = false;
  std::unique_ptr<Logger>       logger_;
  std::unique_ptr<ErrorLogger>  err_logger_;
  std::unique_ptr<SuccessLogger> success_logger_;

  // === Actor references ===
  caf::actor file_access_actor_;
  caf::actor parent_;

  // === Simulation containers ===
  Batch batch_;
  std::unique_ptr<GruStruc>       gru_struc_;
  std::unique_ptr<SummaInitStruc> summa_init_struc_;
  NumGRUInfo num_gru_info_;

  // === Settings ===
  JobActorSettings        job_actor_settings_;
  FileAccessActorSettings fa_actor_settings_;
  HRUActorSettings        hru_actor_settings_;
  ToleranceSettings       tolerance_settings_;   // <-- consolidated tolerances (new)

  // Error-classification map used by restart policy (job_index -> error label)
  std::unordered_map<int, std::string> last_error_type_;  // <-- needed by cpp

  // Lower bounds for clamp during tolerance tightening
  const double MIN_REL_TOL = 1e-6;
  const double MIN_ABS_TOL = 1e-6;

  // IDA dt init factor (kept from prior design)
  int  dt_init_factor_ = 1;

  // Whether to use default tolerances from SUMMA init
  bool default_tol_ = true;

  // === Run state ===
  int  num_steps_ = 0;
  int  iFile_ = 1;
  int  steps_in_ffile_ = 0;
  int  forcing_step_ = 1;
  int  timestep_ = 1;
  int  num_gru_done_timestep_ = 0;
  int  output_step_ = 1;  // index in output ring buffer
  int  num_write_msgs_ = 0;
  bool da_paused_ = false;

  // Restart netCDF path hint (propagated to GRU actors)
  std::string restart_;

public:
  JobActor(caf::event_based_actor* self,
           Batch batch,
           bool enable_logging,
           JobActorSettings job_settings,
           FileAccessActorSettings fa_settings,
           HRUActorSettings hru_settings,
           caf::actor parent,
           std::string restart)
  : self_(self),
    batch_(batch),
    enable_logging_(enable_logging),
    job_actor_settings_(job_settings),
    fa_actor_settings_(fa_settings),
    hru_actor_settings_(hru_settings),
    parent_(parent),
    restart_(std::move(restart)),
    default_tol_(default_tol_) {}

  // Behaviors
  caf::behavior make_behavior();        // initial (spawns, init, routes to mode)
  caf::behavior data_assimilation_mode();
  caf::behavior async_mode();

  // Lifecycle helpers
  void spawnGruActors();
  void spawnGruBatches();
  void processTimestep();
  void handleFinishedGRU(int job_index);
  void finalizeJob();

  // Error handling
  void handleGRUError(int err_code, int job_index, int timestep, std::string& err_msg);
  void handleFileAccessError(int err_code, std::string& err_msg);
};

/*********************************************
 * Job Actor Data Structures
 *********************************************/
struct GRU_Container {
  std::vector<GRU*> gru_list;
  std::chrono::time_point<std::chrono::system_clock> gru_start_time;
  int num_gru_done = 0;
  int num_gru_failed = 0;        // waiting to be restarted
  int num_gru_in_run_domain = 0; // currently solving
  int run_attempts_left = 1;     // attempts remaining for this batch
};

/*********************************************
 * Job Actor state variables
 *********************************************/
struct job_state {
  TimingInfo job_timing;
  std::unique_ptr<Logger>       logger;
  std::unique_ptr<ErrorLogger>  err_logger;
  std::unique_ptr<SuccessLogger> success_logger;

  // Actor refs
  caf::actor file_access_actor;
  caf::actor parent;

  // Scope info
  Batch        batch;
  std::unique_ptr<GruStruc> gru_struc;
  NumGRUInfo   num_gru_info;
  GRU_Container gru_container;

  std::unique_ptr<SummaInitStruc> summa_init_struc;

  // Monitoring
  int dt_init_start_factor = 1;
  int num_gru_done = 0;
  int num_gru_failed = 0;

  std::string hostname;

  // Settings
  FileAccessActorSettings file_access_actor_settings;
  JobActorSettings        job_actor_settings;
  HRUActorSettings        hru_actor_settings;

  // Forcing
  int iFile = 1;
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

  DistributedSettings      distributed_settings;
  JobActorSettings         job_actor_settings;
  HRUActorSettings         hru_actor_settings;
  FileAccessActorSettings  file_access_actor_settings;

  std::vector<caf::actor> connected_nodes;

  std::vector<std::vector<double>> gru_times_per_node;
  std::vector<double> node_walltimes;

  std::chrono::time_point<std::chrono::system_clock> load_balance_start_time;
  std::chrono::time_point<std::chrono::system_clock> load_balance_end_time;
  double load_balance_time = 0.0;

  // <hru_actor, node_actor>
  std::unordered_map<caf::actor, caf::actor>                    hru_to_node_map;
  std::unordered_map<caf::actor, double>                        hru_walltimes;
  std::unordered_map<caf::actor, double>                        node_walltimes_map;
  std::unordered_map<caf::actor, std::unordered_map<caf::actor, double>> node_to_hru_map;

  std::vector<std::pair<caf::actor, HRU>> hrus_to_balance;

  std::unordered_map<caf::actor, std::vector<std::pair<caf::actor, HRU>>> node_to_hru_to_balance_map;
  std::unordered_map<caf::actor, int>                                      node_to_hru_to_balance_map_size;

  int num_hrus_to_swap = 0; // target ~25%

  // Forcing
  int iFile = 1;
  int stepsInCurrentFFile;
  int forcingStep = 1;
  int timestep = 1;
  int num_gru_done_timestep = 0;
  int num_steps = 0;

  // Misc counters
  int messages_returned = 0;
  int hru_batch_maps_received = 0;

  int num_times_load_balanced = 0;
  int num_serialize_messages_sent = 0;
  int num_serialize_messages_received = 0;
};

