#include <string>

extern "C" {
  void setTimesDirsAndFiles_fortran(char const* file_manager_path,  
        int* err, void* err_msg);
}

class FileManager {

  public:
    std::string file_manager_path_; // path to the file manager file
    // defines the time of the run
    std::string control_vrs_;       // control version
    std::string sim_start_tm_;      // simulation start time
    std::string sim_end_tm_;        // simulation end time
    std::string nc_time_zone_;      // time zone info
    // defines the path for data files (and default values)
    std::string settings_path_;     // settings dir path
    std::string state_path_;        // state file / init. cond. dir path (if omitted, defaults to SETTINGS_PATH for input, OUTPATH for output)
    std::string forcing_path_;      // input_dir_path
    std::string output_path_;       // output_dir_path
    // define name of control files (and default values)
    std::string m_decisions_;       // definition of model decisions
    std::string output_control_;    // metadata for model variables
    std::string local_attributes_;  // local attributes
    std::string localparam_info_;   // default values and constraints for local model parameters
    std::string basinparam_info_;   // default values and constraints for basin model parameters
    std::string vegparm_;           // noah vegetation parameter table
    std::string soilparm_;          // noah soil parameter table
    std::string genparm_;           // noah general parameter table
    std::string mptable_;           // noah mp parameter table
    std::string forcing_filelist_;  // list of focing files for each HRU
    std::string model_initcond_;    // model initial conditions
    std::string parameter_trial_;   // trial values for model parameters
    std::string output_prefix_;     // prefix for the output file
  
    // Constructor - read file_manager_path & populate class variables
    FileManager(const std::string& file_manager_path);
    ~FileManager() {};

    // Set the variables that are used by the summaFileManager (summaFileManager.f90)
    std::string setTimesDirsAndFiles();
    std::string toString();
};