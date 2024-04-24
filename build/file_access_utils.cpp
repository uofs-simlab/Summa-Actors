
#include <forcing_file_info.hpp>
/**
 * Same file_info from data_types.f90 
 * This is a C++ Representation of the file_info data type
*/
// class forcFileInfo {
//   private:
//     std::string filenmData;     // name of data file
//     int nVars;                  // number of variables in file
//     int nTimeSteps;             // number of time steps in file
//     std::vector<int> var_ix;    // index of each forcing data variable in the data structure
//     std::vector<int> data_id;   // netcdf variable id for each forcing data variable
//     std::vector<int> varName;   // netcdf variable name for each forcing data variable
//     double firstJulDay;         // first julian day in forcing file
//     double convTime2Days;       // conversion factor to convert time units to days
// };

// fileInfo::fileInfo(void *handle_forcing_file_info) {
//   std::cout << "hello from forcFileInfo constructor\n";
// }