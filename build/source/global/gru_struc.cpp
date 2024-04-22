#include "gru_struc.hpp"
#include <iostream>
#include <netcdf.h>
#include <vector>
#include <future>

gruStruc::gruStruc(const std::string &settingsPath, 
    const std::string &attributeFile, int num_gru, int start_gru) {
  std::cout << "Creating GRU Structure\n";
  
  size_t fileGRU, fileHRU;
  int ncid, gru_dim_id, hru_dim_id, var_id;


  std::string attributes_path = settingsPath + attributeFile;
  // Read and set dimensions of whole file
  nc_open(attributes_path.c_str(), NC_NOWRITE, &ncid);
  nc_inq_dimid(ncid, "gru", &gru_dim_id);
  nc_inq_dimid(ncid, "hru", &hru_dim_id);
  nc_inq_dimlen(ncid, gru_dim_id, &fileGRU);
  nc_inq_dimlen(ncid, hru_dim_id, &fileHRU);

  std::vector<long int> gru_id(fileGRU);
  std::vector<int> hru_ix(fileHRU);
  std::vector<long int> hru_id(fileHRU);
  std::vector<long int> hru2gru_id(fileHRU);

  size_t start[1] = {0};
  size_t count[1] = {fileGRU};
  nc_inq_varid(ncid, "gruId", &var_id);
  nc_get_vara_long(ncid, var_id, start, count, gru_id.data());

  count[0] = fileHRU;
  nc_inq_varid(ncid, "hruId", &var_id);
  nc_get_vara_long(ncid, var_id, start, count, hru_id.data());

  nc_inq_varid(ncid, "hru2gruId", &var_id);
  nc_get_vara_long(ncid, var_id, start, count, hru2gru_id.data());

  std::cout << "DONE READING\n";
  nc_close(ncid);
  
  int file_hru = fileHRU;
  int file_gru = fileGRU;
  init_gru_struc(&num_gru, &file_hru, hru_ix.data());

  std::vector<std::future<void>> futures;
  
  for (int i = 1; i < num_gru; i++) {
    futures.push_back(std::async(std::launch::async, pop_gru_struc, &i, 
        gru_id.data(), hru_id.data(), hru2gru_id.data(), hru_ix.data(), 
        &file_gru, &file_hru, &num_gru, &start_gru));
    // pop_gru_struc(&i, gru_id.data(), hru_id.data(), hru2gru_id.data(), 
    //     hru_ix.data(), &file_gru, &file_hru, &num_gru, &start_gru);
  }

}