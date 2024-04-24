#include "forcing_file_info.hpp"
#include <memory>

fileInfo::fileInfo() {
  filenmData = "";
  nVars = 0;
  nTimeSteps = 0;
  var_ix = std::vector<int>();
  data_id = std::vector<int>();
  varName = std::vector<std::string>();
  firstJulDay = 0.0;
  convTime2Days = 0.0;
}


forcingFileContainer::forcingFileContainer() {
  forcing_files_ = std::vector<fileInfo>();
}

forcingFileContainer::~forcingFileContainer() {
  freeForcingFiles_fortran();
}

int forcingFileContainer::initForcingFiles(int num_gru) {
  int num_files, err;
  
  // initalize the fortran side
  std::unique_ptr<char[]> message(new char[256]);
  ffile_info_fortran(num_gru, num_files, err, &message);
  if (err != 0) {
    std::cout << "Error initializing forcing files: " << message.get() << "\n";
    return -1;
  }

  forcing_files_.resize(num_files);

  for (int i = 1; i < num_files+1; i++) {
    int var_ix_size = 0;
    int data_id_size = 0;
    int varName_size = 0;
    getFileInfoSizes_fortran(i, var_ix_size, data_id_size, varName_size);
    forcing_files_[i-1].var_ix.resize(var_ix_size);
    forcing_files_[i-1].data_id.resize(data_id_size);
    forcing_files_[i-1].varName.resize(varName_size);

    // Allocate space for the file name and variable names
    std::unique_ptr<char[]> file_name(new char[256]);
    std::vector<std::unique_ptr<char[]>> var_name_arr;
    for (int j = 0; j < varName_size; j++) {
      var_name_arr.push_back(std::unique_ptr<char[]>(new char[256]));
    }

    getFileInfoCopy_fortran(i, &file_name, forcing_files_[i-1].nVars,
        forcing_files_[i-1].nTimeSteps, varName_size, var_ix_size, 
        data_id_size, var_name_arr.data(), forcing_files_[i-1].var_ix.data(), 
        forcing_files_[i-1].data_id.data(), forcing_files_[i-1].firstJulDay,
        forcing_files_[i-1].convTime2Days);

    forcing_files_[i-1].filenmData = std::string(file_name.get());
    forcing_files_[i-1].nVars = varName_size;
    for (int j = 0; j < varName_size; j++) {
      forcing_files_[i-1].varName[j] = std::string(var_name_arr[j].get());
    }
  }

  return 0;
}


int forcingFileContainer::loadForcingFile(int file_ID, int start_gru, 
    int num_gru) {
  int err = 0;
  if (forcing_files_[file_ID-1].is_loaded) {
    return 0;
  }

  if (file_ID < 1 || file_ID > forcing_files_.size()) {
    std::cout << "Error: Invalid file ID: " << file_ID << std::endl;
    return -1;
  }

  std::unique_ptr<char[]> message(new char[256]);
  read_forcingFile(file_ID, start_gru, num_gru, err, &message);
  if (err != 0) {
    std::cout << "Error reading forcing file: " << message.get() << std::endl;
    return -2;
  }
  forcing_files_[file_ID-1].is_loaded = true;
  files_loaded_++;
  return 0;
}

bool forcingFileContainer::isFileLoaded(int file_ID) {
  return forcing_files_[file_ID-1].is_loaded;
}




Forcing_File_Info::Forcing_File_Info(int file_ID) {
  this->file_ID = file_ID;
  this->num_steps = 0;
  this->is_loaded = false;
}

int Forcing_File_Info::getNumSteps() {
  return this->num_steps;
}

bool Forcing_File_Info::isFileLoaded() {
  return this->is_loaded;
}

void Forcing_File_Info::updateIsLoaded() {
  this->is_loaded = true;
}

void Forcing_File_Info::updateNumSteps(int num_steps) {
  this->num_steps = num_steps;
  this->is_loaded = true;
}


