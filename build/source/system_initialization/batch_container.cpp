#include "batch_container.hpp"

// ####################################################################
//                              Constructors
// ####################################################################
BatchContainer::BatchContainer(std::string name, std::string file_manager, 
    int start_gru, int num_gru, int num_gru_per_batch, Settings settings, bool use_state_file) : 
    name_(name), file_manager_(file_manager), start_gru_(start_gru),
    num_gru_(num_gru), num_gru_per_batch_(num_gru_per_batch),
    settings_(settings), use_state_file_(use_state_file) {
  
  std::string log_dir = "";
  if (!log_dir.empty() && log_dir.back() != '/') {
    log_dir += "/"; // Ensure log_dir_ is a directory
  }

  // Check if we can build from sate file
  if (use_state_file_) {
    struct stat info;
    if (stat(settings_.state_dir_.c_str(), &info) != 0) {
      // Directory Does not Exist
      assembleBatches(log_dir);
    } else {
      // Directory Exists
      int err = readStateFile();
      if (err == FAILURE) {
        std::cout << "ERROR--Batch_Container: Could not read state file\n"
                  << "Building Batches from Scratch\n";
        assembleBatches(log_dir);
      }
    }
  } else {
    assembleBatches(log_dir);
  }
     
  // Initialize logger
  if (!log_dir.empty()) {
    logger_ = Logger(log_dir + "batch_container");
    logger_.log("----------------Batch List----------------");
    logger_.log(this->getBatchesAsString());
    logger_.log("------------------------------------------");
  } else {
    logger_ = Logger("");
  }
}

// ####################################################################
//                              Private Methods
// ####################################################################
void BatchContainer::assembleBatches(std::string log_dir) {
  int remaining_gru_to_batch = num_gru_;
  int batch_id = 0;
  int start_gru_local = start_gru_;

  while (remaining_gru_to_batch > 0) {
    int current_batch_size = std::min(num_gru_per_batch_, 
                                      remaining_gru_to_batch);
    batch_list_.push_back(Batch(name_, batch_id, file_manager_, start_gru_local, 
        current_batch_size));
    batch_list_[batch_id].setLogDir(log_dir);

    remaining_gru_to_batch -= current_batch_size;
    start_gru_local += current_batch_size;
    if (current_batch_size == num_gru_per_batch_)
      batch_id += 1;
  }
  
  batches_remaining_ = batch_list_.size();
  if (use_state_file_) createStateFile(settings_.state_dir_);
}

// ####################################################################
//                              Getters
// ####################################################################
std::string BatchContainer::getBatchesAsString() {
  std::string out_string = "";
  for (auto& batch : batch_list_) {
    out_string += batch.toString();
  }
  return out_string;
}

std::optional<Batch> BatchContainer::getUnsolvedBatch() {
  for (auto& batch : batch_list_) {
    if (!batch.isAssigned() && !batch.isSolved()) {
      batch.updateAssigned(true);
      logger_.log("Starting Batch " + std::to_string(batch.getBatchID()));
      return batch;
    }
  }
  logger_.log("ERROR--Batch_Container: No Unsolved Batches");
  return {};
}

std::string BatchContainer::getAllBatchInfoString() {
  std::string out_string = "";
  for (auto& batch : batch_list_) {
    out_string += "_____________________________\n";
    out_string += batch.toString();
    out_string += "_____________________________\n";
  }
  return out_string;
}

double BatchContainer::getTotalReadTime() {
  double total_read_time = 0.0;
  for (auto& batch : batch_list_) {
    total_read_time += batch.getReadTime();
  }
  return total_read_time;
}

double BatchContainer::getTotalWriteTime() {
  double total_write_time = 0.0;
  for (auto& batch : batch_list_) {
    total_write_time += batch.getWriteTime();
  }
  return total_write_time;
}

// ####################################################################
//                              Setters
// ####################################################################
void BatchContainer::setBatchAssigned(Batch batch) {
  batch_list_[batch.getBatchID()].updateAssigned(true);
}

void BatchContainer::setBatchUnassigned(Batch batch) {
  batch_list_[batch.getBatchID()].updateAssigned(false);
}

// ####################################################################
//                          Public Methods
// ####################################################################
std::string BatchContainer::toString() {
  std::string out_string = "BatchContainer: " + name_ + "\n";
    out_string += "File Manager: " + file_manager_ + "\n";
    out_string += "Start GRU: " + std::to_string(start_gru_) + "\n";
    out_string += "Num GRU: " + std::to_string(num_gru_) + "\n";
    out_string += "Num GRU Per Batch: "+std::to_string(num_gru_per_batch_)+"\n";
    out_string += "Batches Remaining: "+std::to_string(batches_remaining_)+"\n";
  return out_string;
}

void BatchContainer::updateBatch(Batch batch) {
  batch_list_[batch.getBatchID()] = batch;
  batch_list_[batch.getBatchID()].updateSolved(true);
  batches_remaining_--;
}

bool BatchContainer::hasUnsolvedBatches() { return batches_remaining_ > 0;}


// ####################################################################
//                        State File Methods
// ####################################################################
void BatchContainer::createStateFile(const std::string &state_dir) {
  state_file_ = state_dir + name_ + "_state.csv";
  
  std::ofstream file;
  file.open(state_file_, std::ios::out);
  file << "batch_id,start_gru,num_gru,assigned,solved\n";
  
  for (auto& batch : batch_list_) {
    file << batch.getBatchID() << "," << batch.getStartGru() << "," 
         << batch.getNumGru() << "," << batch.isAssigned() << "," 
         << batch.isSolved() << "\n";
  }
  file.close();  
}

int BatchContainer::readStateFile() {
  state_file_ = settings_.state_dir_ + name_ + "_state.csv";

  // Check if the file exists
  struct stat info;
  if (stat(state_file_.c_str(), &info) != 0) {
    // File Does Not Exist
    std::cout << "State File for " << name_ << " does not exist\n";
    return FAILURE;
  }
  std::cout << "State File for " << name_ << " exists\n";

  int expected_num_batches = 0;
  int remaining_gru_to_batch = num_gru_;
  while (remaining_gru_to_batch > 0) {
    int current_batch_size = std::min(num_gru_per_batch_, 
                                      remaining_gru_to_batch);
    expected_num_batches += 1;
    remaining_gru_to_batch -= current_batch_size;
  }
  std::cout << "\n\n\t Expected Batches: " << expected_num_batches << "\n\n";

  std::ifstream file_in(state_file_);
  std::vector<std::string> lines;
  std::string line;

  // Read all lines from the file
  while (std::getline(file_in, line)) {
    lines.push_back(line);
  }
  file_in.close();

  // Check if the number of lines in the file matches the expected number of batches
  if (lines.size() - 1 != expected_num_batches) {
    std::cout << "ERROR--Batch_Container: Number of Batches in State File does not match\n";
    return FAILURE;
  }

  std::cout << lines[0] << "\n";
  // remove the header
  lines.erase(lines.begin());
  for (auto &line : lines) {
    std::cout << line << "\n";
    // Parse the line (comma separated values)
    std::istringstream ss(line);

    int id, start_gru, num_gru;
    bool assigned, solved;
    char comma;
    ss >> id >> comma >> start_gru >> comma >> num_gru >> comma >> assigned 
       >> comma >> solved;
    std::cout << "BATCH\n"
              << "\tID: " << id << "\n"
              << "\tStart GRU: " << start_gru << "\n"
              << "\tNum GRU: " << num_gru << "\n"
              << "\tAssigned: " << assigned << "\n"
              << "\tSolved: " << solved << "\n";

    // Create the batch
    batch_list_.push_back(Batch(name_, id, file_manager_, start_gru, num_gru));
    if (assigned) {
      batch_list_.back().updateAssigned(true);
    }

    if (solved) {
      batch_list_.back().updateSolved(true);
    } else {
      batches_remaining_++;
    }
  } 

  std::cout << "Number of Batches in State File matches\n";

  return SUCCESS;
}

void BatchContainer::assignBatchSF(const Batch &batch) {
  std::ifstream file_in(state_file_);
  std::vector<std::string> lines;
  std::string line;

  // Read all lines from the file
  while (std::getline(file_in, line)) {
    lines.push_back(line);
  }
  file_in.close();

  // Find the line with the matching batch_id and update the assigned column
  for (size_t i = 1; i < lines.size(); ++i) { // Start from 1 to skip the header
    std::istringstream ss(lines[i]);
    std::string id, start_gru, num_gru;
    std::getline(ss, id, ',');
    std::getline(ss, start_gru, ',');
    std::getline(ss, num_gru, ',');
    bool assigned = true;
    bool solved = false;

    if (std::stoi(id) == batch.getBatchID()) {
      std::ostringstream new_line;
      new_line << id << "," << start_gru << "," << num_gru << "," 
               << assigned << "," << solved;
      lines[i] = new_line.str();
      break;
    }
  }

  // Write all lines back to the file
  std::ofstream file_out(state_file_);
  for (const auto& l : lines) {
    file_out << l << "\n";
  }
  file_out.close();
}

void BatchContainer::unassignBatchSF(const Batch &batch) {
  std::ifstream file_in(state_file_);
  std::vector<std::string> lines;
  std::string line;

  // Read all lines from the file
  while (std::getline(file_in, line)) {
    lines.push_back(line);
  }
  file_in.close();

  // Find the line with the matching batch_id and update the assigned column
  for (size_t i = 1; i < lines.size(); ++i) { // Start from 1 to skip the header
    std::istringstream ss(lines[i]);
    std::string id, start_gru, num_gru;
    std::getline(ss, id, ',');
    std::getline(ss, start_gru, ',');
    std::getline(ss, num_gru, ',');

    bool assigned = false;
    bool solved = false;

    if (std::stoi(id) == batch.getBatchID()) {
      std::ostringstream new_line;
      new_line << id << "," << start_gru << "," << num_gru << "," 
               << assigned << "," << solved;
      lines[i] = new_line.str();
      break;
    }
  }

  // Write all lines back to the file
  std::ofstream file_out(state_file_);
  for (const auto& l : lines) {
    file_out << l << "\n";
  }
  file_out.close();
}

void BatchContainer::solvedBatchSF(const Batch &batch) {
  std::ifstream file_in(state_file_);
  std::vector<std::string> lines;
  std::string line;

  // Read all lines from the file
  while (std::getline(file_in, line)) {
    lines.push_back(line);
  }
  file_in.close();

  // Find the line with the matching batch_id and update the assigned column
  for (size_t i = 1; i < lines.size(); ++i) { // Start from 1 to skip the header
    std::istringstream ss(lines[i]);
    std::string id, start_gru, num_gru;
    std::getline(ss, id, ',');
    std::getline(ss, start_gru, ',');
    std::getline(ss, num_gru, ',');

    bool assigned = false;
    bool solved = true;

    if (std::stoi(id) == batch.getBatchID()) {
      std::ostringstream new_line;
      new_line << id << "," << start_gru << "," << num_gru << "," 
               << assigned << "," << solved;
      lines[i] = new_line.str();
      break;
    }
  }

  // Write all lines back to the file
  std::ofstream file_out(state_file_);
  for (const auto& l : lines) {
    file_out << l << "\n";
  }
  file_out.close();
}





