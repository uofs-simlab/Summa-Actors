#include "utility.hpp"


bool createDirectory(const std::string& path) {
  struct stat info;

  if (stat(path.c_str(), &info) != 0) {
    // Directory does not exist
    if (errno == ENOENT) {
      if (mkdir(path.c_str(), 0755) != 0) {
        std::cerr << "Error creating directory: " << strerror(errno) << "\n";
        return false;
      }
    } else {
      std::cerr << "Error checking directory: " << strerror(errno) << "\n";
      return false;
    }
  } else if (!(info.st_mode & S_IFDIR)) {
    std::cerr << "Path exists but is not a directory\n";
    return false;
  }
  return true;
}