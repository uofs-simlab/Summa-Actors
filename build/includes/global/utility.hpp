#pragma once
#include "utility.hpp"
#include <sys/stat.h>  
#include <cerrno>      
#include <cstring>     
#include <iostream>    

bool createDirectory(const std::string& path);
