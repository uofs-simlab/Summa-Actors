#ifndef TESTCOORDINATOR_H_
#define TESTCOORDINATOR_H_
#include <string>
#include "caf/all.hpp"
#include "../actors/messageAtoms.h"
struct test_state {
    int countGRU = 10;
    int currGRU = 1;
    int dt_init_factor = 1; // Inital Value
    int counter;
    std::string fileManager = "/project/gwf/gwf_cmt/kck540/domain_NorthAmerica/settings/SUMMA/fileManager.txt";
    caf::actor hru;
};

#endif