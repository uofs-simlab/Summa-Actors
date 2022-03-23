#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "SummaActor.h"
#include <string>
#include "messageAtoms.h"
#include <bits/stdc++.h>
#include <unistd.h>
#include <iostream>

using namespace caf;

/* Configuration class that handles the config and 
/  command line options for the actors program */
class config : public actor_system_config {
    public:
        int startGRU = -1;
        int countGRU = -1;
        std::string fileManager = ""; // master file 
        std::string csvOut = "";
    
    config() {
        opt_group{custom_options_, "global"}
            .add(startGRU, "gru,g", "Starting GRU Index")
            .add(countGRU, "countGRU,c", "Total Number of GRUs")
            .add(fileManager, "master,m", "Path name of master file")
            .add(csvOut, "csv,v", "nameOfCSV");
    }
};

void caf_main(actor_system& sys, const config& cfg) {
    scoped_actor self{sys};
    if (cfg.startGRU == -1) {
        aout(self) << "Starting GRU was not defined!! " << 
            "startGRU is set with the \"-g\" option\n";
        aout(self) << "EXAMPLE: ./summaMain -g 1 -c 10 -m file/manager/location \n";
        return;
    }
    if (cfg.countGRU == -1) {
        aout(self) << "Number of GRUs was not defined!! " <<
            "countGRU is set with the \"-c\" option\n";
        aout(self) << "EXAMPLE: ./summaMain -g 1 -c 10 -m file/manager/location \n";
        return;
    }
    if (cfg.fileManager == "") {
        aout(self) << "File Manager was not defined!! " << 
            "fileManger is set with the \"-m\" option\n";
        aout(self) << "EXAMPLE: ./summaMain -g 1 -c 10 -m file/manager/location \n";
        return;
    }
    // start SUMMA
    auto summa = sys.spawn(summa_actor, cfg.startGRU, cfg.countGRU, cfg.fileManager, cfg.csvOut);
}

CAF_MAIN(id_block::summa)