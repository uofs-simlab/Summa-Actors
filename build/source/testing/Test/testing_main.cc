#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "/home/k13nk/SummaProject/Summa-Actors/build/source/testing/Test/JobTest.h"
#include <string>
#include "messageAtoms.h"
#include <bits/stdc++.h>
#include <unistd.h>
#include <iostream>

using namespace caf;


void caf_main(actor_system& sys) {
    scoped_actor self{sys};

    // start SUMMA
   auto job = self->spawn(job_actor);
}

CAF_MAIN(id_block::summaTest)