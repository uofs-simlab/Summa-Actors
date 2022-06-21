#ifndef SUMMASERVER_H_
#define SUMMASERVER_H_

#include "caf/all.hpp"
#include "caf/io/all.hpp"

using namespace caf;

struct summa_server_state {

};


behavior summa_server(stateful_actor<summa_server_state>* self) {
    aout(self) << "Summa Server has Started \n"; 

    return {
        [=](int i) {
            aout(self) << "Received " << i << "\n";
            return "Received";
        }

    };

}





#endif