#include "caf/all.hpp"
#include "caf/io/all.hpp"

#include "main.hpp"

using namespace caf;

Batch::Batch(int batch_id0, int start_hru0) { this->batch_id=batch_id0; this->start_hru=start_hru0; }//: batch_id(batch_id0), start_hru(start_hru0){}

int Batch::getBatchID() {
    return this->batch_id;
}

int Batch::getStartHRU() {
    return this->start_hru;
}

Client::Client(int id, caf::actor client_actor, std::string hostname) {
    this->id = id;
    this->client_actor = client_actor;
    this->hostname = hostname;
}

behavior testee(event_based_actor* self) {
    return {
        // [=](Batch& b) {
        //     aout(self) << b.getBatchID() << std::endl;
        // }
    };
}


void caf_main(actor_system& sys) {
    scoped_actor self{sys};
    aout(self) << "Started Test Actor \n"; 
    anon_send(sys.spawn(testee), Batch{2, 4});
}

CAF_MAIN(id_block::custom_types_3)