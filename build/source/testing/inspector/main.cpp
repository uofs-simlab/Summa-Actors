#include "caf/all.hpp"
#include "caf/io/all.hpp"

#include "main.hpp"

using namespace caf;




// class Batch {
//     private:
//         int batch_id;
//         int start_hru;

//     public:
Batch::Batch(int batch_id0, int start_hru0) { this->batch_id=batch_id0; this->start_hru=start_hru0; }//: batch_id(batch_id0), start_hru(start_hru0){}

int Batch::getBatchID() {
    return this->batch_id;
}

int Batch::getStartHRU() {
    return this->start_hru;
}
//             this->batch_id = batch_id0;
//             this->start_hru = start_hru0;
//         }
//         int getBatchID() {
//             return this->batch_id;
//         }

//         int getStartHRU() {
//             return this->start_hru;
//         }

//     template <class Inspector>
//     friend bool inspect(Inspector& inspector, Batch& batch) {
//         return inspector.object(batch).fields(inspector.field("a", batch.batch_id), inspector.field("b", batch.start_hru));
//     }

// };

// template<class Inspector>
// bool inspect(Inspector& inpsector, Batch& batch) {
//     auto getBatchID = [&batch] {return batch.getBatchID();};
//     auto getStartHRU
// }

behavior testee(event_based_actor* self) {
    return {
        [=](Batch& b) {
            aout(self) << b.getBatchID() << std::endl;
        }
    };
}


void caf_main(actor_system& sys) {
    scoped_actor self{sys};
    aout(self) << "Started Test Actor \n"; 
    anon_send(sys.spawn(testee), Batch{2, 4});
}

CAF_MAIN(id_block::custom_types_3)