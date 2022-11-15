#pragma once
#include "caf/all.hpp"
#include "caf/io/all.hpp"
class Batch;
class Client;

using namespace caf;

enum batch_status {
    unassigned,
    assigned,
    solved,
    failed
};

CAF_BEGIN_TYPE_ID_BLOCK(custom_types_3, first_custom_type_id)

    CAF_ADD_ATOM(custom_types_3, hello)

    CAF_ADD_TYPE_ID(custom_types_3, (Batch))
    CAF_ADD_TYPE_ID(custom_types_3, (Client))


CAF_END_TYPE_ID_BLOCK(custom_types_3)



class Client {
    private:
        caf::actor client_actor;
        std::string hostname;

        int id;
        int batches_solved;


    public:
        Client(int id = -1, caf::actor client_actor = nullptr, std::string hostname = "");


    template <class Inspector>
    friend bool inspect(Inspector& inspector, Client& client) {
        return inspector.object(client).fields(
            inspector.field("client_actor",client.client_actor),
            inspector.field("hostname",client.hostname),
            inspector.field("id",client.id),
            inspector.field("batches_solved",client.batches_solved));
        }
};



class Batch {
    private:
        int batch_id;
        int start_hru;

        int test;
        caf::actor assigned_actor;
        batch_status status;

    public:
        Batch(int batch_id0 = 0, int start_hru0 = 0);
        int getBatchID();

        int getStartHRU();

    template <class Inspector>
    friend bool inspect(Inspector& inspector, Batch& batch) {
        return inspector.object(batch).fields(
                                        inspector.field("a", batch.batch_id), 
                                        inspector.field("b", batch.start_hru),
                                        inspector.field("c", batch.test),
                                        inspector.field("d", batch.assigned_actor));
    }

};
