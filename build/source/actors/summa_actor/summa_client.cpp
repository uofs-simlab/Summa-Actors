#include "caf/all.hpp"
#include "caf/io/all.hpp"

#include "summa_client.hpp"
#include "summa_actor.hpp"
#include "message_atoms.hpp"
#include "batch_manager.hpp"

#include <unistd.h>
#include <limits.h>


namespace caf {

behavior summa_client(stateful_actor<summa_client_state>* self) {
    self->set_down_handler([=](const down_msg& dm){
        if(dm.source == self->state.current_server) {
            aout(self) << "*** Lost Connection to Server" << std::endl;
            self->state.current_server = nullptr;
            self->become(unconnected(self));
        }
    });
    return unconnected(self);
}
/**
 * Attempt to connect to the server 
 */
behavior unconnected(stateful_actor<summa_client_state>* self) {
    return {
        [=] (connect_atom, const std::string& host, uint16_t port) {
            connecting(self, host, port);
        },
    };
}

void connecting(stateful_actor<summa_client_state>* self, const std::string& host, uint16_t port) {
    self->state.current_server = nullptr;

    auto mm = self->system().middleman().actor_handle();
    self->request(mm, infinite, connect_atom_v, host, port)
        .await(
            [=](const node_id&, strong_actor_ptr serv,
                const std::set<std::string>& ifs) {
                if (!serv) {
                    aout(self) << R"(*** no server found at ")" << host << R"(":)" << port
                     << std::endl;
                    return;
                }
                if (!ifs.empty()) {
                    aout(self) << R"(*** typed actor found at ")" << host << R"(":)"
                        << port << ", but expected an untyped actor " << std::endl;
                    return;
                }
                aout(self) << "*** successfully connected to server" << std::endl;
                self->state.current_server = serv;
                auto hdl = actor_cast<actor>(serv);
                self->monitor(hdl);
                self->become(running(self, hdl));
                },
            [=](const error& err) {
                aout(self) << R"(*** cannot connect to ")" << host << R"(":)" << port
                   << " => " << to_string(err) << std::endl;
                self->become(unconnected(self));
        });
}

behavior running(stateful_actor<summa_client_state>* self, const actor& server_actor) {
    char host[HOST_NAME_MAX];
    aout(self) << "Client Has Started Successfully" << std::endl;
    gethostname(host, HOST_NAME_MAX);
    self->state.hostname = host;

    self->send(server_actor, connect_to_server_v, self, self->state.hostname);
    return {
        [=](batch, int client_id, int batch_id, int start_hru, int num_hru, std::string config_path) {
            aout(self) << "Received batch to compute" << std::endl;
            self->state.client_id = client_id;
            self->state.batch_id = batch_id;
            self->state.summa_actor_ref = self->spawn(summa_actor, start_hru, num_hru, config_path, self);
        },

        [=](done_batch, double total_duration, double total_read_duration, double total_write_duration) {
            aout(self) << "summa_actor has finished, sending message to the server for another batch\n";
            self->send(server_actor, done_batch_v, self, self->state.client_id, self->state.batch_id, 
                total_duration, total_read_duration, total_write_duration);
        },

        [=](time_to_exit) {
            aout(self) << "Client Exiting\n";
            self->quit();
        }
        
    };
}
}