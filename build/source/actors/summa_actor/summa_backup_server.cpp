#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "message_atoms.hpp"
#include "summa_backup_server.hpp"
#include "global.hpp"
#include <chrono>
#include <thread>
namespace caf {

behavior summa_backup_server(stateful_actor<summa_backup_state>* self) {

    self->set_down_handler([=](const down_msg& dm){
        if(dm.source == self->state.current_server) {
            aout(self) << "*** Lost Connection to Server" << std::endl;
            uint16_t port = 4444;
            std::string host = "a0449745d77d";
            connecting(self, host, port);

            // auto new_server = self->spawn(backup_server);

            // int port = 4444;
            // aout(self) << "Attempting to publish backup server on port - " << port << std::endl;
            // auto is_port = io::publish(new_server, port);
            // if (!is_port) {
            //     std::cerr << "********PUBLISH FAILED*******" << to_string(is_port.error()) << "\n";
            //     return;
            // }
            // aout(self) << "Successfully Published summa_server_actor on port " << *is_port << "\n";

            // self->state.current_server = nullptr;
            // self->become(unconnected(self));
        }
    });
    return unconnected(self);
}

/**
 * Attempt to connect to the server 
 */
behavior unconnected(stateful_actor<summa_backup_state>* self) {
    return {
        [=] (connect_atom, const std::string& host, uint16_t port) {
            connecting(self, host, port);
        },
    };
}

void connecting(stateful_actor<summa_backup_state>* self, const std::string& host, uint16_t port) {
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

behavior running(stateful_actor<summa_backup_state>* self, const actor& server_actor) {
    aout(self) << "Backup Server is now Running\n";
    self->send(server_actor, connect_as_backup_v, self);
    return {
        [=](connect_as_backup) {
            aout(self) << "Successfully connected to current server\n";
        },

        [=](connect_as_backup, actor other_actor, int num_to_send) {
            aout(self) << "Received other actor Can we send them a message?\n";
            self->state.backup = other_actor;
            self->send(self->state.backup, connect_as_backup_v, num_to_send);

        },

        [=](connect_as_backup, int num_to_send) {
            aout(self) << "Received message from the other actor\n";
            aout(self) << "We Got the number " << num_to_send << std::endl;
            std::this_thread::sleep_for(std::chrono::seconds(2));
            num_to_send++;
            self->send(self->state.backup, connect_as_backup_v, num_to_send);
            
        },


    };
}

// behavior backup_server(stateful_actor<summa_backup_state>* self) {
//     aout(self) << "Published\n";
//     return {
//         [=] (connect_as_backup) {

//         }




//     };
// }




}