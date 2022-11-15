#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "message_atoms.hpp"
#include "summa_backup_server.hpp"
#include "global.hpp"
#include <chrono>
#include <thread>
namespace caf {

behavior summa_backup_server_init(stateful_actor<summa_server_state>* self, Distributed_Settings distributed_settings, 
    Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings) {
    aout(self) << "Backup Server Started\n";

    self->state.distributed_settings = distributed_settings;
    self->state.summa_actor_settings = summa_actor_settings; 
    self->state.file_access_actor_settings = file_access_actor_settings;
    self->state.job_actor_settings = job_actor_settings;
    self->state.hru_actor_settings = hru_actor_settings;

    self->set_down_handler([=](const down_msg& dm){
        if(dm.source == self->state.current_server) {
            // aout(self) << "*** Lost Connection to Server" << std::endl;
            // uint16_t port = 4444;
            // std::string host = "a0449745d77d";
            // connecting(self, host, port);
        }
    });
    return {
        // Called by main to init the process
        [=](connect_atom, const std::string& host, uint16_t port) {
            connecting_backup(self, host, port);
        },
        [=] (connect_as_backup) {
            aout(self) << "Received Message to connect to lead to server\n";
            self->send(self->state.current_server_actor, connect_as_backup_v, self);
        }
        // [=]() 
    };
}


void connecting_backup(stateful_actor<summa_server_state>* self, const std::string& host, uint16_t port) {
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
                self->state.current_server_actor = hdl;
                self->monitor(hdl);
                aout(self) << "Should become test\n";
                self->become(summa_backup_server(self, hdl));

                },
            [=](const error& err) {
                aout(self) << R"(*** cannot connect to ")" << host << R"(":)" << port
                   << " => " << to_string(err) << std::endl;
        });
}

behavior summa_backup_server(stateful_actor<summa_server_state>* self, const actor& server_actor) {
    aout(self) << "We are the test behaviour\n";
    self->send(server_actor, connect_as_backup_v, self);

    return {

        [=] (connect_as_backup) {
            aout(self) << "We are now connected to the lead server\n";
        },

        [=] (update_with_current_state ) {

        }
    };

}
}