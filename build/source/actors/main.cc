#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "summa_actor/SummaActor.h"
#include "global/messageAtoms.h"
#include "global/global.h"

#include <string>
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
        std::string configPath = ""; // master file 
        bool debugMode = false;
        uint16_t port = 4444;
        std::string host = "localhost";
        bool server_mode = false;
    
    config() {
        opt_group{custom_options_, "global"}
            .add(startGRU, "gru,g", "Starting GRU Index")
            .add(countGRU, "numGRU,n", "Total Number of GRUs")
            .add(configPath, "config,c", "Path name of the config directory")
            .add(debugMode, "debug-mode,d", "enable debug mode")
            .add(port, "port,p", "set port")
            .add(host, "host,h", "set Host (ignored in server mode)")
            .add(server_mode, "server-mode,s", "enable server mode");
    }
};

struct server_state {

};

behavior say_hello(stateful_actor<server_state>* self) {
    aout(self) << "Hello" << std::endl;
    return {
        [=](int i) {
            aout(self) << "Recieved" << i << std::endl;
            return "Got It";
        },
    };
}

void run_server(actor_system& system, const config& cfg) {
    scoped_actor self{system};
    auto hello = system.spawn(say_hello);
    aout(self) << "SEVER" << std::endl;
    aout(self) << "Attempting to publish actor" << cfg.port << std::endl;
    auto is_port = io::publish(hello, cfg.port);
    if (!is_port) {
        std::cerr << "********PUBLISH FAILED*******" << to_string(is_port.error()) << std::endl;
        return;
    }
    aout(self) << "Successfully Published" << *is_port << std::endl;
    std::string dummy;
    std::getline(std::cin, dummy);
    std::cout << "...cya" << std::endl;
    anon_send_exit(hello, exit_reason::user_shutdown);
}

struct client_state {
    strong_actor_ptr current_server;
};

behavior unconnected(stateful_actor<client_state>*);
void connecting(stateful_actor<client_state>*, const std::string& host, uint16_t port);
behavior running(stateful_actor<client_state>*, const actor& say_hello);

behavior client(stateful_actor<client_state>* self) {
    self->set_down_handler([=](const down_msg& dm){
        if(dm.source == self->state.current_server) {
            aout(self) << "*** Lost Connection to Server" << std::endl;
            self->state.current_server = nullptr;
            self->become(unconnected(self));
        }
    });
    return unconnected(self);
}

behavior unconnected(stateful_actor<client_state>* self) {
    return {
        [=] (connect_atom, const std::string& host, uint16_t port) {
            connecting(self, host, port);
        },
    };
}

void connecting(stateful_actor<client_state>* self, const std::string& host, uint16_t port) {
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

behavior running(stateful_actor<client_state>* self, const actor& say_hello) {
    aout(self) << "HERE" << std::endl;
    self->send(say_hello, infinite, 80);
    return {
        [=](std::string test) {
            aout(self) << test << std::endl;
        }
    };
}


void run_client(actor_system& system, const config& cfg) {
    scoped_actor self{system};
    aout(self) << "CLIENT" << std::endl;
    auto c = system.spawn(client);
    if (!cfg.host.empty() && cfg.port > 0) {
        anon_send(c, connect_atom_v, cfg.host, cfg.port);
    } else {
        aout(self) << "No Server Config" << std::endl;
    }
}


void caf_main(actor_system& sys, const config& cfg) {
    // scoped_actor self{sys};
    // if (cfg.startGRU == -1) {
    //     aout(self) << "Starting GRU was not defined!! " << 
    //         "startGRU is set with the \"-g\" option\n";
    //     aout(self) << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
    //     return;
    // }
    // if (cfg.countGRU == -1) {
    //     aout(self) << "Number of GRUs was not defined!! " <<
    //         "countGRU is set with the \"-n\" option\n";
    //     aout(self) << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
    //     return;
    // }
    // if (cfg.configPath == "") {
    //     aout(self) << "File Manager was not defined!! " << 
    //         "fileManger is set with the \"-c\" option\n";
    //     aout(self) << "EXAMPLE: ./summaMain -g 1 -n 10 -c location/of/config \n";
    //     return;
    // }
    // if (cfg.debugMode) {
    //     aout(self) << "Starting SUMMA-Actors in DebugMode\n";
    //     debug = true;
    // }
    // start SUMMA
    auto system = cfg.server_mode ? run_server : run_client;
    system(sys, cfg);


    // auto summa = sys.spawn(summa_actor, cfg.startGRU, cfg.countGRU, cfg.configPath);
}

CAF_MAIN(id_block::summa, io::middleman)