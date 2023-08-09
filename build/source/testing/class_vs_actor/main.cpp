#include "caf/all.hpp"
#include "GRU.hpp"
#include <chrono>


using namespace caf;
CAF_BEGIN_TYPE_ID_BLOCK(summa_test, first_custom_type_id)
    CAF_ADD_ATOM(summa_test, start_gru)
    CAF_ADD_ATOM(summa_test, get_info)
    CAF_ADD_TYPE_ID(summa_test, (std::tuple<int, int, double, double, double, double, double>))
CAF_END_TYPE_ID_BLOCK(summa_test)

struct gru_state_test {
    int global_gru_index;
    int local_gru_index;
    int dt_init_factor;
    int attempts_left;
    double run_time;
    double init_duration;
    double forcing_duration;
    double run_physics_duration;
    double write_output_duration;
};


behavior gru_actor(stateful_actor<gru_state_test>* self, int global_gru_index, int local_gru_index) {
    self->state.global_gru_index = global_gru_index;
    self->state.local_gru_index = local_gru_index;
    self->state.run_time = 0.0;
    self->state.init_duration = 0.0;
    self->state.forcing_duration = 0.0;
    self->state.run_physics_duration = 0.0;
    self->state.write_output_duration = 0.0;

    return {
        [=](get_info) {

            return std::make_tuple(self->state.global_gru_index, 
                                   self->state.local_gru_index, 
                                   self->state.run_time, 
                                   self->state.init_duration, 
                                   self->state.forcing_duration, 
                                   self->state.run_physics_duration, 
                                   self->state.write_output_duration);
        },

    };


}




void caf_main(actor_system& sys) {
    scoped_actor self{sys};
    int num_actors = 10000;

    auto gru_list = std::vector<actor>{};
    auto container_list = std::vector<GRU*>{};

    for (int i = 0; i < num_actors; ++i) {
        gru_list.push_back(sys.spawn(gru_actor, i, i));

        container_list.push_back(new GRU(i, i, gru_list[i], 1, 1));

    }

    // Retreive the information from the GRU actors

    std::chrono::time_point<std::chrono::system_clock> actor_start, actor_end;

    actor_start = std::chrono::system_clock::now();
    for (auto& actor : gru_list) {
        self->request(actor, infinite, get_info_v).receive(
            [&](std::tuple<int, int, double, double, double, double, double> info) {
                // aout(self) << "Recieved\n";
                int global_gru_index = std::get<0>(info);
                int local_gru_index = std::get<1>(info);
                double run_time = std::get<2>(info);
                double init_duration = std::get<3>(info);
                double forcing_duration = std::get<4>(info);
                double run_physics_duration = std::get<5>(info);
                double write_output_duration = std::get<6>(info);

                aout(self) << "global_gru_index = " << global_gru_index 
                           << "local_gru_index = " << local_gru_index
                           << "run_time = " << run_time
                           << "init_duration = " << init_duration
                           << "forcing_duration = " << forcing_duration
                           << "run_physics_duration = " << run_physics_duration
                           << "write_output_duration = " << write_output_duration << "\n";
                            
                self->send_exit(actor, exit_reason::user_shutdown);
            },
            [&](error& err) {
                aout(self) << to_string(err) << std::endl;
            });
    }
    actor_end = std::chrono::system_clock::now();

    std::chrono::time_point<std::chrono::system_clock> class_start, class_end;

    class_start = std::chrono::system_clock::now();
    for(auto& gru : container_list) {
        int global_gru_index = gru->getGlobalGRUIndex();
        int local_gru_index = gru->getLocalGRUIndex();
        double run_time = gru->getRunTime();
        double init_duration = gru->getInitDuration();
        double forcing_duration = gru->getForcingDuration();
        double run_physics_duration = gru->getRunPhysicsDuration();
        double write_output_duration = gru->getWriteOutputDuration();

        aout(self) << "global_gru_index = " << global_gru_index 
                   << "local_gru_index = " << local_gru_index
                   << "run_time = " << run_time
                   << "init_duration = " << init_duration
                   << "forcing_duration = " << forcing_duration
                   << "run_physics_duration = " << run_physics_duration
                   << "write_output_duration = " << write_output_duration << "\n";
        delete gru;
    }
    class_end = std::chrono::system_clock::now();

    std::chrono::duration<double> elapsed_seconds_actor = actor_end - actor_start;
    aout(self) << "Actor Elapsed time: " << elapsed_seconds_actor.count() << "s\n";

    std::chrono::duration<double> elapsed_seconds_class = class_end - class_start;
    aout(self) << "Class Elapsed time: " << elapsed_seconds_class.count() << "s\n";




}

CAF_MAIN(id_block::summa_test)