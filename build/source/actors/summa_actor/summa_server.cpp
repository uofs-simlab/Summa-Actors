#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include <string>
#include "batch_manager.hpp"
#include "summa_server.hpp"
#include "message_atoms.hpp"
#include "global.hpp"
#include <optional>


namespace caf {

behavior summa_server(stateful_actor<summa_server_state>* self, Distributed_Settings distributed_settings, 
    Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings) {
        
    aout(self) << "Summa Server has Started \n";
    self->state.distributed_settings = distributed_settings;
    self->state.summa_actor_settings = summa_actor_settings; 
    self->state.file_access_actor_settings = file_access_actor_settings;
    self->state.job_actor_settings = job_actor_settings;
    self->state.hru_actor_settings = hru_actor_settings;

    self->state.client_container = new Client_Container();
    self->state.batch_container = new Batch_Container(
            self->state.distributed_settings.total_hru_count,
            self->state.distributed_settings.num_hru_per_batch);
    
    self->state.batch_container->printBatches();

    return {
        /**
         * @brief A message from a client requresting to connect
         * 
         * @param client the actor_ref of the client_actor 
         * (used to send messages to the client_actor)
         * @param hostname human readable hostname of the machine that the actor is running on
         */
        [=](connect_to_server, actor client_actor, std::string hostname) {

            aout(self) << "Actor trying to connect with hostname " << hostname << "\n";
            self->state.client_container->addClient(client_actor, hostname);

            // Tell client they are connected
            self->send(client_actor, connect_to_server_v, self->state.client_container->getClientID(client_actor), 
                self->state.summa_actor_settings, self->state.file_access_actor_settings, self->state.job_actor_settings, 
                self->state.hru_actor_settings);
            
            std::optional<Batch> batch = self->state.batch_container->assignBatch(hostname, client_actor);
            if (batch.has_value()) {
                self->send(client_actor, batch.value());
            } else {
                aout(self) << "no more batches left to assign\n";
                aout(self) << "we are not done yet. Clients could Fail\n";
            }
            
        },

        [=](done_batch, actor client_actor, int client_id, Batch& batch) {
            aout(self) << "Recieved Completed Batch From Client\n";
    
            aout(self) << batch.toString() << "\n\n";

            self->state.batch_container->updateBatch_success(batch, self->state.csv_output_name);
            aout(self) << "******************\n" << "Batches Remaining: " << 
                self->state.batch_container->getBatchesRemaining() << "\n******************\n\n";

            std::optional<Batch> new_batch = self->state.batch_container->assignBatch(
                self->state.client_container->getHostname_ByClientID(client_id), client_actor);
            if (new_batch.has_value()) {
                self->send(client_actor, new_batch.value());
            } else {
                aout(self) << "no more batches left to assign\n";
                aout(self) << "we are not done yet. Clients could Fail\n";
            }
        }
    };
}


void initializeCSVOutput(std::string csv_output_name) {
    std::ofstream csv_output;
    csv_output.open(csv_output_name, std::ios_base::out);
    csv_output << 
        "Batch_ID,"  <<
        "Start_HRU," <<
        "Num_HRU,"   << 
        "Hostname,"  <<
        "Run_Time,"  <<
        "Read_Time," <<
        "Write_Time\n";
    csv_output.close();
}

} // end namespace
