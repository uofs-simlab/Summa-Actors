#include "caf/all.hpp"
#include "caf/io/all.hpp"
#include "message_atoms.hpp"
#include "summa_actor.hpp"
#include "global.hpp"
#include "job_actor.hpp"
#include "json.hpp"
#include <iostream>
#include <chrono>
#include <string>
#include <fstream>

using json = nlohmann::json;

namespace caf {

behavior summa_actor(stateful_actor<summa_actor_state>* self, int startGRU, int numGRU, 
	Summa_Actor_Settings summa_actor_settings, File_Access_Actor_Settings file_access_actor_settings,
    Job_Actor_Settings job_actor_settings, HRU_Actor_Settings hru_actor_settings, actor parent) {

 	// Set Timing Variables
	self->state.summa_actor_timing = TimingInfo();
	self->state.summa_actor_timing.addTimePoint("total_duration");
	self->state.summa_actor_timing.updateStartPoint("total_duration");
	// Set Variables
	self->state.startGRU = startGRU;
	self->state.numGRU = numGRU;
	self->state.parent = parent;

	self->state.summa_actor_settings = summa_actor_settings;
	self->state.file_access_actor_settings = file_access_actor_settings;
	self->state.job_actor_settings = job_actor_settings;
	self->state.hru_actor_settings = hru_actor_settings;

	// Create the job_actor and start SUMMA
	spawnJob(self);

	return {
		[=](done_job, int numFailed, double job_duration, double read_duration, double write_duration) {
			auto& timing_info = self->state.timing_info_for_jobs;


			self->state.numFailed += numFailed;

			timing_info.job_duration.push_back(job_duration);
			timing_info.job_read_duration.push_back(read_duration);
			timing_info.job_write_duration.push_back(write_duration);

			if (self->state.numGRU <= 0) {
				self->state.summa_actor_timing.updateEndPoint("total_duration");



				for (size_t i = 0; i < timing_info.job_duration.size(); ++i) {
					
   				 	 
    				aout(self) << "\n________________Job " << ++i << " Info_____________\n"
               				   << "Job Duration = " << timing_info.job_duration[i] << "\n"
               				   << "Job Read Duration = " << timing_info.job_read_duration[i - 1] << "\n"
               				   << "Job Write Duration = " << timing_info.job_write_duration[i - 1] << "\n"
							   << "_____________________________________________________\n\n";
				}


				double total_read_duration = std::accumulate(timing_info.job_read_duration.begin(),
															 timing_info.job_read_duration.end(),
															 0.0);
				double total_write_duration = std::accumulate(timing_info.job_write_duration.begin(),
															  timing_info.job_write_duration.end(),
															  0.0);
				
				aout(self) << "\n________________SUMMA_ACTOR TIMING INFO________________\n"
						   << "Total Duration = " << self->state.summa_actor_timing.getDuration("total_duration").value_or(-1.0) << " Seconds\n"
						   << "Total Duration = " << self->state.summa_actor_timing.getDuration("total_duration").value_or(-1.0) / 60 << " Minutes\n"
						   << "Total Duration = " << (self->state.summa_actor_timing.getDuration("total_duration").value_or(-1.0) / 60) / 60 << " Hours\n"
						   << "Total Read Duration = " << total_read_duration  << "Seconds\n"
						   << "Total Write Duration = " << total_write_duration << "Seconds\n"
						   << "___________________Program Finished__________________\n"; 



				self->send(self->state.parent, done_batch_v, 
						   self->state.summa_actor_timing.getDuration("total_duration").value_or(-1.0), 
						   total_read_duration,
						   total_write_duration);		

			} else {
				spawnJob(self);
			}
		},

		[=](err) {
			aout(self) << "Unrecoverable Error: Attempting To Fail Gracefully\n";
			self->quit();
		}
	};
}


void spawnJob(stateful_actor<summa_actor_state>* self) {
	// Ensure we do not start a job with too many GRUs
	if (self->state.numGRU > self->state.summa_actor_settings.max_gru_per_job) {
		// spawn the job actor
		aout(self) << "\n Starting Job with startGRU = " << self->state.startGRU << "\n";
		self->state.currentJob = self->spawn(job_actor, self->state.startGRU, self->state.summa_actor_settings.max_gru_per_job, 
											 self->state.file_access_actor_settings, self->state.job_actor_settings, 
											 self->state.hru_actor_settings, self);
		
		// Update GRU count
		self->state.numGRU = self->state.numGRU - self->state.summa_actor_settings.max_gru_per_job;
		self->state.startGRU = self->state.startGRU + self->state.summa_actor_settings.max_gru_per_job;

	} else {

		self->state.currentJob = self->spawn(job_actor, self->state.startGRU, self->state.numGRU, 
										     self->state.file_access_actor_settings, self->state.job_actor_settings, 
											 self->state.hru_actor_settings, self);
		self->state.numGRU = 0;
	}
}

} // end namespace
