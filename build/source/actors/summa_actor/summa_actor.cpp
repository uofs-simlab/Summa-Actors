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

behavior summa_actor(stateful_actor<summa_actor_state>* self, int startGRU, int numGRU, std::string configPath, actor parent) {
 	self->state.start = std::chrono::high_resolution_clock::now();
	// Set Variables
	self->state.startGRU = startGRU;
	self->state.numGRU = numGRU;
	self->state.configPath = configPath;
	self->state.parent = parent;

	self->state.outputStrucSize = getSettings(self->state.configPath, "SummaActor", "OuputStructureSize", 
		self->state.outputStrucSize).value_or(250);
	self->state.maxGRUPerJob = getSettings(self->state.configPath, "SummaActor", "maxGRUPerJob",
		self->state.maxGRUPerJob).value_or(100);

	aout(self) << "SETTINGS FOR SUMMA_ACTOR\n";
	aout(self) << "Output Structure Size = " << self->state.outputStrucSize << "\n";
	aout(self) << "Max GRUs Per Job = " << self->state.maxGRUPerJob << "\n";

	// Create the job_actor and start SUMMA
	spawnJob(self);

	return {
		[=](done_job, int numFailed, int job_duration, int read_duration, int write_duration) {
			self->state.numFailed += numFailed;
			aout(self) << "Job Done\n"; 
			if (self->state.numGRU <= 0) {

				self->state.end = std::chrono::high_resolution_clock::now();
            	self->state.duration = calculateTime(self->state.start, self->state.end);
				
            	self->state.duration = self->state.duration / 1000; // Convert to milliseconds

				
				aout(self) << "Total Program Duration:\n";
            	aout(self) << "     " << self->state.duration / 1000  << " Seconds\n";
            	aout(self) << "     " << (self->state.duration / 1000) / 60  << " Minutes\n";
            	aout(self) << "     " << ((self->state.duration / 1000) / 60) / 60 << " Hours\n";
				aout(self) << "Program Finished \n";

				self->send(self->state.parent, done_batch_v, self->state.duration);		

			} else {
				// spawn a new job
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
	if (self->state.numGRU > self->state.maxGRUPerJob) {
		// spawn the job actor
		aout(self) << "\n Starting Job with startGRU = " << self->state.startGRU << "\n";
		self->state.currentJob = self->spawn(job_actor, self->state.startGRU, self->state.maxGRUPerJob, 
			self->state.configPath, self->state.outputStrucSize, self);
		
		// Update GRU count
		self->state.numGRU = self->state.numGRU - self->state.maxGRUPerJob;
		self->state.startGRU = self->state.startGRU + self->state.maxGRUPerJob;

	} else {

		self->state.currentJob = self->spawn(job_actor, self->state.startGRU, self->state.numGRU, 
			self->state.configPath, self->state.outputStrucSize, self);
		self->state.numGRU = 0;
	}
}
} // end namespace
