#ifndef SUMMAACTOR_H_ 
#define SUMMAACTOR_H_

#include "SummaManager.h"

using namespace caf;

/**
 * Top Level Actor for Summa. This actor recieves the number of GRUs to compute from main and
 * divides them into jobs that compute one at a time.
 *
 * @param startGRU - starting GRU for the simulation 
 * @param numGRU - total number of GRUs to compute
 * @param fileManager - location of file information for SUMMA
 * @return behavior 
 */
behavior summa_actor(stateful_actor<summa_manager>* self, int startGRU, int numGRU, std::string fileManager, std::string csvOut) {
 	self->state.start = std::chrono::high_resolution_clock::now();

	// Set Variables
	self->state.startGRU = startGRU;
	self->state.numGRU = numGRU;
	self->state.fileManager = fileManager;
	self->state.csvOut = csvOut;

	// Create the job_actor and start SUMMA
	spawnJob(self);

	return {
		[=](done_job, int numFailed) {
			self->state.numFailed += numFailed;
			aout(self) << "Job Done\n"; 
			if (self->state.numGRU <= 0) {

				self->state.end = std::chrono::high_resolution_clock::now();
            		self->state.duration = std::chrono::duration_cast<std::chrono::seconds>
                (self->state.end - self->state.start).count();
				aout(self) << "Total Program Duration:";
                	aout(self) << "     " << self->state.duration << " Seconds\n";
                	aout(self) << "     " << self->state.duration / 60 << " Minutes\n";
                	aout(self) << "     " << (self->state.duration / 60) / 60 << " Hours\n";

				aout(self) << "Program Finished \n";

			} else {
				// spawn a new job
				spawnJob(self);
			}
		},
	};
}


void spawnJob(stateful_actor<summa_manager>* self) {
	// Ensure we do not start a job with too many GRUs
	if (self->state.numGRU > self->state.maxGRUPerJob) {
		// spawn the job actor
		self->state.currentJob = self->spawn(job_actor, self->state.startGRU, self->state.maxGRUPerJob, 
			self->state.fileManager, self->state.outputStrucSize, self->state.csvOut, self);
		
		// Update GRU count
		self->state.numGRU = self->state.numGRU - self->state.maxGRUPerJob;
		self->state.startGRU = self->state.startGRU + self->state.maxGRUPerJob;

	} else {

		self->state.currentJob = self->spawn(job_actor, self->state.startGRU, self->state.numGRU, 
			self->state.fileManager, self->state.outputStrucSize, self->state.csvOut, self);
		self->state.numGRU = 0;
	}
}



#endif