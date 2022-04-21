#ifndef SUMMAACTOR_H_ 
#define SUMMAACTOR_H_

#include "SummaManager.h"

using namespace caf;
using json = nlohmann::json;
/**
 * Top Level Actor for Summa. This actor recieves the number of GRUs to compute from main and
 * divides them into jobs that compute one at a time.
 *
 * @param startGRU - starting GRU for the simulation 
 * @param numGRU - total number of GRUs to compute
 * @param configPath - location of file information for SUMMA
 * @return behavior 
 */
behavior summa_actor(stateful_actor<summa_manager>* self, int startGRU, int numGRU, std::string configPath) {
 	self->state.start = std::chrono::high_resolution_clock::now();
	// Set Variables
	self->state.startGRU = startGRU;
	self->state.numGRU = numGRU;
	self->state.configPath = configPath;

	parseSettings(self, configPath);
	aout(self) << "SETTINGS FOR SUMMA_ACTOR\n";
	aout(self) << "Output Structure Size = " << self->state.outputStrucSize << "\n";
	aout(self) << "Max GRUs Per Job = " << self->state.maxGRUPerJob << "\n";

	// Create the job_actor and start SUMMA
	spawnJob(self);

	return {
		[=](done_job, int numFailed) {
			self->state.numFailed += numFailed;
			aout(self) << "Job Done\n"; 
			if (self->state.numGRU <= 0) {

				self->state.end = std::chrono::high_resolution_clock::now();
            	self->state.duration = calculateTime(self->state.start, self->state.end);
				aout(self) << "Total Program Duration:\n";
            	aout(self) << "     " << self->state.duration / 1000  << " Seconds\n";
            	aout(self) << "     " << (self->state.duration / 1000) / 60  << " Minutes\n";
            	aout(self) << "     " << ((self->state.duration / 1000) / 60) / 60 << " Hours\n";

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

void parseSettings(stateful_actor<summa_manager>* self, std::string configPath) {
	json settings;
	std::string SummaActorsSettings = "/Summa_Actors_Settings.json";
	std::ifstream settings_file(configPath + SummaActorsSettings);
	settings_file >> settings;
	settings_file.close();
	
	if (settings.find("SummaActor") != settings.end()) {
		json SummaActorConfig = settings["SummaActor"];
		
		// Find the desired OutputStrucSize
		if (SummaActorConfig.find("OuputStructureSize") != SummaActorConfig.end()) {
			self->state.outputStrucSize = SummaActorConfig["OuputStructureSize"];
		} else {
			aout(self) << "Error Finding OutputStructureSize in JOSN - Reverting to default value\n";
			self->state.outputStrucSize = 250;
		}

		// Find the desired maxGRUPerJob size
		if (SummaActorConfig.find("maxGRUPerJob") != SummaActorConfig.end()) {
			self->state.maxGRUPerJob = SummaActorConfig["maxGRUPerJob"];
		} else {
			aout(self) << "Error Finding maxGRUPerJob in JOSN - Reverting to default value\n";
			self->state.maxGRUPerJob = 500;
		}

	} else {
		aout(self) << "Error Finding SummaActor in JSON - Reverting to default values\n";
		self->state.outputStrucSize = 250;
		self->state.maxGRUPerJob = 500;
	}
}



#endif