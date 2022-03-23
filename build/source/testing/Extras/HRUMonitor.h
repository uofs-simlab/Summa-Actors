#ifndef HRUMonitor_H_
#define HRUMonitor_H_

#include "HRUinfo.h"
#include "../HRUActor.h"
#include "Job.h"
#include "JobActor.h"
#include "caf/all.hpp"
#include <vector>

// class HRUMonitor {
//   private:
//     std::vector<HRUinfo> hruList;
//     int numHRU;
//     int numCompleted;
//     int numFailed;
//     int startHRU;

//   public:
//     HRUMonitor() {};
//     ~HRUMonitor() {};

//   /**
//    * @brief Initalize HRU and add it to the hruList
//    * 
//    * @param self The parent HRU initalizing the HRU
//    * @param fileManager The location of the file with the SUMMA settings
//    * @param dt_init_start_factor The factor for the timestep length
//    * @param maxRunAttempts The max number of run attempts before we consider the hru failed
//    */
//   void initalizeHRU(caf::actor* self, std::string fileManager, int dt_init_start_factor, int maxRunAttempts) {
//     int startHRU = this->hruList.size() + this->startHRU;
//     int indexHRU = this->hruList.size() + 1; // Fortran reference starts at 1
//     auto hru = self->spawn(hru_actor, startHRU, indexHRU, fileManager, self);
//     HRUinfo infoHRU = HRUinfo(startHRU, indexHRU, hru, dt_init_start_factor, maxRunAttempts);
//     this->hruList.push_back(infoHRU);
//   }

//   int getNewStartHRU()

//   bool isAllHRUInitialized() {
//     return this->hruList.size() == this->numHRU;
//   }


// };


#endif