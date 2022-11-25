#pragma once

#include "caf/all.hpp"
#include <iostream>
#include <fstream>

class GRUinfo {
    private:
        int refGRU; // This will be the same as the refGRU
        int indxGRU;
        caf::actor GRU;

        // Variable to update
        int dt_init;

        // Completed Information
        int currentAttempt;
        int maxAttempts;
        bool completed;
        bool failed;

        // Timing information for the GRU
        double runTime;
        double initDuration;
        double forcingDuration;
        double runPhysicsDuration;
        double writeOutputDuration;

        public:
            
            // Constructor
            GRUinfo(int refGRU, int indxGRU, caf::actor gru, int dt_init, int maxAttempts);

            // Deconstructor
            ~GRUinfo();


            int getRefGRU();

            int getIndxGRU();

            int getDt_init();

            caf::actor getActor();

            void updateGRU(caf::actor gru);

            void updateFailed();

            void updateCompletedToTrue();

            void updateDt_init();

            void updateCurrentAttempt();

            bool isMaxAttemptsReached();

            bool isFailed();

            bool isCompleted();

            void doneRun(double runTime, double initDuration, double forcingDuration,
                double runPhysicsDuration, double writeOutputDuration);

            void writeSuccess(std::string fileName, std::string hostname);

            void writeFail(std::string fileName);

            void printOutput();
};