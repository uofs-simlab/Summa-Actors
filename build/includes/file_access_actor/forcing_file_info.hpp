#pragma once

class Forcing_File_Info {
    private:
        int file_ID;
        int num_steps;
        bool is_loaded;
    
    public:
        Forcing_File_Info(int file_ID);

        int getNumSteps();

        bool isFileLoaded();

        void updateIsLoaded();

        void updateNumSteps(int num_steps);

};