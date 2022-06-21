#include "forcing_file_info.hpp"

Forcing_File_Info::Forcing_File_Info(int file_ID) {
    this->file_ID = file_ID;
    this->num_steps = 0;
    this->is_loaded = false;
}

int Forcing_File_Info::getNumSteps() {
    return this->num_steps;
}

bool Forcing_File_Info::isFileLoaded() {
    return this->is_loaded;
}

void Forcing_File_Info::updateIsLoaded() {
    this->is_loaded = true;
}


void Forcing_File_Info::updateNumSteps(int num_steps) {
    this->num_steps = num_steps;
    this->is_loaded = true;
}


