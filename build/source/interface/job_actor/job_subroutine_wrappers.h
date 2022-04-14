#ifndef JOB_SUBROUTINE_WRAPPERS_H_
#define JOB_SUBROUTINE_WRAPPERS_H_

extern "C" {
    void initGlobals(char const*str1, int* totalGRUs, int* totalHRUs, 
        int* numGRUs, int* numHRUs, int* startGRUIndex, int* err);

    void cleanUpJobActor(int* err);
    
}

#endif