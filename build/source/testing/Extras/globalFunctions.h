#ifndef GLOBALFUNCTIONS_H_
#define GLOBALFUNCTIONS_H_


#include <chrono>
#include <unistd.h>
#include <ios>
#include <iostream>
#include <fstream>
#include <string>


/* This is a function that checks if this actor
* should send a message to its parent letting it know it
* is still running.
* timeSinceCheckIn = the last time that we sent a message to our parent
* maxCheckInDuration = The max amount of time in minutes we can go without
* checking in with our parent
*/
bool isCheckInTime(std::chrono::time_point<std::chrono::system_clock> timeSinceCheckIn, 
    int maxCheckInDuration) {

    return (std::chrono::duration_cast<std::chrono::minutes>
        (std::chrono::high_resolution_clock::now() - timeSinceCheckIn).count() >  maxCheckInDuration);

}

void process_mem_usage(double& vm_usage, double& resident_set)
{
   using std::ios_base;
   using std::ifstream;
   using std::string;

   vm_usage     = 0.0;
   resident_set = 0.0;

   // 'file' stat seems to give the most reliable results
   //
   ifstream stat_stream("/proc/self/stat",ios_base::in);

   // dummy vars for leading entries in stat that we don't care about
   //
   string pid, comm, state, ppid, pgrp, session, tty_nr;
   string tpgid, flags, minflt, cminflt, majflt, cmajflt;
   string utime, stime, cutime, cstime, priority, nice;
   string O, itrealvalue, starttime;

   // the two fields we want
   //
   unsigned long vsize;
   long rss;

   stat_stream >> pid >> comm >> state >> ppid >> pgrp >> session >> tty_nr
               >> tpgid >> flags >> minflt >> cminflt >> majflt >> cmajflt
               >> utime >> stime >> cutime >> cstime >> priority >> nice
               >> O >> itrealvalue >> starttime >> vsize >> rss; // don't care about the rest

   stat_stream.close();

   long page_size_kb = sysconf(_SC_PAGE_SIZE) / 1024; // in case x86-64 is configured to use 2MB pages
   vm_usage     = vsize / 1024.0;
   resident_set = rss * page_size_kb;
}

#endif