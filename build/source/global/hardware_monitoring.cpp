#include "hardware_monitoring.hpp"

int getBatchSize() {
    unsigned int threads = std::thread::hardware_concurrency();

    if (threads == 0)
        return 1000;
    else if (threads < 5)
        return 16 * threads;
    else if (threads < 9)
        return 8 * threads;
    else if (threads > 16)
        return 4 * threads;

    // For 9 <= threads <= 16
    return 8 * threads;
}

