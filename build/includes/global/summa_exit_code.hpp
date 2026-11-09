#pragma once
#include <atomic>

// process exit code: EXIT_FAILURE after a fatal error or failed GRUs, returned by main once all actors finish
extern std::atomic<int> summa_exit_code;
