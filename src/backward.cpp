// Needed for:
//	- source filename
//	- line and column numbers
//	- source code snippet (assuming the file is accessible)
//
// On linux:
#if defined(__linux) || defined(__linux__)
#define BACKWARD_HAS_DW 1
// On macOS:
#elif defined(__APPLE__)
#define BACKWARD_HAS_DWARF 1
#endif

// Gives slightly better backtraces, but this is annoying to get working on
// windows
#ifndef _WIN32
#define BACKWARD_HAS_LIBUNWIND 1
#endif

#include "backward.h"

// backward::SignalHandling sh;
//
backward::SignalHandling *sh;

#ifdef _WIN32
// Join the reporter thread before ExitProcess terminates it at an arbitrary
// point (e.g., while still starting up), which can hang libstdc++'s
// DLL_PROCESS_DETACH handler.
static void unregister_sh() {
    delete sh;
    sh = nullptr;
}
#endif

extern "C" {
// Let's use a c interface so we can easily setup a signal handler
bool register_sh() {
    if (sh == nullptr) {
        sh = new backward::SignalHandling();
#ifdef _WIN32
        // Registered after the constructor initializes SignalHandling's
        // statics, so this runs before their destructors.
        std::atexit(unregister_sh);
#endif
    }
    return sh->loaded();
}
}
