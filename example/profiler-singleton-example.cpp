#include <ProfilerSingle.h>
#ifdef HAVE_MPI_H
#include "mpi.h"
#endif
#include <chrono>
#include <thread>

/*!
 * @brief Utility function that exposes default profiler for the program
 *
 * This avoids having to hard-code the name through out the application
 * and simplifies refactoring
 *
 */
inline std::shared_ptr<Profiler> profiler(Profiler::sortMethod sortMethod = Profiler::wall, int level = INT_MAX) {
    return ProfilerSingle::Instance("Profiler Singleton Example", sortMethod, level);
}

void sleep_milliseconds(const std::string &name, int repeats) {
    auto p = profiler()->push(name);
    for (size_t i = 0; i < repeats; ++i) {
        std::this_thread::sleep_for(std::chrono::milliseconds{1});
    }
    p += repeats;
}

int main(int argc, char *argv[]) {
#ifdef HAVE_MPI_H
    MPI_Init(&argc, &argv);
#endif
    // initial call to create the profiler with necessary settings
    profiler(Profiler::wall);
    sleep_milliseconds("fast", 200);
    sleep_milliseconds("medium", 300);
    sleep_milliseconds("slow", 500);
    std::cout << *profiler() << std::endl;
#ifdef HAVE_MPI_H
    MPI_Finalize();
#endif
    return 0;
}
