#include <ProfilerSingle.h>
#ifdef HAVE_MPI_H
#include "mpi.h"
#endif
#include <chrono>
#include <thread>


void sleep_milliseconds(const std::string &name, int repeats) {
    auto p = ProfilerSingle::instance()->push(name);
    for (size_t i = 0; i < repeats; ++i) {
        std::this_thread::sleep_for(std::chrono::milliseconds{1});
    }
    p += repeats;
}

void run(const std::string &name) {
    // initial call to create the profiler with necessary settings
    ProfilerSingle::create("Singleton Example: " + name, Profiler::wall);
    sleep_milliseconds("fast", 200);
    sleep_milliseconds("medium", 300);
    sleep_milliseconds("slow", 500);
    ProfilerSingle::instance()->stop();
}

int main(int argc, char *argv[]) {
#ifdef HAVE_MPI_H
    MPI_Init(&argc, &argv);
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
#else
    int rank{0};
#endif
    run("job 1");
    run("job 2");
    for (const auto &profiler : ProfilerSingle::profilers) {
        auto summary = profiler.second->str();
        if (rank == 0)
            std::cout << summary << std::endl;
    }
#ifdef HAVE_MPI_H
    MPI_Finalize();
#endif
    return 0;
}
