#include <ProfilerSingle.h>
#ifdef HAVE_MPI_H
#include "mpi.h"
#endif
#include <chrono>
#include <thread>

void sleep_milliseconds(const std::string & name, int repeats) {
    auto p = ProfilerSingle::Instance()->push(name);
    for (size_t i = 0; i < repeats; ++i) {
        std::this_thread::sleep_for(std::chrono::milliseconds{1});
    }
    p += repeats;
}

int main(int argc, char *argv[]) {
#ifdef HAVE_MPI_H
    MPI_Init(&argc, &argv);
#endif
    auto profiler = ProfilerSingle::Instance("Profiler Singleton Example", Profiler::wall);
    sleep_milliseconds("fast", 200);
    sleep_milliseconds("medium", 300);
    sleep_milliseconds("slow", 500);
    std::cout << *ProfilerSingle::Instance() << std::endl;
#ifdef HAVE_MPI_H
    MPI_Finalize();
#endif
    return 0;
}
