#include <molpro/ProfilerSingle.h>
#ifdef HAVE_MPI_H
#include "mpi.h"
#endif
#include <chrono>
#include <thread>

void sleep_milliseconds(const std::string &name, int repeats) {
  auto p = molpro::ProfilerSingle::instance()->push(name);
  for (size_t i = 0; i < repeats; ++i) {
    std::this_thread::sleep_for(std::chrono::milliseconds{1});
  }
  p += repeats;
}

void run(const std::string &name) {
  // initial call to create the profiler with necessary settings
  molpro::ProfilerSingle::create("Singleton Example: " + name, molpro::Profiler::wall);
  sleep_milliseconds("fast", 200);
  sleep_milliseconds("medium", 300);
  sleep_milliseconds("slow", 500);
  molpro::ProfilerSingle::instance()->stop();
}

int main(int argc, char *argv[]) {
#ifdef HAVE_MPI_H
  MPI_Init(&argc, &argv);
#else
#endif
  {
    auto p = molpro::ProfilerSingle::create("", false, false);
#ifdef HAVE_MPI_H
    auto p2 = molpro::ProfilerSingle::create("", MPI_COMM_WORLD, false, false);
#else
    auto p2 = molpro::ProfilerSingle::create("", 0, false, false);
#endif
  }
  run("job 1");
  run("job 2");
  for (const auto &profiler : molpro::ProfilerSingle::profilers()) {
    std::cout << *profiler.second;
  }
#ifdef HAVE_MPI_H
  MPI_Finalize();
#endif
  return 0;
}
