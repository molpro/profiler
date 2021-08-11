#include <chrono>
#include <iostream>
#include <memory>
#include <molpro/Profiler.h>
#include <molpro/profiler/report.h>
#include <sstream>
#include <sys/time.h>
#include <thread>
#ifdef MOLPRO_PROFILER_MPI
#include <mpi.h>
#endif
struct ElapsedTime {
  using time_point = decltype(std::chrono::system_clock::now());
  explicit ElapsedTime(size_t n) : n_operations(n) {
    start = std::chrono::system_clock::now();
    stop_ = start;
  }
  void stop() { stop_ = std::chrono::system_clock::now(); }
  ~ElapsedTime() {
    if (stop_ == start)
      stop();
    int rank = 0;
#ifdef MOLPRO_PROFILER_MPI
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
#endif
    if (rank == 0) {
      std::cout << "Elapsed time: " << (std::chrono::duration<double>(stop_ - start)).count() << std::endl;
      std::cout << "Elapsed time per call: " << (std::chrono::duration<double>(stop_ - start)).count() / n_operations
                << std::endl;
      std::cout << std::endl;
    }
  }
  size_t n_operations;
  time_point start;
  time_point stop_;
};

void delay() {
#ifdef MOLPRO_PROFILER_MPI
  int delay = 2000;
  int rank = 0;
  MPI_Barrier(MPI_COMM_WORLD);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  std::this_thread::sleep_for(std::chrono::milliseconds{delay * rank});
#endif
}

int main(int argc, char* argv[]) {
  int rank = 0;
#ifdef MOLPRO_PROFILER_MPI
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
#endif
  size_t repeat = 1000000;
  std::cout << repeat << " instances of Profiler::push()" << std::endl;
  if (argc > 1)
    repeat = std::stoi(argv[1]);
  {
    molpro::Profiler tp{"test tree profiler: depth = 1, rank = " + std::to_string(rank)};
    auto et = ElapsedTime(repeat);
    for (size_t i = 0; i < repeat; i++) {
      tp.push("test");
    }
    et.stop();
    tp.stop_all();
#ifdef MOLPRO_PROFILER_MPI
    std::stringstream out;
    report(tp, std::cout, MPI_COMM_WORLD);
    delay();
    std::cout << out.str() << std::endl;
    delay();
#endif
    report(tp, std::cout);
    delay();
  }
  {
    molpro::Profiler tp{"test tree profiler: depth = 1, rank = " + std::to_string(rank)};
    auto et = ElapsedTime(repeat);
    for (size_t i = 0; i < repeat / 10000; i++) {
      auto p1 = tp.push("test1");
      for (auto ii = 0; ii < 10; ii++) {
        auto p2 = tp.push("test2");
        for (auto iii = 0; iii < 10; iii++) {
          auto p3 = tp.push("test3");
          for (auto iiii = 0; iiii < 10; iiii++) {
            auto p4 = tp.push("test4");
            for (auto iiiii = 0; iiiii < 10; iiiii++) {
              auto p5 = tp.push("test5");
            }
          }
        }
      }
    }
    tp.stop_all();
    et.stop();
#ifdef MOLPRO_PROFILER_MPI
    std::stringstream out;
    report(tp, out, MPI_COMM_WORLD);
    delay();
    std::cout << out.str() << std::endl;
    delay();
#endif
    report(tp, std::cout, true);
#ifdef MOLPRO_PROFILER_MPI
    delay();
#endif
    report(tp, std::cout, false);
  }
  {
    molpro::Profiler p{"tree profiler: operations , rank = " + std::to_string(rank)};
    auto et = ElapsedTime(repeat);
    for (size_t i = 0; i < repeat; i++) {
      auto proxy = p.push("test");
      proxy += 7;
    }
    et.stop();
    p.stop_all();
#ifdef MOLPRO_PROFILER_MPI
    std::stringstream out;
    report(p, out, MPI_COMM_WORLD);
    if (rank == 0)
      std::cout << out.str() << std::endl;
    delay();
#endif
    std::cout << p << std::endl;
    delay();
  }

  std::cout << repeat << " instances of 2*gettimeofday()" << std::endl;
  auto start = std::chrono::system_clock::now();
  for (size_t i = 0; i < repeat * 2; i++) {
    struct timeval time;
    gettimeofday(&time, NULL);
  }
  std::cout << "Elapsed time: " << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count()
            << std::endl;
  std::cout << "Elapsed time per call: "
            << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count() / repeat << std::endl;

  std::cout << repeat << " instances of 2*chrono::system_clock::now()" << std::endl;
  start = std::chrono::system_clock::now();
  auto result = std::chrono::system_clock::now();
  for (size_t i = 0; i < repeat; i++) {
    result += std::chrono::system_clock::now() - std::chrono::system_clock::now();
  }
  std::cout << "Elapsed time: " << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count()
            << std::endl;
  std::cout << "Elapsed time per call: "
            << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count() / repeat << std::endl;

#ifdef MOLPRO_PROFILER_MPI
  MPI_Finalize();
#endif
}