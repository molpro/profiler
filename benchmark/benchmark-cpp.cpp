#include <chrono>
#include <memory>
#include <molpro/Profiler.h>
#include <molpro/Profiler/Tree/Profiler.h>
#include <molpro/Profiler/Tree/Reporter.h>
#include <sys/time.h>
#ifdef MOLPRO_PROFILER_MPI
#include <mpi.h>
#endif
struct ElapsedTime {
  using time_point = decltype(std::chrono::system_clock::now());
  explicit ElapsedTime(size_t n) : n_operations(n) { start = std::chrono::system_clock::now(); }
  ~ElapsedTime() {
    std::cout << "Elapsed time: " << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count()
              << std::endl;
    std::cout << "Elapsed time per call: "
              << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count() / n_operations
              << std::endl;
    std::cout << std::endl;
  }
  size_t n_operations;
  time_point start;
};
int main(int argc, char* argv[]) {
#ifdef MOLPRO_PROFILER_MPI
  MPI_Init(&argc, &argv);
#endif
  size_t repeat = 1000000;
  std::cout << repeat << " instances of Profiler::push()" << std::endl;
  if (argc > 1)
    repeat = std::stoi(argv[1]);
  {
    molpro::Profiler p{"test: depth = 1"};
    auto et = ElapsedTime(repeat);
    for (auto i = 0; i < repeat; i++) {
      p.push("test");
    }
    std::cout << p << std::endl;
  }
  {
    auto tp = molpro::profiler::tree::Profiler{"test tree profiler: depth = 1"};
    auto et = ElapsedTime(repeat);
    for (auto i = 0; i < repeat; i++) {
      tp.start("test");
      tp.stop();
    }
    tp.stop_all();
    report(tp);
  }
  {
    molpro::Profiler p{"test: depth = 5"};
    auto et = ElapsedTime(repeat);
    for (auto i = 0; i < repeat / 10000; i++) {
      auto p1 = p.push("test1");
      for (auto ii = 0; ii < 10; ii++) {
        auto p2 = p.push("test2");
        for (auto iii = 0; iii < 10; iii++) {
          auto p3 = p.push("test3");
          for (auto iiii = 0; iiii < 10; iiii++) {
            auto p4 = p.push("test4");
            for (auto iiiii = 0; iiiii < 10; iiiii++) {
              auto p5 = p.push("test5");
            }
          }
        }
      }
    }
    std::cout << p << std::endl;
  }
  {
    auto tp = molpro::profiler::tree::Profiler{"test tree profiler: depth = 1"};
    auto et = ElapsedTime(repeat);
    for (auto i = 0; i < repeat / 10000; i++) {
      tp.start("test1");
      for (auto ii = 0; ii < 10; ii++) {
        tp.start("test2");
        for (auto iii = 0; iii < 10; iii++) {
          tp.start("test3");
          for (auto iiii = 0; iiii < 10; iiii++) {
            tp.start("test4");
            for (auto iiiii = 0; iiiii < 10; iiiii++) {
              tp.start("test5");
              tp.stop();
            }
            tp.stop();
          }
          tp.stop();
        }
        tp.stop();
      }
      tp.stop();
    }
    tp.stop_all();
    report(tp);
  }

  std::cout << repeat << " instances of 2*gettimeofday()" << std::endl;
  auto start = std::chrono::system_clock::now();
  auto wall = (double)0;
  for (auto i = 0; i < repeat * 2; i++) {
    struct timeval time;
    if (!gettimeofday(&time, NULL))
      wall = (double)time.tv_sec + (double)time.tv_usec * .000001;
  }
  std::cout << "Elapsed time: " << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count()
            << std::endl;
  std::cout << "Elapsed time per call: "
            << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count() / repeat << std::endl;

  std::cout << repeat << " instances of 2*chrono::system_clock::now()" << std::endl;
  start = std::chrono::system_clock::now();
  auto result = std::chrono::system_clock::now();
  for (auto i = 0; i < repeat; i++) {
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