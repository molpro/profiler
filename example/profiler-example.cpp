#include <cmath>
#include <ctime>
#include <iostream>
#include <molpro/Profiler.h>
#include <molpro/ProfilerC.h>
#include <sys/time.h>
#ifdef HAVE_MPI_H
#include "mpi.h"
#endif

using molpro::Profiler;
using molpro::profiler::SortBy;

int main(int argc, char* argv[]) {
  const size_t repeat = 20000000, repeatr = 1000000, repeats = 10000000;
  {
#ifdef HAVE_MPI_H
    MPI_Init(&argc, &argv);
#endif
    Profiler profiler("C++");

    profiler.start("sqrt");
    auto a = (double)0;
    for (size_t i = 0; i < repeat; i++)
      a *= std::sqrt(a + i) / std::sqrt(a + i + 1);
    profiler.stop() += 2 * repeat;

    profiler.start("exp");
    for (size_t i = 0; i < repeat; i++)
      a *= std::exp(a + (double)1 / i) / std::exp(a + (double)1 / i + 1);
    profiler.stop() += 2 * repeat;

    for (size_t i = 0; i < repeatr; i++) {
      profiler.start("profiler");
      profiler.stop() += 1;
    }

    profiler.start("gettimeofday");
    for (size_t i = 0; i < repeats; i++) {
      struct timeval time {
        0
      };
      gettimeofday(&time, nullptr);
    }
    profiler.stop() += repeats;

    {
      auto envelope = profiler.push("Envelope");
      {
        auto s = profiler.push("gettimeofday-Stack-1");
        for (size_t i = 0; i < repeats; i++) {
          struct timeval time {
            0
          };
          gettimeofday(&time, nullptr);
        }
        s += repeats;
      }
      {
        auto s = profiler.push("gettimeofday-Stack-2");
        for (size_t i = 0; i < repeats; i++) {
          struct timeval time {
            0
          };
          gettimeofday(&time, nullptr);
          ++s;
        }
      }
      {
        auto s = profiler.push("gettimeofday-Stack-1");
        for (size_t i = 0; i < repeats; i++) {
          struct timeval time {
            0
          };
          gettimeofday(&time, nullptr);
        }
        s += repeats;
      }
      {
        auto s = profiler.push("gettimeofday-Stack-2");
        for (size_t i = 0; i < repeats; i++) {
          struct timeval time {
            0
          };
          gettimeofday(&time, nullptr);
          ++s;
        }
      }
    }

    std::cout << profiler << std::endl;
    auto gv = std::string{argv[0]} + ".gv";
    profiler.dotgraph(gv);
    gv = std::string{argv[0]} + ".noncumulative.gv";
    profiler.dotgraph(gv, 0.01, false);
  }

  {
    void* profilerC = profilerNewSerialA((char*)"C", 0);
    auto a = (double)0;
    profilerStart(profilerC, (char*)"sqrt");
    for (size_t i = 0; i < repeat; i++)
      a *= std::sqrt(a + i) / std::sqrt(a + i + 1);
    profilerStop(profilerC, (char*)"sqrt", 2 * repeat);
    profilerStart(profilerC, (char*)"exp");
    for (size_t i = 0; i < repeat; i++)
      a *= std::exp(a + (double)1 / i) / std::exp(a + (double)1 / i + 1);
    profilerStop(profilerC, (char*)"exp", 2 * repeat);
    for (size_t i = 0; i < 1000000; i++) {
      profilerStart(profilerC, (char*)"profiler");
      profilerStop(profilerC, (char*)"profiler", 1);
    }
    std::cout << profilerStr(profilerC, 0, 0, 3) << std::endl;
    profilerDestroy(profilerC);
  }
#ifdef HAVE_MPI_H
  MPI_Finalize();
#endif
}
