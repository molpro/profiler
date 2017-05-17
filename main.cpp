#include "Profiler.h"
#include <iostream>
#include <cmath>
#include <time.h>
#include <sys/time.h>
#ifdef MPI2
#include "mpi.h"
#endif
int main(int argc, char *argv[])
{
  const size_t repeat=20000000, repeatr=1000000, repeats=10000000;
  {
#ifdef MPI2
    MPI_Init(&argc,&argv);
#endif
    Profiler profiler("C++",Profiler::name);

    profiler.start("sqrt");
    double a=(double)0;
    for (size_t i=0; i<repeat; i++) a*=std::sqrt(a+i)/std::sqrt(a+i+1);
    profiler.stop("sqrt",2*repeat);

    profiler.start("exp");
    for (size_t i=0; i<repeat; i++) a*=std::exp(a+(double)1/i)/std::exp(a+(double)1/i+1);
    profiler.stop("exp",2*repeat);

    profiler.start("getResources"); for (size_t i=0; i<repeatr; i++) struct Profiler::resources r = profiler.getResources(); profiler.stop("getResources",2*repeatr);

    for (size_t i=0; i<repeatr ; i++){ profiler.start("profiler"); profiler.stop("profiler",1); }

    profiler.start("gettimeofday"); for (size_t i=0; i<repeats ; i++){ struct timeval time; gettimeofday(&time,NULL); } profiler.stop("gettimeofday",repeats);

    {ProfilerPush s(profiler,"gettimeofday-Stack-1"); for (size_t i=0; i<repeats ; i++){ struct timeval time; gettimeofday(&time,NULL); } s+=repeats; }
    {ProfilerPush s(profiler,"gettimeofday-Stack-2"); for (size_t i=0; i<repeats ; i++){ struct timeval time; gettimeofday(&time,NULL); ++s; } }

    std::cout << profiler << std::endl;
  }

  {
    void* profilerC=profilerNew((char*)"C");
    double a=(double)0;
    profilerStart(profilerC,(char*)"sqrt");
    for (size_t i=0; i<repeat; i++) a*=std::sqrt(a+i)/std::sqrt(a+i+1);
    profilerStop(profilerC,(char*)"sqrt",2*repeat);
    profilerStart(profilerC,(char*)"exp");
    for (size_t i=0; i<repeat; i++) a*=std::exp(a+(double)1/i)/std::exp(a+(double)1/i+1);
    profilerStop(profilerC,(char*)"exp",2*repeat);
    for (size_t i=0; i<1000000; i++) {
      profilerStart(profilerC,(char*)"profiler");
      profilerStop(profilerC,(char*)"profiler",1);
    }
    std::cout << profilerStr(profilerC,0,0,3) << std::endl;
  }
#ifdef MPI2
  MPI_Finalize();
#endif

}
