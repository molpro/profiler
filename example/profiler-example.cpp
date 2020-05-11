#include "Profiler.h"
#include "ProfilerC.h"
#include <iostream>
#include <cmath>
#include <ctime>
#include <sys/time.h>
#ifdef HAVE_MPI_H
#include "mpi.h"
#endif
int main(int argc, char *argv[])
{
  const size_t repeat=20000000, repeatr=1000000, repeats=10000000;
  {
#ifdef HAVE_MPI_H
    MPI_Init(&argc,&argv);
#endif
    molpro::Profiler profiler("C++", molpro::Profiler::name);

    profiler.start("sqrt");
    auto a=(double)0;
    for (size_t i=0; i<repeat; i++) a*=std::sqrt(a+i)/std::sqrt(a+i+1);
    profiler.stop("sqrt",2*repeat);

    profiler.start("exp");
    for (size_t i=0; i<repeat; i++) a*=std::exp(a+(double)1/i)/std::exp(a+(double)1/i+1);
    profiler.stop("exp",2*repeat);

    profiler.start("getResources"); for (size_t i=0; i<repeatr; i++) struct molpro::Profiler::resources r = profiler.getResources(); profiler.stop("getResources",2*repeatr);

    for (size_t i=0; i<repeatr ; i++){ profiler.start("profiler"); profiler.stop("profiler",1); }

    profiler.start("gettimeofday"); for (size_t i=0; i<repeats ; i++){ struct timeval time{0}; gettimeofday(&time,nullptr); } profiler.stop("gettimeofday",repeats);

    {auto envelope=profiler.push("Envelope");
    {molpro::Profiler::Push s(profiler,"gettimeofday-Stack-1"); for (size_t i=0; i<repeats ; i++){ struct timeval time{0}; gettimeofday(&time,nullptr); } s+=repeats; }
    {molpro::Profiler::Push s(profiler,"gettimeofday-Stack-2"); for (size_t i=0; i<repeats ; i++){ struct timeval time{0}; gettimeofday(&time,nullptr); ++s; } }
    {auto s = profiler.push("gettimeofday-Stack-1"); for (size_t i=0; i<repeats ; i++){ struct timeval time{0}; gettimeofday(&time,nullptr); } s+=repeats; }
    {auto s = profiler.push("gettimeofday-Stack-2"); for (size_t i=0; i<repeats ; i++){ struct timeval time{0}; gettimeofday(&time,nullptr); ++s; } }
    }

    std::cout << profiler << std::endl;
  }

  {
    void* profilerC=profilerNewSerialA((char*)"C");
    auto a=(double)0;
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
#ifdef HAVE_MPI_H
  MPI_Finalize();
#endif

}
