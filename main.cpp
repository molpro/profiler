#include "Profiler.h"
#include <iostream>
#include <cmath>
#include <time.h>
#include <sys/time.h>
int main(int argc, char *argv[])
{
  const size_t repeat=20000000, repeatr=1000000;
  {
    Profiler profiler("C++");

    profiler.start("sqrt");
    double a=(double)0;
    for (int i=0; i<repeat; i++) a*=std::sqrt(a+i)/std::sqrt(a+i+1);
    profiler.stop("sqrt",2*repeat);

    profiler.start("exp");
    for (int i=0; i<repeat; i++) a*=std::exp(a+(double)1/i)/std::exp(a+(double)1/i+1);
    profiler.stop("exp",2*repeat);

    profiler.start("getResources"); for (int i=0; i<repeatr; i++) struct Profiler::resources r = profiler.getResources(); profiler.stop("getResources",2*repeatr);

    for (int i=0; i<repeatr ; i++){ profiler.start("profiler"); profiler.stop("profiler",1); }

    profiler.start("gettimeofday"); for (int i=0; i<repeatr ; i++){ struct timeval time; gettimeofday(&time,NULL); } profiler.stop("gettimeofday",repeatr);

    std::cout << profiler << std::endl;
  }

  {
    void* profilerC=profilerNew((char*)"C");
    double a=(double)0;
    profilerStart(profilerC,(char*)"sqrt");
    for (int i=0; i<repeat; i++) a*=std::sqrt(a+i)/std::sqrt(a+i+1);
    profilerStop(profilerC,(char*)"sqrt",2*repeat);
    profilerStart(profilerC,(char*)"exp");
    for (int i=0; i<repeat; i++) a*=std::exp(a+(double)1/i)/std::exp(a+(double)1/i+1);
    profilerStop(profilerC,(char*)"exp",2*repeat);
    for (int i=0; i<1000000; i++) {
      profilerStart(profilerC,(char*)"profiler");
      profilerStop(profilerC,(char*)"profiler",1);
    }
    std::cout << profilerStr(profilerC,0,0,3) << std::endl;
  }

}
