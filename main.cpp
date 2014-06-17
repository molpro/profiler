#include "Profiler.h"
#include "ProfilerC.h"
#include <iostream>
#include <cmath>
int main(int argc, char *argv[])
{
  const size_t repeat=20000000;
  {
    Profiler profiler("C++");
    profiler.start("sqrt");
    double a=(double)0;
    for (int i=0; i<repeat; i++) a*=std::sqrt(a+i)/std::sqrt(a+i+1);
    profiler.stop("sqrt",2*repeat);
    profiler.start("exp");
    for (int i=0; i<repeat; i++) a*=std::exp(a+(double)1/i)/std::exp(a+(double)1/i+1);
    profiler.stop("exp",2*repeat);
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
    std::cout << profilerStr(profilerC) << std::endl;
  }

}
