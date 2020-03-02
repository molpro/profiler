#ifndef PROFILER_PROFILER_H
#define PROFILER_PROFILER_H
#ifdef PROFILER_MPI
#include "ProfilerMPI.h"
#else
#include "ProfilerSerial.h"
#endif

#ifdef PROFILER_MPI
using Profiler = ProfilerMPI;
#else
using Profiler = ProfilerSerial;
#endif

#ifdef __cplusplus
extern "C" {
#endif
void* profilerNew(char* name);
void profilerReset(void* profiler, char* name);
void profilerActive(void* profiler, int level, int stopPrint);
void profilerStart(void* profiler, char* name);
void profilerDeclare(void* profiler, char* name);
void profilerStop(void* profiler, char* name, long operations=0);
char* profilerStr(void* profiler, int verbosity, int cumulative, int precision);
void profilerStrSubroutine(void*profiler, char* result, int maxResult, int verbosity, int cumulative, int precision);
#ifdef __cplusplus
}
#endif

#endif //PROFILER_PROFILER_H
