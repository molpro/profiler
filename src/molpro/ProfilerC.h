#ifndef MOLPRO_PROFILERC_H
#define MOLPRO_PROFILERC_H

#include <cstdint>
#ifdef __cplusplus
extern "C" {
#endif
#ifdef MOLPRO_PROFILER_MPI
void* profilerNewMPIA(char* name, int64_t comm, int cpu);
void* profilerNewMPIB(char* name, int sort, int level, int64_t comm, int cpu);
#endif
void* profilerNewSerialB(char* name, int sort, int level, int cpu);
void* profilerNewSerialA(char* name, int cpu);
void profilerDestroy(void* profiler);
void profilerReset(void* profiler, char* name);
void profilerActive(void* profiler, int level, int stopPrint);
void profilerStart(void* profiler, char* name);
void profilerDeclare(void* profiler, char* name);
void profilerStop(void* profiler, char* name, long operations = 0);
char* profilerStr(void* profiler, int verbosity, int cumulative, int precision);
void profilerStrSubroutine(void* profiler, char* result, int maxResult, int verbosity, int cumulative, int precision);
void profilerDotgraph(void* profiler, const char* path, double threshold, int cumulative);
#ifdef __cplusplus
}
#endif

#endif // MOLPRO_PROFILERC_H
