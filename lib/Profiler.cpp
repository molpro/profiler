#include "ProfilerSingle.h"

// C binding
extern "C" {
#include <stdlib.h>
#include <string.h>
#ifdef PROFILER_MPI
 void* profilerNewComm(char* name, int comm) { return ProfilerSingle::create(std::string(name),MPI_Comm_f2c(comm)).get(); }
#endif
void* profilerNew(char* name) { return ProfilerSingle::create(std::string(name)).get(); }
void profilerDelete(char* name) { ProfilerSingle::destroy(std::string(name)); }
void profilerReset(void* profiler, char* name) { Profiler* obj=(Profiler*)profiler; obj->reset(std::string(name)); }
void profilerActive(void* profiler, int level, int stopPrint) { Profiler* obj=(Profiler*)profiler; obj->active(level,stopPrint); }
void profilerStart(void* profiler, char* name) { Profiler* obj=(Profiler*)profiler; obj->start(std::string(name)); }
void profilerStop(void* profiler, char* name, long operations) { Profiler* obj=(Profiler*)profiler; obj->stop(std::string(name),operations); }
char* profilerStr(void* profiler, int verbosity, int cumulative, int precision) { Profiler* obj=(Profiler*)profiler; std::string res = obj->str(verbosity,bool(cumulative), precision); char* result = (char*)malloc(res.size()+1); strcpy(result, res.c_str()); return result; }
void profilerStrSubroutine(void*profiler, char* result, int maxResult, int verbosity, int cumulative, int precision) { strncpy(result, profilerStr(profiler, verbosity, cumulative, precision),maxResult-1);}
}
