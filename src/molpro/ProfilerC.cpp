#include "ProfilerC.h"
#include "molpro/ProfilerSingle.h"

// C binding
extern "C" {
#include <cstdlib>
#include <cstring>
#ifdef MOLPRO_PROFILER_MPI
void* profilerNewMPIA(char* name, int comm) {
  return molpro::ProfilerSingle::create(std::string(name), molpro::Profiler::wall, INT_MAX, MPI_Comm_f2c(comm)).get();
}
void* profilerNewMPIB(char* name, int sort, int level, int comm) {
  molpro::Profiler::sortMethod sortBy;
  switch (sort) {
  case 1:
    sortBy = molpro::Profiler::wall;
    break;
  case 2:
    sortBy = molpro::Profiler::cpu;
    break;
  case 3:
    sortBy = molpro::Profiler::calls;
    break;
  case 4:
    sortBy = molpro::Profiler::operations;
    break;
  default:
    sortBy = molpro::Profiler::wall;
  }
  if (level == -1)
    level = INT_MAX;
  return molpro::ProfilerSingle::create(std::string(name), sortBy, level, MPI_Comm_f2c(comm)).get();
}
#endif
void* profilerNewSerialA(char* name) { return molpro::ProfilerSingle::create(std::string(name)).get(); }
void* profilerNewSerialB(char* name, int sort, int level) {
  molpro::Profiler::sortMethod sortBy;
  switch (sort) {
  case 1:
    sortBy = molpro::Profiler::wall;
    break;
  case 2:
    sortBy = molpro::Profiler::cpu;
    break;
  case 3:
    sortBy = molpro::Profiler::calls;
    break;
  case 4:
    sortBy = molpro::Profiler::operations;
    break;
  default:
    sortBy = molpro::Profiler::wall;
  }
  if (level == -1)
    level = INT_MAX;
  return molpro::ProfilerSingle::create(std::string(name), sortBy, level).get();
}
void profilerDelete(char* name) { molpro::ProfilerSingle::destroy(std::string(name)); }
void profilerReset(void* profiler, char* name) {
  molpro::Profiler* obj = (molpro::Profiler*)profiler;
  obj->reset(std::string(name));
}
void profilerActive(void* profiler, int level, int stopPrint) {
  molpro::Profiler* obj = (molpro::Profiler*)profiler;
  obj->active(level, stopPrint);
}
void profilerStart(void* profiler, char* name) {
  molpro::Profiler* obj = (molpro::Profiler*)profiler;
  obj->start(std::string(name));
}
void profilerStop(void* profiler, char* name, long operations) {
  molpro::Profiler* obj = (molpro::Profiler*)profiler;
  obj->stop(std::string(name), operations);
}
char* profilerStr(void* profiler, int verbosity, int cumulative, int precision) {
  molpro::Profiler* obj = (molpro::Profiler*)profiler;
  std::string res = obj->str(verbosity, bool(cumulative), precision);
  char* result = (char*)malloc(res.size() + 1);
  strcpy(result, res.c_str());
  return result;
}
void profilerStrSubroutine(void* profiler, char* result, int maxResult, int verbosity, int cumulative, int precision) {
  strncpy(result, profilerStr(profiler, verbosity, cumulative, precision), maxResult - 1);
}
}
