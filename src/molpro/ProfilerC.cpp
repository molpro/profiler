#include "ProfilerC.h"
#include "molpro/ProfilerSingle.h"

#include <algorithm>
#include <list>
namespace {
std::list<std::shared_ptr<molpro::Profiler>> global_prof;
void register_prof(const std::shared_ptr<molpro::Profiler>& p) {
  if (std::find(global_prof.begin(), global_prof.end(), p) == global_prof.end())
    global_prof.push_back(p);
}
void destroy_prof(molpro::Profiler* p) {
  auto f = std::find_if(global_prof.begin(), global_prof.end(),
                        [p](const std::shared_ptr<molpro::Profiler>& el) -> bool { return el.get() == p; });
  if (f != global_prof.end())
    global_prof.erase(f);
}
} // namespace

// C binding
extern "C" {
#include <cstdlib>
#include <cstring>
#ifdef MOLPRO_PROFILER_MPI
void* profilerNewMPIA(char* name, int comm, int cpu) {
  auto p =
      molpro::ProfilerSingle::create(std::string(name), molpro::Profiler::wall, INT_MAX, MPI_Comm_f2c(comm), cpu != 0);
  register_prof(p);
  return p.get();
}
void* profilerNewMPIB(char* name, int sort, int level, int comm, int cpu) {
  molpro::Profiler::sortMethod sortBy;
  switch (sort) {
    case 1:sortBy = molpro::Profiler::wall;
      break;
    case 2:sortBy = molpro::Profiler::cpu;
      break;
    case 3:sortBy = molpro::Profiler::calls;
      break;
    case 4:sortBy = molpro::Profiler::operations;
      break;
    default:sortBy = molpro::Profiler::wall;
  }
  if (level == -1)
    level = INT_MAX;
  auto p = molpro::ProfilerSingle::create(std::string(name), sortBy, level, MPI_Comm_f2c(comm), cpu != 0);
  register_prof(p);
  return p.get();
}
#endif
void* profilerNewSerialA(char* name, int cpu) {
  auto p = molpro::ProfilerSingle::create(std::string(name),
                                          molpro::Profiler::wall,
                                          INT_MAX,
                                          PROFILER_DEFAULT_KEY,
                                          cpu != 0);
  register_prof(p);
  return p.get();
}
void* profilerNewSerialB(char* name, int sort, int level, int cpu) {
  molpro::Profiler::sortMethod sortBy;
  switch (sort) {
    case 1:sortBy = molpro::Profiler::wall;
      break;
    case 2:sortBy = molpro::Profiler::cpu;
      break;
    case 3:sortBy = molpro::Profiler::calls;
      break;
    case 4:sortBy = molpro::Profiler::operations;
      break;
    default:sortBy = molpro::Profiler::wall;
  }
  if (level == -1)
    level = INT_MAX;
  auto p = molpro::ProfilerSingle::create(std::string(name), sortBy, level, PROFILER_DEFAULT_KEY, cpu != 0);
  register_prof(p);
  return p.get();
}
void profilerDestroy(void* profiler) { destroy_prof(static_cast<molpro::Profiler*>(profiler)); }
void profilerReset(void* profiler, char* name) {
  molpro::Profiler* obj = (molpro::Profiler*) profiler;
  obj->reset(std::string(name));
}
void profilerActive(void* profiler, int level, int stopPrint) {
  molpro::Profiler* obj = (molpro::Profiler*) profiler;
  obj->active(level, stopPrint);
}
void profilerStart(void* profiler, char* name) {
  molpro::Profiler* obj = (molpro::Profiler*) profiler;
  obj->start(std::string(name));
}
void profilerStop(void* profiler, char* name, long operations) {
  molpro::Profiler* obj = (molpro::Profiler*) profiler;
  obj->stop(std::string(name), operations);
}
char* profilerStr(void* profiler, int verbosity, int cumulative, int precision) {
  molpro::Profiler* obj = (molpro::Profiler*) profiler;
  std::string res = obj->str(verbosity, bool(cumulative), precision);
  char* result = (char*) malloc(res.size() + 1);
  strcpy(result, res.c_str());
  return result;
}
void profilerStrSubroutine(void* profiler, char* result, int maxResult, int verbosity, int cumulative, int precision) {
  strncpy(result, profilerStr(profiler, verbosity, cumulative, precision), maxResult - 1);
}
}
