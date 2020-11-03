#include "ProfilerC.h"
#include "molpro/Profiler.h"
#include "molpro/Profiler/Tree/report.h"

#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <list>
#include <sstream>
using molpro::profiler::tree::Profiler;
using molpro::profiler::tree::SortBy;
namespace {
struct ProfilerInfo {
  ProfilerInfo(std::shared_ptr<Profiler> p) : prof{std::move(p)} {}
  ProfilerInfo(std::shared_ptr<Profiler> p, SortBy s) : prof{std::move(p)}, sort_by{s} {}
  std::shared_ptr<Profiler> prof;
  SortBy sort_by = SortBy::wall;
#ifdef MOLPRO_PROFILER_MPI
  ProfilerInfo(std::shared_ptr<Profiler> p, SortBy s, MPI_Comm c) : prof{std::move(p)}, sort_by{s}, comm{c} {}
  MPI_Comm comm = MPI_COMM_NULL;
#endif
};
std::list<ProfilerInfo> global_prof;
void register_prof(ProfilerInfo p) {
  auto it = std::find_if(global_prof.cbegin(), global_prof.cend(),
                         [&p](const ProfilerInfo& el) { return el.prof.get() == p.prof.get(); });
  if (it == global_prof.end())
    global_prof.emplace_back(std::move(p));
}
ProfilerInfo& get_prof(Profiler* p) {
  auto it = std::find_if(global_prof.begin(), global_prof.end(), [&p](ProfilerInfo& el) { return el.prof.get() == p; });
  return *it;
}
void destroy_prof(Profiler* p) {
  auto f = std::find_if(global_prof.cbegin(), global_prof.cend(),
                        [p](const ProfilerInfo& el) -> bool { return el.prof.get() == p; });
  if (f != global_prof.end())
    global_prof.erase(f);
}
std::array<SortBy, 5> sort_orders = {SortBy::wall, SortBy::cpu, SortBy::calls, SortBy::operations, SortBy::wall};
} // namespace

// C binding
extern "C" {
#ifdef MOLPRO_PROFILER_MPI
void* profilerNewMPIA(char* name, int comm, int cpu) {
  auto p = Profiler::single(std::string(name), true, cpu != 0);
  register_prof({p, SortBy::wall});
  return p.get();
}
void* profilerNewMPIB(char* name, int sort, int level, int comm, int cpu) {
  auto sortBy = sort_orders[sort];
  auto p = Profiler::single(std::string(name), true, cpu != 0);
  if (level != -1)
    p->set_max_depth(level);
  register_prof({p, sortBy, MPI_Comm_f2c(comm)});
  return p.get();
}
#endif
void* profilerNewSerialA(char* name, int cpu) {
  auto p = Profiler::single(std::string(name), true, cpu != 0);
  register_prof({p});
  return p.get();
}
void* profilerNewSerialB(char* name, int sort, int level, int cpu) {
  auto sortBy = sort_orders[sort];
  auto p = Profiler::single(std::string(name), true, cpu != 0);
  if (level == -1)
    p->set_max_depth(level);
  register_prof({p, sortBy});
  return p.get();
}
void profilerDestroy(void* profiler) { destroy_prof(static_cast<Profiler*>(profiler)); }
void profilerReset(void* profiler, char* name) {
  auto obj = static_cast<Profiler*>(profiler);
  obj->reset(std::string(name));
}
void profilerActive(void* profiler, int level, int stopPrint) {
  auto obj = static_cast<Profiler*>(profiler);
  obj->set_max_depth(level);
}
void profilerStart(void* profiler, char* name) {
  auto obj = static_cast<Profiler*>(profiler);
  obj->start(std::string(name));
}
void profilerStop(void* profiler, char* name, long operations) {
  auto obj = static_cast<Profiler*>(profiler);
  obj->stop();
  *obj += operations;
}
char* profilerStr(void* profiler, int verbosity, int cumulative, int precision) {
  auto obj = static_cast<Profiler*>(profiler);
  auto prof_info = get_prof(obj);
  std::stringstream out;
#ifdef MOLPRO_PROFILER_MPI
  if (prof_info.comm != MPI_COMM_NULL)
    molpro::profiler::tree::report(*obj, out, prof_info.comm, cumulative, prof_info.sort_by);
  else
    molpro::profiler::tree::report(*obj, out, cumulative, prof_info.sort_by);
#else
  molpro::profiler::tree::report(*obj, out, cumulative, prof_info.sort_by);
#endif
  std::string res = out.str();
  char* result = (char*)malloc(res.size() + 1);
  strcpy(result, res.c_str());
  return result;
}
void profilerStrSubroutine(void* profiler, char* result, int maxResult, int verbosity, int cumulative, int precision) {
  strncpy(result, profilerStr(profiler, verbosity, cumulative, precision), maxResult - 1);
}
}
