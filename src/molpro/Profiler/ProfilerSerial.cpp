#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>
#include <deque>
#include <iomanip>
#include <iostream>
#include <queue>
#include <string>
#include <sys/time.h>
#ifdef PROFILER_MEMORY
#include <molpro/memory.h>
#endif

#include "ProfilerSerial.h"

namespace molpro {
namespace profiler {
ProfilerSerial::ProfilerSerial(const std::string &name, sortMethod sortBy, const int level, ProfilerSerial::key_t key)
    : m_sortBy(sortBy) {
  reset(name);
  active(level);
}

void ProfilerSerial::reset(const std::string &name) {
  Name = name;
  stopall();
  results.clear();
  active();
  level = 0;
  start("TOP");
}

void ProfilerSerial::active(const int level, const int stopPrint) {
  activeLevel = level;
  stopPrint_ = stopPrint;
}

static char colon_replace = (char)30;

void ProfilerSerial::start(const std::string &name) {
  level++;
  if (level > activeLevel)
    return;
  assert(level == (int)resourcesStack.size() + 1);
  struct resources now = getResources();
  now.name = name;
  //  std::cout << "Profiler::start "<<name<<" wall="<<now.wall<<std::endl;
  for (auto c = now.name.begin(); c != now.name.end(); c++)
    if (*c == ':')
      *c = colon_replace;
  now.calls = 1;
  now.parent = this;
  if (!resourcesStack.empty())
    totalise(now, 0, 0);
#ifdef PROFILER_MEMORY
  // memory accounting:
  // the statistic for a segment is the maximum used, ie memory_used(1) minus the actual start memory, ie memory_used(0)
  // memory_reset_maximum_stack()  is used to reset the memory manager's notion of high water
  // memoryStack[01] are used to store the values from memory_used in start()
  // in start(),
  //  (a) remember current memory in memoryStack[01]
  // in totalise(),
  //  (a) set the my maximum memory to max(existing,maximum memory used - memoryStack0[me]);
  // in stop(),
  //  (a) reset maximum stack to memoryStack1[me]
  // in accumulate(), add max(children) to parent
  // if (! memoryStack0.empty()) {
  // resourcesStack.back().stack = std::max((int64_t)memory_used(1)-memoryStack0.back(),resourcesStack.back().stack);
  //}
  memoryStack0.push_back(memory_used(0));
  memoryStack1.push_back(memory_used(1));
#endif
  resourcesStack.push_back(now);
  startResources.push_back(now);
}

void ProfilerSerial::totalise(const struct resources now, const long operations, const int calls) {
  resources diff = now;
  diff -= resourcesStack.back();
  diff.name = resourcesStack.back().name;
  diff.operations = operations;
  diff.parent = this;
#ifdef PROFILER_MEMORY
  diff.stack = memory_used(1) - memoryStack0.back();
#endif
  std::string key;
  for (std::vector<resources>::const_reverse_iterator r = resourcesStack.rbegin(); r != resourcesStack.rend(); r++)
    key = r->name + ":" + key;
  key.erase(key.end() - 1, key.end());
  diff.name = key;
  results[key] += diff;
  results[key].calls += calls;
}

void ProfilerSerial::stop(const std::string &name, long operations) {
  level--;
  if (level > 0 && level >= activeLevel)
    return;
  assert(level == (int)resourcesStack.size() - 1);
#ifndef NDEBUG
  std::string nam(name);
  for (auto c = nam.begin(); c != nam.end(); c++)
    if (*c == ':')
      *c = colon_replace;
  assert(nam == "" || nam == resourcesStack.back().name);
#endif
  struct resources now = getResources();
  now.operations = operations;
  now.parent = this;
  //  std::cout << "Profiler::stop  "<<name<<" wall="<<now.wall<<std::endl;
  totalise(now, operations, 1);

  if (stopPrint_ > -1) {
    struct resources diff = now;
    diff -= startResources.back();
    diff.name = "";
    for (std::vector<resources>::const_reverse_iterator r = resourcesStack.rbegin(); r != resourcesStack.rend(); r++)
      diff.name = r->name + ":" + diff.name;
    diff.name.erase(diff.name.end() - 1, diff.name.end());
  }

#ifdef PROFILER_MEMORY
  memoryStack0.pop_back();
  memoryStack1.pop_back();
  if (!memoryStack1.empty())
    memory_reset_maximum_stack(memoryStack1.back());
#endif
  resourcesStack.pop_back();
  startResources.pop_back();
  if (!resourcesStack.empty()) {
    now.name = resourcesStack.back().name;
    resourcesStack.back() = now;
  }
}

void ProfilerSerial::stopall() {
  while (!resourcesStack.empty())
    stop();
}

ProfilerSerial::resultMap ProfilerSerial::totals() const {
  ProfilerSerial thiscopy =
      *this; // take a copy so that we can run stopall yet be const, and so that we can sum globally
  thiscopy.stopall();
  while (thiscopy.results.erase(""))
    ;
  for (auto &x : thiscopy.results)
    x.second.parent = this;
  thiscopy.accumulate(thiscopy.results);
  return thiscopy.results;
}

std::string ProfilerSerial::resources::str(const int width, const int verbosity, const bool cumulative,
                                           const int precision, const std::string defaultName) const {
  std::stringstream ss;
  std::vector<std::string> prefixes;
  prefixes.push_back("");
  prefixes.push_back("k");
  prefixes.push_back("M");
  prefixes.push_back("G");
  prefixes.push_back("T");
  prefixes.push_back("P");
  prefixes.push_back("E");
  prefixes.push_back("Z");
  prefixes.push_back("Y");
  const struct resources *r = this;
  std::string name = r->name;
  if (cumulative)
    r = (r->cumulative);
  if (name.find(":") == std::string::npos)
    if (defaultName != "")
      name = defaultName;
    else
      name = cumulative ? "All" : "(other)";
  else
    name.replace(0, name.find(":") + 1, "");
  if (cumulative) {
    auto pos = name.rfind(":", name.size() - 1);
    if (pos != std::string::npos) {
      size_t lev = 2;
      for (size_t k = 0; k < pos; k++)
        if (name[k] == ':')
          lev++;
      name = std::string(lev, '.') + name.substr(pos + 1);
    } else if (name != "All")
      name.insert(0, ".");
    for (auto c = name.begin(); c != name.end(); c++)
      if (*c == colon_replace)
        *c = ':';
  }
  size_t wid = width > 0 ? width : name.size();
  ss.precision(precision);
  ss << (cumulative ? std::left : std::right) << std::setw(wid) << name << ":";
  if (r->calls > 0)
    ss << " calls=" << r->calls << ",";
  ss << " cpu=" << std::fixed << r->cpu << ","
     << " wall=" << r->wall;
  double ops = r->operations;
  double wall = r->wall;
  if (ops > (double)0 && wall > (double)0) {
    ops /= wall;
    int shifter = ops > 1 ? (int)(log10(ops) / 3) : 0;
    shifter = shifter >= (int)prefixes.size() ? (int)prefixes.size() - 1 : shifter;
    ops *= pow((double)10, -shifter * 3);
    ss << ", " << ops << " " << prefixes[shifter] << "op/s";
  }
  size_t stack = r->stack;
  if (stack > (size_t)0) {
    ss << ", stack=" << stack;
  }
  return ss.str();
}

std::string ProfilerSerial::str(const int verbosity, const bool cumulative, const int precision) const {
  if (verbosity < 0)
    return "";
  resultMap localResults = totals();
  int n = localResults.size();
  size_t maxWidth = 0;
  if (cumulative)
    for (int i = 0; i < n; i++) {
      std::string key;
      resultMap::iterator s = localResults.begin();
      for (int j = 0; j < i; j++)
        s++;
      key = s->first;
      results[key].cumulative = s->second.cumulative;
      auto w = key.rfind(':');
      if (w != std::string::npos) {
        w = key.size() - w + std::count(key.begin(), key.begin() + w, ':');
        maxWidth = std::max(maxWidth, w);
      }
    }
  typedef std::pair<std::string, ProfilerSerial::resources> data_t;
  std::priority_queue<data_t, std::deque<data_t>, compareResources<data_t>> q(localResults.begin(), localResults.end());
  std::stringstream ss;
  if (!cumulative) {
    for (resultMap::const_iterator s = localResults.begin(); s != localResults.end(); ++s)
      if ((*s).first.size() > maxWidth)
        maxWidth = (*s).first.size();
    maxWidth -= localResults.begin()->first.size() + 1; // assumes the first node is the top level
  }
  if (maxWidth < 7)
    maxWidth = 7;
  ss << "Profiler \"" << Name << "\"";
  if (cumulative)
    ss << " (cumulative)";
  if (activeLevel < INT_MAX)
    ss << " to depth " << activeLevel;
  ss << std::endl;
  for (; !q.empty(); q.pop())
    ss << q.top().second.str(maxWidth, verbosity, cumulative, precision) << std::endl;
  return ss.str();
}

void ProfilerSerial::accumulate(resultMap &results) {
  for (resultMap::iterator r = results.begin(); r != results.end(); ++r)
    r->second.name = r->first;
  for (resultMap::iterator parent = results.begin(); parent != results.end(); ++parent) {
    parent->second.cumulative = new ProfilerSerial::resources;
    *parent->second.cumulative -= *parent->second.cumulative;
    // nb 'child' includes the parent itself
    for (resultMap::iterator child = results.begin(); child != results.end(); ++child) {
      if (parent->first == child->first || (parent->first.size() <= child->first.size() &&
                                            parent->first + ":" == child->first.substr(0, parent->first.size() + 1))) {
        *parent->second.cumulative += child->second;
        if (parent->first != child->first)
          parent->second.cumulative->stack =
              std::max(parent->second.cumulative->stack, parent->second.stack + child->second.stack);
        parent->second.cumulative->calls = parent->second.calls;
        parent->second.cumulative->parent = this;
      }
    }
  }
}

static int init = 1;
static double wallbase;

struct ProfilerSerial::resources ProfilerSerial::getResources() {
  struct ProfilerSerial::resources result;
  result.operations = 0;
  result.calls = 0;
  result.cpu = (double)clock() / CLOCKS_PER_SEC;
  struct timeval time;
  result.wall = (double)0;
  if (!gettimeofday(&time, NULL)) {
    result.wall = (double)time.tv_sec + (double)time.tv_usec * .000001;
    if (init)
      wallbase = result.wall;
    init = 0;
    result.wall -= wallbase;
  }
#ifdef PROFILER_MEMORY
  result.stack = (size_t)memory_used(1);
#else
  result.stack = 0;
  result.cumulative = NULL;
#endif
  return result;
}

/*!
 * \brief Profiler::resources::operator += add another object to this one
 * \param w2 object to add
 * \return a copy of the object
 */
struct ProfilerSerial::resources &ProfilerSerial::resources::operator+=(const struct ProfilerSerial::resources &w2) {
  cpu += w2.cpu;
  wall += w2.wall;
  operations += w2.operations;
  if (w2.stack > stack)
    stack = w2.stack; // choose maximum stack of the two objects
  return *this;
}

struct ProfilerSerial::resources ProfilerSerial::resources::operator+(const struct ProfilerSerial::resources &w2) {
  struct ProfilerSerial::resources result = *this;
  result += w2;
  return result;
}

struct ProfilerSerial::resources &ProfilerSerial::resources::operator-=(const struct ProfilerSerial::resources &w2) {
  cpu -= w2.cpu;
  wall -= w2.wall;
  operations -= w2.operations;
  stack -= w2.stack;
  return *this;
}

struct ProfilerSerial::resources ProfilerSerial::resources::operator-(const struct ProfilerSerial::resources &w2) {
  struct ProfilerSerial::resources result = *this;
  result -= w2;
  return result;
}

ProfilerSerial::Push ProfilerSerial::push(const std::string &name) { return Push(*this, name); }

} // namespace profiler
} // namespace molpro

std::ostream &operator<<(std::ostream &os, molpro::profiler::ProfilerSerial &obj) {
  return os << obj.str() << std::endl;
}
