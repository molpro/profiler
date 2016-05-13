#include <algorithm>
#include <sstream>
#include <iostream>
#include <deque>
#include <queue>
#include <string>
#include <string.h>
#include <iomanip>
#include "Profiler.h"
#include "memory.h"

//#define DEBUG

Profiler::Profiler()
{
}
Profiler::Profiler(std::string name, const int level)
{
  reset(name);
  active(level);
}

const std::string Profiler::rootNode="!:!:!:TOP";
void Profiler::reset(const std::string name)
{
  Name=name;
  stopall();
  results.clear();
  active();
  level=0;
  start(rootNode);
}

void Profiler::active(const int level)
{
    activeLevel=level;
}

#include <assert.h>
void Profiler::start(const std::string name)
{
  level++;
#ifdef DEBUG
  std::cout << "start "<<name<<", stack size="<<resourcesStack.size()<<std::endl;
#endif
  if (level>activeLevel) return;
#ifdef DEBUG
  std::cout << "start continuing"<<level<<resourcesStack.size()<<std::endl;
#endif
  assert(level==resourcesStack.size()+1);
  struct resources now=getResources();now.name=name;
  if (! resourcesStack.empty())
    totalise(now,0,0);
  resourcesStack.push_back(now);
#ifdef DEBUG
    std::cout << "end of start "<<name<<", stack size="<<resourcesStack.size()<<" top name="<<resourcesStack.back().name<<std::endl;
#endif
#ifdef MEMORY_H
  memory_reset_maximum_stack(-1);
#endif
}

void Profiler::totalise(const struct resources now, const long operations, const int calls)
{
  resources diff=now;
  diff-=resourcesStack.back();
  diff.name=resourcesStack.back().name;
  diff.operations=operations;
  std::string key;
  for(std::vector<resources>::const_reverse_iterator r=resourcesStack.rbegin(); r!= resourcesStack.rend(); r++) key=r->name+":"+key; key.pop_back();
  diff.name=key;
  results[key] += diff;
  results[key].calls += calls;
#ifdef DEBUG
  std::cout << "totalise base.wall="<<resourcesStack.back().wall<<", now.wall="<<now.wall<<", wall increment="<<diff.wall<<", updated results["<<key<<"].wall="<<results[key].wall<<std::endl;
#endif
}

void Profiler::stop(const std::string name, long operations)
{
  level--;
#ifdef DEBUG
  std::cout << "stop "<<name<<", stack size="<<resourcesStack.size()<<level<<" top name="<<resourcesStack.back().name<<std::endl;
#endif
  if (level > 0 && level>=activeLevel) return;
#ifdef DEBUG
  std::cout << "stop continuing"<<std::endl;
#endif
  assert(level==resourcesStack.size()-1);
  assert(name=="" || name == resourcesStack.back().name);
  struct resources now=getResources();now.operations=operations;
  totalise(now,operations,1);
#ifdef MEMORY_H
  memory_reset_maximum_stack(resourcesStack.back().stack);
#endif
  resourcesStack.pop_back();
  if (! resourcesStack.empty()) {now.name=resourcesStack.back().name; resourcesStack.back()=now;}
#ifdef DEBUG
  if (! resourcesStack.empty())
    std::cout << "end of stop "<<name<<", stack size="<<resourcesStack.size()<<" top name="<<resourcesStack.back().name<<std::endl;
#endif
}

void Profiler::declare(const std::string name)
{
  if (results.count(name)==0) {
  struct resources tt; tt.cpu=0; tt.wall=0; tt.name=name; tt.operations=0; tt.calls=0;
  results[name] = tt;
  }
}

void Profiler::stopall()
{
  while (! resourcesStack.empty()) stop();
}

#include <cmath>
#ifdef GCI_PARALLEL
#define HAVE_PPIDD
#endif
#ifdef HAVE_PPIDD
extern "C" {
#include "ppidd_c.h"
}
#endif
#ifdef MOLPRO
#include "mpp/CxMpp.h"
#include "cic/ItfMpp.h"
itf::FMppInt interface(itf::FMppInt::MPP_NeedSharedFs|itf::FMppInt::MPP_GlobalDeclaration);
//extern "C" {
//int64_t get_iprocs_cxx_();
//}
#endif
Profiler::resultMap Profiler::totals()
{
  stopall();
  resultMap localResults=this->results; // local copy that we can sum globally
  while(localResults.erase(""));
  for (resultMap::iterator s=localResults.begin(); s!=localResults.end(); ++s) {
#ifdef GCI_PARALLEL
    int64_t type=1, len=1;
    char* opm=strdup("max");
    PPIDD_Gsum(&type,&((*s).second.wall),&len,opm);
    char* op=strdup("+");
    PPIDD_Gsum(&type,&((*s).second.cpu),&len,op);
    int64_t value=(int64_t)(*s).second.calls; type=0;
    PPIDD_Gsum(&type,&value,&len,op);
    (*s).second.calls=(int)value;
    value=(int64_t)(*s).second.operations; type=0;
    PPIDD_Gsum(&type,&value,&len,op);
    (*s).second.operations=(long)value;
    char* opm=strdup("max");
    int64_t stack=(fortint)(*s).second.stack; type=0;
    PPIDD_Gsum(&type,&stack,&len,opm);
    (*s).second.stack=(int64_t)stack;
#else
#ifdef MOLPRO
    // only '+' works in Molpro runtime
    //    interface.GlobalSum(&((*s).second.wall),(std::size_t)1,(uint)0,(const char*) "max");
    interface.GlobalSum(&((*s).second.cpu),(std::size_t)1);
    // Molpro interface presently only does doubles
    double value=(double)(*s).second.calls;
    interface.GlobalSum(&value,(std::size_t)1);
    (*s).second.calls=(int)value;
    value=(double)(*s).second.operations;
    interface.GlobalSum(&value,(std::size_t)1);
    (*s).second.operations=(long)value;
    // global max of (*s).second.stack
    value=(double)(*s).second.stack;
    interface.GlobalSum(&value,(std::size_t)1, (uint)0, "max");
    (*s).second.stack=(int64_t)value;
#endif
#endif
  }
  accumulate(localResults);
  return localResults;
}

std::string Profiler::str(const int verbosity, const bool cumulative, const int precision)
{
  if (verbosity<0) return "";
  resultMap localResults=totals();
  typedef std::pair<std::string,Profiler::resources> data_t;
  std::priority_queue<data_t, std::deque<data_t>, compareResources<data_t>  > q(localResults.begin(),localResults.end());
  std::stringstream ss;
  size_t maxWidth=0;
  for (resultMap::const_iterator s=localResults.begin(); s!=localResults.end(); ++s) {
      if ((*s).first.size() > maxWidth) maxWidth=(*s).first.size();
  }
  maxWidth-=rootNode.size()+1;
  ss << "Profiler "<<Name; if(cumulative) ss<<" (cumulative)"; if (activeLevel < INT_MAX) ss <<" to depth "<<activeLevel; ss <<std::endl;
//  ss << "Precision="<<precision<<std::endl;
  std::vector<std::string> prefixes;
  prefixes.push_back(""); prefixes.push_back("k"); prefixes.push_back("M"); prefixes.push_back("G");
  prefixes.push_back("T"); prefixes.push_back("P"); prefixes.push_back("E"); prefixes.push_back("Z"); prefixes.push_back("Y");
  while (! q.empty()) {
    Profiler::resources r=q.top().second;
    if (!r.name.empty() && cumulative) r=*(r.cumulative);
    ss.precision(precision);
    std::string name=q.top().first;
    name.replace(0,rootNode.size()+1,""); if (name == "") name = cumulative ? "All" : "(other)";
    ss <<std::right <<std::setw(maxWidth) << name <<": calls="<<r.calls<<", cpu="<<std::fixed<<r.cpu<<", wall="<<r.wall;
    double ops=r.operations;
    double wall=r.wall;
    if (ops>(double)0 && wall>(double)0) {
      ops /= wall;
      int shifter = ops > 1 ? (int)(log10(ops)/3) : 0 ; shifter = shifter >= (int) prefixes.size() ? (int) prefixes.size()-1 : shifter;  ops *= pow((double)10, -shifter*3);
      ss<<", "<<ops<<" "<<prefixes[shifter]<<"op/s";
    }
    size_t stack=r.stack;
    if (stack > (size_t)0) {
      ss<<", stack="<<stack;
    }
      ss <<std::endl;
    q.pop();
  }
  return ss.str();
}

void Profiler::accumulate(resultMap& results)
{
//  std::cout << "accumulate all"<<std::endl;
  for (resultMap::iterator r=results.begin(); r!=results.end(); ++r) r->second.name=r->first;
  for (resultMap::iterator r=results.begin(); r!=results.end(); ++r) {
    //std::cout << "accumulate "<<r->first<<" : "<<r->second.name<<std::endl;
//    accumulate(r->second, results);
      r->second.cumulative = new Profiler::resources;
      *r->second.cumulative-=*r->second.cumulative;
    for (resultMap::iterator s=results.begin(); s!=results.end(); ++s) {
      if (r->first.size() <= s->first.size() && r->first == s->first.substr(0,r->first.size())) {
        int64_t memo=r->second.cumulative->stack;
        *r->second.cumulative += s->second;
        r->second.cumulative->stack=std::max(memo,r->second.stack+s->second.stack);
        r->second.cumulative->calls = r->second.calls;
//        std::cout << "accumulate "<<r->first<<" with " <<s->second.name<<s->second.wall<<":"<<r->second.cumulative->wall<<std::endl;
      }
    }
  }
}

std::ostream& operator<<(std::ostream& os, Profiler & obj)
{
  return os << obj.str();
}

#include <time.h>
#include <sys/time.h>
static int init=1;
static double wallbase;
struct Profiler::resources Profiler::getResources()
{
  struct Profiler::resources result;
  result.operations=0;
  result.cpu=(double)clock()/CLOCKS_PER_SEC;
  struct timeval time;
  result.wall=(double)0;
  if (!gettimeofday(&time,NULL)) {
    result.wall = (double)time.tv_sec + (double)time.tv_usec * .000001;
    if (init) wallbase=result.wall;
    init=0;
    result.wall-=wallbase;
  }
#ifdef MEMORY_H
  result.stack=(size_t) memory_used('S',(size_t)1);
#else
  result.stack=0;
  result.cumulative=NULL;
#endif
  return result;
}

/*!
 * \brief Profiler::resources::operator += add another object to this one
 * \param w2 object to add
 * \return a copy of the object
 */
struct Profiler::resources& Profiler::resources::operator+=( const struct Profiler::resources &w2)
{
  cpu += w2.cpu;
  wall += w2.wall;
  operations += w2.operations;
  if (w2.stack > stack) stack = w2.stack; // choose maximum stack of the two objects
  return *this;
}

struct Profiler::resources Profiler::resources::operator+(const struct Profiler::resources &w2)
{
  struct Profiler::resources result=*this;
  result += w2;
  return result;
}

struct Profiler::resources& Profiler::resources::operator-=( const struct Profiler::resources &w2)
{
  cpu -= w2.cpu;
  wall -= w2.wall;
  operations -= w2.operations;
  stack -= w2.stack;
  return *this;
}

struct Profiler::resources Profiler::resources::operator-(const struct Profiler::resources &w2)
{
  struct Profiler::resources result=*this;
  result -= w2;
  return result;
}

// C binding
extern "C" {
#include <stdlib.h>
#include <string.h>
void* profilerNew(char* name) { return new Profiler(name); }
void profilerReset(void* profiler, char* name) { Profiler* obj=(Profiler*)profiler; obj->reset(std::string(name)); }
void profilerActive(void* profiler, int level) { Profiler* obj=(Profiler*)profiler; obj->active(level); }
void profilerStart(void* profiler, char* name) { Profiler* obj=(Profiler*)profiler; obj->start(std::string(name)); }
void profilerDeclare(void* profiler, char* name) { Profiler* obj=(Profiler*)profiler; obj->declare(std::string(name)); }
void profilerStop(void* profiler, char* name, long operations) { Profiler* obj=(Profiler*)profiler; obj->stop(std::string(name),operations); }
char* profilerStr(void* profiler, int verbosity, int cumulative, int precision) { Profiler* obj=(Profiler*)profiler; std::string res = obj->str(verbosity,bool(cumulative), precision); char* result = (char*)malloc(res.size()+1); strcpy(result, res.c_str()); return result; }
void profilerStrSubroutine(void*profiler, char* result, int maxResult, int verbosity, int cumulative, int precision) { strncpy(result, profilerStr(profiler, verbosity, cumulative, precision),maxResult-1);}
}
