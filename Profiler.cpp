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

Profiler::Profiler()
{
}
Profiler::Profiler(std::string name, const int level)
{
  reset(name);
  active(level);
}

void Profiler::reset(const std::string name)
{
  Name=name;
  stopall();
  results.clear();
  active();
  start("* Other");
}

void Profiler::active(const int level)
{
    activeLevel=level;
}

void Profiler::start(const std::string name)
{
  if (resourcesStack.size()>=activeLevel) return;
  struct resources now=getResources();
  if (! resourcesStack.empty())
    resourcesStack.top()+=now;
  struct resources minusNow; minusNow.cpu=-now.cpu; minusNow.wall=-now.wall; minusNow.name=name; minusNow.operations=0; minusNow.stack = -now.stack;
  resourcesStack.push(minusNow);
#ifdef MEMORY_H
  memory_reset_maximum_stack(-1);
#endif
}

#include <assert.h>
void Profiler::stop(const std::string name, long operations)
{
  if (resourcesStack.size()>activeLevel) return;
  assert(name=="" || name == resourcesStack.top().name);
  struct resources now=getResources();now.operations=operations;
  struct resources* tt = &resourcesStack.top();
  (*tt)+=now;
  results[tt->name] += resourcesStack.top();
  results[tt->name].calls++;
#ifdef MEMORY_H
  memory_reset_maximum_stack(tt->stack);
#endif
  resourcesStack.pop();
  if (! resourcesStack.empty()) resourcesStack.top()-=now;
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
std::string Profiler::str(const int verbosity, const int precision)
{
  if (verbosity<0) return "";
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
  typedef std::pair<std::string,Profiler::resources> data_t;
  std::priority_queue<data_t, std::deque<data_t>, compareResources<data_t>  > q(localResults.begin(),localResults.end());
  std::stringstream ss;
  size_t maxWidth=0;
  long maxOperations=0;
  Profiler::resources totalResources=getResources();totalResources-=totalResources;
  for (resultMap::const_iterator s=localResults.begin(); s!=localResults.end(); ++s) {
      if ((*s).second.operations > maxOperations) maxOperations=(*s).second.operations;
      if ((*s).first.size() > maxWidth) maxWidth=(*s).first.size();
      //std::cout << "totalResources contributor name=" << (*s).second.name <<", cpu="<< (*s).second.cpu <<", stack="<< (*s).second.stack << std::endl;
      totalResources += (*s).second;
  }
  totalResources.calls=1;
  q.push(data_t("* TOTAL",totalResources));
  ss << "Profiler "<<Name<<std::endl;
  std::vector<std::string> prefixes;
  prefixes.push_back(""); prefixes.push_back("k"); prefixes.push_back("M"); prefixes.push_back("G");
  prefixes.push_back("T"); prefixes.push_back("P"); prefixes.push_back("E"); prefixes.push_back("Z"); prefixes.push_back("Y");
  while (! q.empty()) {
    ss.precision(precision);
    ss <<std::right <<std::setw(maxWidth) << q.top().first <<": calls="<<q.top().second.calls<<", cpu="<<std::fixed<<q.top().second.cpu<<", wall="<<q.top().second.wall;
    double ops=q.top().second.operations;
    double wall=q.top().second.wall;
    if (ops>(double)0 && wall>(double)0) {
      ops /= wall;
      int shifter = ops > 1 ? (int)(log10(ops)/3) : 0 ; shifter = shifter >= (int) prefixes.size() ? (int) prefixes.size()-1 : shifter;  ops *= pow((double)10, -shifter*3);
      ss<<", "<<ops<<" "<<prefixes[shifter]<<"op/s";
    }
    size_t stack=q.top().second.stack;
    if (stack > (size_t)0) {
      ss<<", stack="<<stack;
    }
      ss <<std::endl;
    q.pop();
  }
  return ss.str();
}

std::ostream& operator<<(std::ostream& os, Profiler & obj)
{
  return os << obj.str();
}

#include <time.h>
#include <sys/time.h>
struct Profiler::resources Profiler::getResources()
{
  struct Profiler::resources result;
  result.operations=0;
  result.cpu=(double)clock()/CLOCKS_PER_SEC;
  struct timeval time;
  result.wall=(double)0;
  if (!gettimeofday(&time,NULL))
    result.wall = (double)time.tv_sec + (double)time.tv_usec * .000001;
#ifdef MEMORY_H
  result.stack=(size_t) memory_used('S',(size_t)1);
#else
  result.stack=0;
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
char* profilerStr(void* profiler) { Profiler* obj=(Profiler*)profiler; char* result = (char*)malloc(obj->str().size()+1); strcpy(result, obj->str().c_str()); return result; }
  void profilerStrSubroutine(void*profiler, char* result, int maxResult) { strncpy(result, profilerStr(profiler),maxResult-1);}
}
