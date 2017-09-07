#ifndef PROFILER_H
#define PROFILER_H
#ifdef __cplusplus
#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <climits>
#include <stdint.h>
// if you want to force MPI mode, uncomment the next line
//#define PROFILER_MPI
#ifdef MOLPRO
#include "common/molpro_config.h"
#endif
#if defined(GA_MPI) || defined(MPI2) || defined(GCI_MPI) || defined(GCI_PARALLEL) || defined(PPIDD)
#define PROFILER_MPI
#endif
#ifdef PROFILER_MPI
#include "mpi.h"
#endif

/*!
 * \brief The Profiler class: framework for timing code sections.
 *
 * Example of use:
 * \code
const size_t repeat=20000000, repeatr=1000000, repeats=10000000;
{
  Profiler profiler("A C++ profiling test",Profiler::name);

  // Conventional use with stop/start pairs
  profiler.start("sqrt");
  double a=(double)0;
  for (size_t i=0; i<repeat; i++) a*=std::sqrt(a+i)/std::sqrt(a+i+1);
  profiler.stop("sqrt",2*repeat);

  // Object approach controlled with scoping
  {Profiler::Push s(profiler,"gettimeofday-Stack-1"); for (size_t i=0; i<repeats ; i++){ struct timeval time; gettimeofday(&time,NULL); } s+=repeats; }
  {Profiler::Push s(profiler,"gettimeofday-Stack-2"); for (size_t i=0; i<repeats ; i++){ struct timeval time; gettimeofday(&time,NULL); ++s; } }

  std::cout << profiler << std::endl; // print the results
}
 * \endcode
 */
class Profiler
{
  Profiler();
public:
  /*!
   * \brief Sorting criteria for categories in report.
   */
  enum sortMethod { wall //!< Sort by real time.
                    , cpu//!< Sort by CPU time.
                    , name //!< Sort alphabetically by name.
                    , calls //!< Sort by number of calls.
                    , operations //!< Sort by number of operations.
                  };
  /*!
   * \brief Profiler construct a named instance.
   * \param name the title of this object.
   * \param sortBy Criterion for sorting printed result table.
   * \param level
   * A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
   * \param communicator The MPI communicator over which statistics should be aggregated.
   */
  Profiler(const std::string &name, sortMethod sortBy=wall, const int level=INT_MAX
#ifdef PROFILER_MPI
           , const MPI_Comm communicator=MPI_COMM_WORLD
#endif
           );
  /*!
   * \brief Reset the object.
   * \param name The title of this object.
   */
  void reset(const std::string& name);
  /*!
   * \brief Begin timing a code segment.
   * \param name Name of the code segment.
   */
  void start(const std::string& name);
  /*!
   * \brief Finish timing a code segment.
   * \param name If given, must match the argument of the previous start call.
   * \param operations If given, a count of the number of operations (or whatever you like) carried out.
   */
  void stop(const std::string& name="",long operations=0);
  /*!
   * \brief active set the maximum stack depth at which data collection is done.
   * \param level
   * A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
   * \param stopPrint if non-negative, \ref stop() prints the statistics since the corresponding \ref start()
   */
  void active(const int level=INT_MAX, const int stopPrint=-1);
  /*!
   * \brief Generate a printable representation of the object.
   * Must be called by all MPI processes collectively.
   * \param verbosity How much to print.
   * \param cumulative Whether to print cumulative (ie including all children) resources.
   * \param precision How many decimal places for seconds.
   * \return
   */
  std::string str(const int verbosity=0, const bool cumulative=false, const int precision=3) const;
public:
class Push;
public:
  /*!
   * \brief Push to a new level on the stack of a Profiler object.
   * \param name The name of the code segment to be profiled.
   * \return An object that when destroyed will call the corresponding Profiler::stop.
   */
  Push push(const std::string& name="");

 public:
  struct resources {double cpu; double wall; int calls; long operations; std::string name; int64_t stack;
                    struct resources * cumulative;
                    const Profiler *parent;
                    std::string str(const int width=0, const int verbosity=0, const bool cumulative=false, const int precision=3, const std::string defaultName="") const;
                    struct Profiler::resources& operator+=(const struct Profiler::resources &other);
                    struct Profiler::resources& operator-=(const struct Profiler::resources &other);
                    struct Profiler::resources operator+(const struct Profiler::resources &w2);
                    struct Profiler::resources operator-(const struct Profiler::resources &w2);
                    resources() {cpu=0;wall=0;calls=0;operations=0;stack=0;cumulative=nullptr;parent=nullptr;}
               };
  struct resources getResources();

  typedef std::map<std::string,struct Profiler::resources> resultMap;

  /*!
   * \brief Obtain a summary of the resources used for each category.
   * Must be called by all MPI processes collectively.
   * \return std::map of \ref resources
   */
  resultMap totals() const;

 private:
  void totalise(const struct resources now, const long operations, const int calls=1);
  template<class T> struct compareResources : std::binary_function<T,T,bool>
  { inline bool operator () (const T& _left, const T& _right)
    {
//      std::cout <<"compareResources left="<<_left.first<<", right="<<_right.first<<std::endl;
      auto lastcolon_left = _left.first.rfind(':');
      auto lastcolon_right = _right.first.rfind(':');
      if (lastcolon_left == std::string::npos) return false;
      if (lastcolon_right == std::string::npos) return true;
//      std::cout <<"compareResources left="<<_left.first.substr(0,lastcolon_left)<<", right="<<_right.first.substr(0,lastcolon_right)<<std::endl;
//      if (lastcolon_left > lastcolon_right && _left.first.substr(0,lastcolon_right) == _right.first.substr(0,lastcolon_right)) {
      if (_left.first.size() > _right.first.size() && _left.first.substr(0,_right.first.size()) == _right.first) {
//          std::cout << "right is a parent of left"<<std::endl;
          return true;
        }
//      if (lastcolon_left < lastcolon_right && _left.first.substr(0,lastcolon_left) == _right.first.substr(0,lastcolon_left)) {
      if (_right.first.size() > _left.first.size() && _right.first.substr(0,_left.first.size()) == _left.first) {
//          std::cout << "left is a parent of right"<<std::endl;
          return false;
        }
//      auto l = _left.second;
//      auto r = _right.second;
      const Profiler& pl=*(_left.second.parent);
//      int depthl; for ( depthl=0; pl.results[depthl].first != _left.first; depthl++) ;
//      int depthr; for ( depthr=0; pl.results[depthr].first != _right.first; depthr++) ;
      auto depthl=std::count(_left.first.begin(),_left.first.end(),':');
      auto depthr=std::count(_right.first.begin(),_right.first.end(),':');
//      std::cout << " depthl="<<depthl<<", depthr="<<depthr<<std::endl;
//      std::cout <<l.parent->resultMap[_left.first]<<" = "<<_left.first<<std::endl;
      // find the common ancestor
      size_t o; for (o=0; o<std::max(_left.first.size(),_right.first.size()) && _left.first[o]==_right.first[o] ;o++) ;
      while (_left.first[o]!=':')o--;
//      std::cout << "common ancestry "<<_left.first.substr(0,o)<<std::endl;
      auto oL=_left.first.find(':',o+1); if (oL==std::string::npos) oL=_left.first.size();
      auto oR=_right.first.find(':',o+1); if (oR==std::string::npos) oR=_right.first.size();
//      std::cout << "left comparator "<<_left.first.substr(0,oL)<<std::endl;
//      std::cout << "right comparator "<<_right.first.substr(0,oR)<<std::endl;
//      for (auto x = pl.results.begin(); x != pl.results.end(); x++)
//      std::cout << "_right.second.parent"<<_right.second.parent<<std::endl;
//      std::cout << "_left.second.parent"<<_left.second.parent<<std::endl;
//      if (l.parent==nullptr) throw std::runtime_error("parent node not found");
//      for (auto x = pl.results.begin(); x != pl.results.end(); x++) std::cout << "pl.results element "<<x->first<<" cumulative "<<x->second.cumulative<<std::endl;
//      std::cout <<pl.results.at(_left.first.substr(0,oL)).cpu<<std::endl;
//      std::cout <<pl.results.at(_right.first.substr(0,oR)).cpu<<std::endl;
      auto l=pl.results.at(_left.first.substr(0,oL));
      l.cumulative = pl.results.at(_left.first.substr(0,oL)).cumulative;
//      std::cout << "l assigned"<<_left.first.substr(0,oL)<<std::endl;
      auto r=pl.results.at(_right.first.substr(0,oR));
      r.cumulative = pl.results.at(_right.first.substr(0,oR)).cumulative;
//      std::cout << "r assigned"<<_right.first.substr(0,oR)<<std::endl;
//      if (lastcolon_left == lastcolon_right && _left.first.substr(0,lastcolon_left) == _right.first.substr(0,lastcolon_left)) { // brothers
//          std::cout << "compare brothers"<<std::endl;
//        } else {
//        }
      switch (l.parent==nullptr ? wall : l.parent->m_sortBy) {
        case wall:
//          std::cout << "wall test "<<l.wall<<" vs "<<r.wall<<" result "<< (l.wall < r.wall)<<" "<<l.cumulative<<_left.second.cumulative<<std::endl;
          if (l.cumulative==NULL)
            return l.wall < r.wall;
//          std::cout << "wall test "<<l.cumulative->wall<<" vs "<<r.cumulative->wall<<" result "<< (l.cumulative->wall < r.cumulative->wall)<<std::endl;
          return l.cumulative->wall < r.cumulative->wall;
        case cpu:
//          std::cout << "cpu test "<<l.cpu<<" vs "<<r.cpu<<" result "<< (l.cpu < r.cpu)<<std::endl;
          if (l.cumulative==NULL)
            return l.cpu < r.cpu;
//          std::cout << "cpu test "<<l.cumulative->cpu<<" vs "<<r.cumulative->cpu<<" result "<< (l.cumulative->cpu < r.cumulative->cpu)<<std::endl;
          return l.cumulative->cpu < r.cumulative->cpu;
        case operations:
          if (l.cumulative==NULL)
            return l.operations < r.operations;
          return l.cumulative->operations < r.cumulative->operations;
        case calls:
          if (l.cumulative==NULL)
            return l.calls < r.calls;
          return l.cumulative->calls < r.cumulative->calls;
        case name:
          return l.name > r.name;
        }
//      std::cout << "default return false"<<std::endl;
      throw std::logic_error("Failure to compare");
          }
  };

  sortMethod m_sortBy;
  std::string Name;
  std::vector<struct resources> resourcesStack, startResources;
  std::vector<int64_t>memoryStack0;
  std::vector<int64_t>memoryStack1;
  mutable resultMap results;
  int activeLevel; int level;
  int stopPrint_;
  void stopall();
  void accumulate(resultMap &results);
#ifdef PROFILER_MPI
const MPI_Comm m_communicator;
#endif
public:
/*!
 * \brief An object that will execute Profiler::start on construction, and Profiler::stop on destruction.
 */
class Push{
public:
  /*!
   * \brief Push to a new level on the stack of a Profiler object
   * \param profiler The Profiler object
   * \param name The name of the code segment to be profiled
   */
  Push(Profiler& profiler, const std::string & name)
    : m_name(name), m_profiler(profiler), m_operations(0) {m_profiler.start(m_name);}
  ~Push() {m_profiler.stop(m_name,m_operations);}
  /*!
   * \brief Advance the counter holding the notional number of operations executed in the code segment.
   * \param operations The number of additional operations.
   */
  void operator+=(const int operations) { m_operations+=operations;}
  /*!
   * \brief Advance the counter holding the notional number of operations executed in the code segment.
   */
  void operator++() { m_operations++;}
private:
  Push();
  const std::string m_name;
  Profiler& m_profiler;
  int m_operations;
};

};
std::ostream& operator<<(std::ostream& os, Profiler & obj);



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

#endif // PROFILER_H
