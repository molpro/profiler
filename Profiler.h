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
 * \brief The Profiler class: framework for timing code sections
 */
class Profiler
{
  Profiler();
public:
  enum sortMethod { wall, cpu, name };
  /*!
   * \brief Profiler construct a named instance
   * \param name the title of this object
   * \param sortBy Criterion for sorting printed result table
   * \param level
   * A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
   * \param communicator The MPI communicator over which statistics should be aggregated.
   */
  Profiler(std::string name, sortMethod sortBy=wall, const int level=INT_MAX
#ifdef PROFILER_MPI
	   , const MPI_Comm communicator=MPI_COMM_WORLD
#endif
           );
  /*!
   * \brief reset the object
   * \param name the title of this object
   */
  void reset(const std::string name);
  /*!
   * \brief start begin timing a code segment
   * \param name name of the code segment
   */
  void start(const std::string name);
  /*!
   * \brief stop finish timing a code segment
   * \param name if given, must match the argument of the previous start call
   * \param operations if given, a count of the number of operations (or whatever you like) carried out
   */
  void stop(const std::string name="",long operations=0);
  /*!
   * \brief active set the maximum stack depth at which data collection is done.
   * \param level
   * A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
   * \param stopPrint if non-negative, \ref stop() prints the statistics since the corresponding \ref start()
   */
  void active(const int level=INT_MAX, const int stopPrint=-1);
  /*!
   * \brief Generate a printable representation of the object
   * \param verbosity how much to print
   * \param cumulative whether to print cumulative (ie including all children) resources
   * \param precision how many decimal places for seconds
   * \return
   */
  std::string str(const int verbosity=0, const bool cumulative=false, const int precision=3) const;
private:
class Push;
public:
  /*!
   * \brief Push to a new level on the stack of a Profiler object.
   * \param name The name of the code segment to be profiled.
   * \return An object that when destroyed will call the corresponding Profiler::stop.
   */
  Push push(std::string name="");

 public:
  struct resources {double cpu; double wall; int calls; long operations; std::string name; int64_t stack;
                    struct resources * cumulative;
                   const Profiler *parent;
                std::string str(const int width=0, const int verbosity=0, const bool cumulative=false, const int precision=3, const std::string defaultName="") const;
                struct Profiler::resources& operator+=(const struct Profiler::resources &other);
                struct Profiler::resources& operator-=(const struct Profiler::resources &other);
                struct Profiler::resources operator+(const struct Profiler::resources &w2);
                struct Profiler::resources operator-(const struct Profiler::resources &w2);
               };
  struct resources getResources();

  typedef std::map<std::string,struct Profiler::resources> resultMap;

  /*!
   * \brief totals
   * \return std::map of \ref resources
   */
  resultMap totals() const;

 private:
  void totalise(const struct resources now, const long operations, const int calls=1);
  template<class T> struct compareResources : std::binary_function<T,T,bool>
  { inline bool operator () (const T& _left, const T& _right)
    {
//      std::cout<<"Compare "<<_left.first<<" and "<<_right.first<<std::endl;
//      std::cout<<"compare "<<_left.second.wall<<" and "<<_right.second.wall<<std::endl;
//      std::cout<<"compare "<<_left.second.cumulative->wall<<" and "<<_right.second.cumulative->wall<<std::endl;
        switch (_left.second.parent->m_sortBy) {
          case wall:
            if (_left.second.cumulative==NULL)
              return _left.second.wall < _right.second.wall;
            return _left.second.cumulative->wall < _right.second.cumulative->wall;
          case cpu:
            if (_left.second.cumulative==NULL)
              return _left.second.cpu < _right.second.cpu;
            return _left.second.cumulative->cpu < _right.second.cumulative->cpu;
          case name:
            return _left.second.name > _right.second.name;
          }
    }
  };

  sortMethod m_sortBy;
  std::string Name;
  std::vector<struct resources> resourcesStack, startResources;
  std::vector<int64_t>memoryStack0;
  std::vector<int64_t>memoryStack1;
  resultMap results;
  int activeLevel; int level;
  int stopPrint_;
  void stopall();
  void accumulate(resultMap &results);
#ifdef PROFILER_MPI
const MPI_Comm m_communicator;
#endif
private:
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
