#ifndef PROFILER_H
#define PROFILER_H
#ifdef __cplusplus
#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <stdint.h>

/*!
 * \brief The Profiler class: framework for timing code sections
 */
class Profiler
{
public:
  Profiler();
  /*!
   * \brief Profiler construct a named instance
   * \param name the title of this object
   * \param level
   * A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
   */
  Profiler(std::string name, const int level=INT_MAX);
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
   * \brief declare Ensure that a code segment is entered into the result table. This must be called for
   * any code segments for which start/stop is non-collective and therefore might not be called on some processes.
   * \param name name of the code segment
   */
  void declare(const std::string name="");
  /*!
   * \brief active set the maximum stack depth at which data collection is done.
   * \param level
   * A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
   */
  void active(const int level=INT_MAX);
  /*!
   * \brief Generate a printable representation of the object
   * \param verbosity how much to print
   * \param cumulative whether to print cumulative (ie including all children) resources
   * \param precision how many decimal places for seconds
   * \return
   */
  std::string str(const int verbosity=0, const bool cumulative=false, const int precision=3);

 public:
  struct resources {double cpu; double wall; int calls; long operations; std::string name; int64_t stack;
                    struct resources * cumulative;
                struct Profiler::resources& operator+=(const struct Profiler::resources &other);
                struct Profiler::resources& operator-=(const struct Profiler::resources &other);
                struct Profiler::resources operator+(const struct Profiler::resources &w2);
                struct Profiler::resources operator-(const struct Profiler::resources &w2);
               };
  struct resources getResources();
  static const std::string rootNode; //!< the tag on the top level node

  typedef std::map<std::string,struct Profiler::resources> resultMap;

  /*!
   * \brief totals
   * \return std::map of \ref resources
   */
  resultMap totals();

 private:
  void totalise(const struct resources now, const long operations, const int calls=1);
  template<class T> struct compareResources : std::binary_function<T,T,bool>
  { inline bool operator () (const T& _left, const T& _right)
    {
//      std::cout<<"Compare "<<_left.first<<" and "<<_right.first<<std::endl;
//      std::cout<<"compare "<<_left.second.wall<<" and "<<_right.second.wall<<std::endl;
//      std::cout<<"compare "<<_left.second.cumulative->wall<<" and "<<_right.second.cumulative->wall<<std::endl;
      if (_left.second.cumulative==NULL)
      return _left.second.wall < _right.second.wall;
      return _left.second.cumulative->wall < _right.second.cumulative->wall;
    }
  };

  std::string Name;
  std::vector<struct resources> resourcesStack;
  struct resources startResources;
  resultMap results;
  int activeLevel; int level;
  void stopall();
  void accumulate(resultMap &results);
};
  std::ostream& operator<<(std::ostream& os, Profiler & obj);

extern "C" {
#endif
void* profilerNew(char* name);
void profilerReset(void* profiler, char* name);
void profilerActive(void* profiler, int level);
void profilerStart(void* profiler, char* name);
void profilerDeclare(void* profiler, char* name);
void profilerStop(void* profiler, char* name, long operations=0);
char* profilerStr(void* profiler, int verbosity, int cumulative, int precision);
void profilerStrSubroutine(void*profiler, char* result, int maxResult, int verbosity, int cumulative, int precision);
#ifdef __cplusplus
}
#endif

#endif // PROFILER_H
