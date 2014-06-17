#ifndef PROFILER_H
#define PROFILER_H
#include <iostream>
#include <string>
#include <map>
#include <stack>

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
   */
  Profiler(std::string name);
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
   * \brief Generate a printable representation of the object
   * \param verbosity how much to print
   * \param precision how many decimal places for seconds
   * \return
   */
  std::string str(const int verbosity=0, const int precision=3);

private:
  struct times {double cpu; double wall; int calls; long operations; std::string name;
                struct Profiler::times& operator+=(const struct Profiler::times &other);
                struct Profiler::times& operator-=(const struct Profiler::times &other);
                struct Profiler::times operator+(const struct Profiler::times &w2);
                struct Profiler::times operator-(const struct Profiler::times &w2);
               };
  typedef std::map<std::string,struct Profiler::times> resultMap;
  template<class T> struct compareTimes : std::binary_function<T,T,bool>
  { inline bool operator () (const T& _left, const T& _right)
    {
      return _left.second.wall < _right.second.wall;
    }
  };

  std::string Name;
  std::stack<struct times> stack;
  struct times startTimes;
  struct times getTimes();
  resultMap results;
  void stopall();
};
  std::ostream& operator<<(std::ostream& os, Profiler & obj);

#endif // PROFILER_H
