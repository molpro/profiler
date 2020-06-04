#ifndef MOLPRO_PROFILER_PROFILER_H
#define MOLPRO_PROFILER_PROFILER_H
#ifdef __cplusplus
#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <climits>
#include <stdexcept>
#include <stdint.h>
#define PROFILER_DEFAULT_KEY 0

namespace molpro {
namespace profiler{
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
class ProfilerSerial {
public:
    ProfilerSerial() = delete;
    /*!
     * \brief Sorting criteria for categories in report.
     */
    enum sortMethod {
        wall //!< Sort by real time.
        , cpu//!< Sort by CPU time.
        , name //!< Sort alphabetically by name.
        , calls //!< Sort by number of calls.
        , operations //!< Sort by number of operations.
    };
    using key_t = int; //!< extra key to provide consistent interface with ProfilerMPI. No purpose
    /*!
     * \brief Profiler construct a named instance.
     * \param name the title of this object.
     * \param sortBy Criterion for sorting printed result table.
     * \param level
     * \param key currently has no purpose. Gives consistent interface with ProfilerMPI
     * A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
     */
    ProfilerSerial(const std::string &name, sortMethod sortBy = wall, const int level = INT_MAX,
                   key_t key = PROFILER_DEFAULT_KEY);
    /*!
     * \brief Reset the object.
     * \param name The title of this object.
     */
    void reset(const std::string &name);
    /*!
     * \brief Begin timing a code segment.
     * \param name Name of the code segment.
     */
    void start(const std::string &name);
    /*!
     * \brief Finish timing a code segment.
     * \param name If given, must match the argument of the previous start call.
     * \param operations If given, a count of the number of operations (or whatever you like) carried out.
     */
    void stop(const std::string &name = "", long operations = 0);
    /*!
     * \brief active set the maximum stack depth at which data collection is done.
     * \param level
     * A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
     * \param stopPrint if non-negative, \ref stop() prints the statistics since the corresponding \ref start()
     */
    void active(const int level = INT_MAX, const int stopPrint = -1);
    /*!
     * \brief Generate a printable representation of the object.
     * Must be called by all MPI processes collectively.
     * \param verbosity How much to print.
     * \param cumulative Whether to print cumulative (ie including all children) resources.
     * \param precision How many decimal places for seconds.
     * \return
     */
    std::string str(const int verbosity = 0, const bool cumulative = true, const int precision = 3) const;
public:
    class Push;

public:
    /*!
     * \brief Push to a new level on the stack of a Profiler object.
     * \param name The name of the code segment to be profiled.
     * \return An object that when destroyed will call the corresponding Profiler::stop.
     */
    Push push(const std::string &name = "");

public:
    struct resources {
        double cpu;
        double wall;
        int calls;
        long operations;
        std::string name;
        int64_t stack;
        struct resources *cumulative;
        const ProfilerSerial *parent;
        std::string
        str(const int width = 0, const int verbosity = 0, const bool cumulative = false, const int precision = 3,
            const std::string defaultName = "") const;
        struct ProfilerSerial::resources &operator+=(const struct ProfilerSerial::resources &other);
        struct ProfilerSerial::resources &operator-=(const struct ProfilerSerial::resources &other);
        struct ProfilerSerial::resources operator+(const struct ProfilerSerial::resources &w2);
        struct ProfilerSerial::resources operator-(const struct ProfilerSerial::resources &w2);

        resources() {
            cpu = 0;
            wall = 0;
            calls = 0;
            operations = 0;
            stack = 0;
            cumulative = nullptr;
            parent = nullptr;
        }
    };

    struct resources getResources();

    typedef std::map<std::string, struct ProfilerSerial::resources> resultMap;

    /*!
     * \brief Obtain a summary of the resources used for each category.
     * Must be called by all MPI processes collectively.
     * \return std::map of \ref resources
     */
    virtual resultMap totals() const;

protected:
    void totalise(const struct resources now, const long operations, const int calls = 1);

    template<class T>
    struct compareResources : std::binary_function<T, T, bool> {
        inline bool operator()(const T &_left, const T &_right) {
            if (_left.first.rfind(':') == std::string::npos) return false;
            if (_right.first.rfind(':') == std::string::npos) return true;
            auto leftname = _left.first + ":";
            auto rightname = _right.first + ":";
            if (leftname.size() > rightname.size() && leftname.substr(0, rightname.size()) == rightname) {
                return true;
            }
            if (rightname.size() > leftname.size() && rightname.substr(0, leftname.size()) == leftname) {
                return false;
            }
            const ProfilerSerial &pl = *(_left.second.parent);
            // find the common ancestor
            std::ptrdiff_t o;
            for (o = 0; o < static_cast<std::ptrdiff_t>(std::max(leftname.size(), rightname.size())) &&
                        leftname[o] == rightname[o]; o++);
            while ((leftname[o] != ':' || rightname[o] != ':') && o >= 0) o--;
            auto oL = leftname.find(':', o + 1);
            if (oL == std::string::npos) oL = leftname.size();
            auto oR = rightname.find(':', o + 1);
            if (oR == std::string::npos) oR = rightname.size();
            auto l = pl.results.at(leftname.substr(0, oL));
            l.cumulative = pl.results.at(leftname.substr(0, oL)).cumulative;
            auto r = pl.results.at(rightname.substr(0, oR));
            r.cumulative = pl.results.at(rightname.substr(0, oR)).cumulative;
            if (false) { // check the ancestors for debugging purposes only
                std::cout << "\ncompare " << leftname << " and " << rightname << std::endl;
                std::cout << "ancestors\n" << leftname.substr(0, oL) << "\n" << rightname.substr(0, oR) << std::endl;
                auto leftsep = leftname.substr(0, oL).find_last_of(":");
                auto leftroot = leftname.substr(0, leftsep);
                auto leftqualified = leftname.substr(leftsep + 1, oL - leftsep - 1);
                auto rightsep = rightname.substr(0, oR).find_last_of(":");
                auto rightroot = rightname.substr(0, rightsep);
                auto rightqualified = rightname.substr(rightsep + 1, oR - rightsep - 1);
                if (rightroot != leftroot || rightqualified.substr(0, rightqualified.find(":")) ==
                                             leftqualified.substr(0, leftqualified.find(":"))) {
                    std::cout << "L: " << leftroot << "////" << leftqualified << std::endl;
                    std::cout << "R: " << rightroot << "////" << rightqualified << std::endl;
                    if (rightroot != leftroot) throw std::logic_error("left and right roots not the same");
                    if (rightqualified.substr(0, rightqualified.find(":")) ==
                        leftqualified.substr(0, leftqualified.find(":")))
                        throw std::logic_error("left and right nodes unexpectedly identical");
                }
            }
            switch (l.parent == nullptr ? wall : l.parent->m_sortBy) {
                case wall:
                    if (l.cumulative == NULL)
                        return l.wall < r.wall;
                    return l.cumulative->wall < r.cumulative->wall;
                case cpu:
                    if (l.cumulative == NULL)
                        return l.cpu < r.cpu;
                    return l.cumulative->cpu < r.cumulative->cpu;
                case operations:
                    if (l.cumulative == NULL)
                        return l.operations < r.operations;
                    return l.cumulative->operations < r.cumulative->operations;
                case calls:
                    if (l.cumulative == NULL)
                        return l.calls < r.calls;
                    return l.cumulative->calls < r.cumulative->calls;
                case name:return l.name > r.name;
            }
            throw std::logic_error("Failure to compare");
        }
    };

    sortMethod m_sortBy;
    std::string Name;
    std::vector<struct resources> resourcesStack, startResources;
    std::vector<int64_t> memoryStack0;
    std::vector<int64_t> memoryStack1;
    mutable resultMap results;
    int activeLevel;
    int level;
    int stopPrint_;
    void stopall();
    void accumulate(resultMap &results);
public:
    /*!
   * \brief An object that will execute Profiler::start on construction, and Profiler::stop on destruction.
   */
    class Push {
    public:
        /*!
       * \brief Push to a new level on the stack of a Profiler object
       * \param profiler The Profiler object
       * \param name The name of the code segment to be profiled
       */
        Push(ProfilerSerial &profiler, const std::string &name)
                : m_name(name), m_profiler(profiler), m_operations(0) {m_profiler.start(m_name);}

        ~Push() {m_profiler.stop(m_name, m_operations);}

        /*!
       * \brief Advance the counter holding the notional number of operations executed in the code segment.
       * \param operations The number of additional operations.
       */
        void operator+=(const uint64_t operations) {m_operations += operations;}

        /*!
       * \brief Advance the counter holding the notional number of operations executed in the code segment.
       */
        void operator++() {m_operations++;}

    protected:
        Push();
        const std::string m_name;
        ProfilerSerial &m_profiler;
        uint64_t m_operations;
    };

};
} //  namespace profiler
} // namespace molpro
//! Writes summary of profile to the stream, with end of line
std::ostream &operator<<(std::ostream &os, molpro::profiler::ProfilerSerial &obj);

#endif // __cplusplus

#endif // MOLPRO_PROFILER_PROFILER_H
