#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_TIMER_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_TIMER_H

namespace molpro {
namespace profiler {

/*!
 * @brief Measures cpu or wall time. Can be constructed as a dummy that is always stopped
 *
 * The timer is started with start() and stopped with stop(). Repetitive calls to start() do not cause a restart, and
 * the start time from the first call is kept.
 *
 * Basic usage,
 * @code{.cpp}
 * auto t = Timer(Type::wall, false);
 * t.start()
 * // do some work
 * t.stop()
 * // ...
 * std::cout << "the work took = " << t.cumulative_time() << " seconds " << std::endl;
 * @endcode
 *
 * The timer measures cumulative time from all start/stop calls.
 *
 * Calling cumulative_time() while the timer is running returns the cumulative time at this moment without stopping.
 * For example,
 * @code{.cpp}
 * auto t = Timer(Type::wall, false);
 * t.start();
 * // 1 second of work
 * std::cout << "should say 1: " << t.cumulative_time() << std::endl;
 * // another second of work
 * std::cout << "should say 2: " << t.cumulative_time() << std::endl;
 * t.stop()
 * // lots more work
 * std::cout << "should still say 2: " << t.cumulative_time() << std::endl;
 * @endcode
 *
 */
class Timer {
public:
  enum Type { cpu, wall };
  Timer(Type type, bool is_dummy);
  //! Construct Timer with initial cumulative time
  Timer(double cumulative_time, Type type, bool is_dummy);

  //! start timing
  Timer& start();
  //! Stops timing, storing time since creation in cumulative
  Timer& stop();

  //! updates cumulative time
  void operator+=(const Timer& other);

  double start_time() const { return m_start; };
  double stop_time() const { return m_stop; };
  //! Cumulative time over all start/stop periods. If active than it includes the time from start to current moment.
  double cumulative_time() const;
  //! Timer is stopped and is not timing
  bool stopped() const { return m_stopped; };
  //! Timer is dummy and does nothing
  bool dummy() const { return m_dummy; };
  //! Reset the timer erasing all data
  void reset();
  //! type of timer
  Type type() const { return m_type; }

private:
  Type m_type;             //!< type of timer
  double m_start = 0;      //!< time of start
  double m_stop = 0;       //!< time of stop
  double m_cumulative = 0; //!< time from start to stop, accumulated over different timers
  bool m_stopped = true;   //!< whether timer was stopped, and subsequent stop will do nothing
  bool m_dummy = false;    //!< whether the timer is a dummy that does nothing
};

} // namespace profiler
} // namespace molpro

#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_TIMER_H
