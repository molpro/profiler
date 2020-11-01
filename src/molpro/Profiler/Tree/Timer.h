#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_TIMER_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_TIMER_H

namespace molpro {
namespace profiler {
namespace tree {

//! Measures cpu or wall time. Can be constructed as a dummy that measures nothing.
class Timer {
public:
  enum Type { cpu, wall };

  Timer(Type type, bool is_dummy);

  //! start timing
  Timer& start();
  //! Stops timing, storing time since creation in cumulative
  Timer& stop();

  //! updates cumulative time
  void operator+=(const Timer& other);

  double start_time() const { return m_start; };
  double stop_time() const { return m_stop; };
  double cumulative_time() const { return m_cumulative; };
  bool stopped() const { return m_stopped; };
  bool dummy() const { return m_dummy; };

  const Type type; //!< type of timer
private:
  double m_start = 0;      //!< time of start
  double m_stop = 0;       //!< time of stop
  double m_cumulative = 0; //!< time from start to stop, accumulated over different timers
  bool m_stopped = true;   //!< whether timer was stopped, and subsequent stop will do nothing
  bool m_dummy = false;    //!< whether the timer is a dummy that does nothing
};

} // namespace tree
} // namespace profiler
} // namespace molpro

#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_TIMER_H
