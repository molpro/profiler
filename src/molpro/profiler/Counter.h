#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_COUNTER_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_COUNTER_H
#include <molpro/profiler/Timer.h>

#include <map>
#include <memory>
#include <string>

namespace molpro {
namespace profiler {

/*!
 * @brief Resource counter used for storing operation count, call count, timing information.
 */
class Counter {
public:
  Counter() = default;
  Counter(bool with_cpu_time, bool with_wall_time);

  //! Construct counter with initial values
  Counter(size_t call_count_, size_t operation_count_, double wall_time_, double cpu_time_, bool with_cpu_time,
          bool with_wall_time);

  //! Start timing, and increment call_count;
  Counter& start();

  //! Stop timing
  Counter& stop();

  //! Reset all counters and timers
  Counter& reset();

  //! Add to the operation count
  void add_operations(size_t ops) { operation_count += ops; }

  //! Accumulates all attributes
  void operator+=(const Counter& other);

  size_t get_call_count() const { return call_count; }
  size_t get_operation_count() const { return operation_count; }
  const Timer& get_cpu() const { return cpu; }
  const Timer& get_wall() const { return wall; }

protected:
  size_t call_count = 0;            //!< number of times this node was merged
  size_t operation_count = 0;       //!< number of operations performed
  Timer cpu = {Timer::cpu, true};   //!< cpu time
  Timer wall = {Timer::wall, true}; //!< wall time
};

} // namespace profiler
} // namespace molpro

#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_COUNTER_H
