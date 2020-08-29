#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_COUNTER_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_COUNTER_H
#include <molpro/Profiler/Tree/Timer.h>

#include <map>
#include <memory>
#include <string>

namespace molpro {
namespace profiler {
namespace tree {

/*!
 * @brief Resource counter used for storing operation count, call count, timing information.
 */
struct Counter {
  Counter() = default;

  Counter(bool with_cpu_time, bool with_wall_time)
      : cpu(Timer{Timer::cpu, with_cpu_time}), wall(Timer{Timer::wall, with_wall_time}) {}

  //! Start timing, and increment call_count;
  Counter& start();

  //! Stop timing
  Counter& stop();

  //! Add to the operation count
  void add_operations(size_t ops) { operation_count += ops; }

  //! Accumulates all attributes
  void operator+=(const Counter& other);

  size_t call_count = 0;             //!< number of times this node was merged
  size_t operation_count = 0;        //!< number of operations performed
  Timer cpu = {Timer::cpu, false};   //!< cpu time
  Timer wall = {Timer::wall, false}; //!< wall time
};

/*!
 * @brief Represents a counter object as a node in a tree.
 */
struct CounterNode {
  explicit CounterNode(std::string name, Counter counter, std::shared_ptr<CounterNode> parent = nullptr)
      : name(std::move(name)), counter(std::move(counter)), parent(std::move(parent)) {}

  std::string name; //!< name of the node. This is a duplicate, same name is stored in parent's map of children.
  Counter counter;  //! resource counter
  std::shared_ptr<CounterNode> parent;                          //!< parent node
  std::map<std::string, std::shared_ptr<CounterNode>> children; //!< child nodes
};

} // namespace tree
} // namespace profiler
} // namespace molpro

#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_COUNTER_H
