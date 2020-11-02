#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_PROFILER_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_PROFILER_H
#include <memory>
#include <string>

namespace molpro {
namespace profiler {
namespace tree {

class Counter;
template <class CounterT>
class Node;

/*!
 * @brief Instrumental profiler for timing sections of code
 *
 * Profiler constructs and navigates the call tree using start() and stop() to move between nodes or create them on
 * first run.
 * The profiler uses wall clock by default, but could also use cpu time, or do no timing at all and simply
 * accumulate number of calls to each node.
 * The maximum depth of profiler tree can be set using set_max_depth(), any nodes below max_depth are not created.
 * This allow for profiler calls to be used in Production code without degrading the performance
 * See README.md and examples for usage.
 *
 */
class Profiler {
public:
  const std::string description;              //!< name of the root node
  const std::string root_name = "All";        //!< name of the root node
  std::shared_ptr<Node<Counter>> root;        //!< root node of the profiler call tree
  std::shared_ptr<Node<Counter>> active_node; //!< the most recent active node.
protected:
  int m_max_depth;         //!< max depth level of profiler tree counting root as 0. Defaults to largest possible value.
  int m_current_depth = 0; //!< current depth of the active node
public:
  /*!
   * @brief Construct profiler and start timing
   * @param description_ description of profiler
   * @param with_wall whether to include wall time
   * @param with_cpu whether to include cpu time
   */
  explicit Profiler(std::string description_, bool with_wall = true, bool with_cpu = false);
  Profiler(Profiler&&) noexcept = default;
  ~Profiler();

  Profiler() = delete;
  Profiler(const Profiler&) = delete;
  Profiler& operator=(const Profiler&) = delete;
  Profiler& operator=(Profiler&&) = delete;

  /*!
   * @brief Get the maximum depth the profiler tree is allowed to reach
   *
   * Any calls to start() that would lead to profiler tree growing above max depth do nothing.
   * The corresponding stop() still needs to be posted, since the virtual depth of the tree
   * is still being tracked.
   *
   */
  int get_max_depth() const;
  //! Set the maximum depth the profiler tree is allowed to reach
  void set_max_depth(int new_max_depth);
  //! Get the current depth of the call stack from root to the active node. This tracks virtual nodes beyond max_depth,
  //! even though they are not constructed.
  int get_current_depth() const;

  /*!
   * @brief Traverse down to a child node and start timing
   * @param name name of the child node
   * @return
   */
  Profiler& start(const std::string& name);

  //! Stop the active node and traverse up to its parent
  Profiler& stop();

  //! Stop all nodes and traverse up to the root
  Profiler& stop_all();

  //! Access counter at the top of the call stack
  Counter& counter();

protected:
  //! Proxy object that calls start() on creation and stop() on destruction
  struct Proxy {
    Proxy(Profiler& prof, const std::string& name) : prof(prof) { prof.start(name); }
    ~Proxy() { prof.stop(); }
    //! Push a new proxy
    Proxy push(const std::string& name) { return Proxy(prof, name); }
    Profiler& prof;
  };

public:
  //! Returns a proxy object which will start() on construction and stop on destruction.
  Proxy push(const std::string& name) { return Proxy(*this, name); }
};

} // namespace tree
} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_PROFILER_H
