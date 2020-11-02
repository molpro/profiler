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
 * @brief Profiler call graph tree, each node represents a timing section
 *
 * Profiler is just a call tree. A node is created on the first start() identified by its name, and is added to the
 * currently active leaf. The tree is traversed in a depth-first manner. Calling start() goes down the tree, and
 * stop moves you up the tree
 *
 * @code{c++}
 *  auto t = Profiler{"Test"};
 *  for (size_t i = 0; i < n; ++i){
 *      t.start("c"); // First node is created
 *          t.start("cc"); // First child, "c" is the currently active parent
 *              t.start("ccc"); // "cc" is now the currently active parent
 *              t.stop("ccc"); // all children of "cc" are now closed. "c" is the active parent again
 *          t.stop("cc"); // all children of "c" are now closed, but it is root, so it remains the active parent
 *          t.start("cc"); // "cc" is started again
 *          t.start("cc2"); // "cc2" is child of "cc"
 *          t.stop("cc"); // "cc" is not the name of the active leaf node, so it finds a parent called "cc" and stops it
 *                        // stopping a parent also stops all it's children
 *          t.start("cc2"); //
 *      t.stop()
 *  }
 *
 * @endcode
 *
 */
struct Profiler {
  const std::string description;              //!< name of the root node
  const std::string root_name = "All";        //!< name of the root node
  std::shared_ptr<Node<Counter>> root;        //!< root node of the profiler call tree
  std::shared_ptr<Node<Counter>> active_node; //!< the most recent active node.

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
   * @brief Traverse down to a child node and start timing
   * @param name name of the child node
   * @return
   */
  Profiler& start(const std::string& name);

  //! Stop the active node and traverse up to its parent
  Profiler& stop();

  //! Stop timing all nodes up to and including *name* in the call stack and traverse to its parent
  Profiler& stop(const std::string& name);

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
