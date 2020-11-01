#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_PROFILER_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_PROFILER_H

#include <molpro/Profiler/Tree/Counter.h>
#include <molpro/Profiler/Tree/Node.h>

#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <vector>

namespace molpro {
namespace profiler {
namespace tree {

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
  explicit Profiler(std::string description_, bool with_wall = true, bool with_cpu = false)
      : description(std::move(description_)), root(Node<Counter>::make_root(root_name, Counter{with_cpu, with_wall})),
        active_node(root) {
    root->counter.start();
  }

  /*!
   * @brief Traverse down to a child node and start timing
   * @param name name of the child node
   * @return
   */
  Profiler& start(const std::string& name) {
    auto ch = active_node->children.find(name);
    if (ch == active_node->children.end()) {
      auto count = Counter{!root->counter.get_cpu().dummy(), !root->counter.get_wall().dummy()};
      count.start();
      active_node = Node<Counter>::add_child(name, count, active_node);
    } else {
      ch->second->counter.start();
      active_node = ch->second;
    }
    return *this;
  }

  //! Stop timing current leaf and traverse up to its parent
  Profiler& stop() {
    active_node->counter.stop();
    if (active_node->parent)
      active_node = active_node->parent;
    return *this;
  }

  //! Stop timing all nodes up to and including name, and traverse to its parent
  Profiler& stop(const std::string& name) {
    while (active_node->parent and active_node->name != name) {
      stop();
    }
    return stop();
  }
  //! Stop all nodes and traverse up to the root
  Profiler& stop_all() { return stop(root->name); }

  //! Access counter at the top of the call stack
  Counter& counter() { return active_node->counter; }

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
