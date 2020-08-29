#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_PROFILER_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_PROFILER_H

#include <molpro/Profiler/Tree/Counter.h>

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
  const std::string description;       //!< name of the root node
  const std::string root_name = "All"; //!< name of the root node
  std::shared_ptr<CounterNode> root;   //!< root node
  std::shared_ptr<CounterNode> leaf;   //!< The active leaf node is the last node to be started.
  explicit Profiler(std::string description_, bool with_wall = true, bool with_cpu = false)
      : description(std::move(description_)),
        root(std::make_shared<CounterNode>(root_name, Counter{with_cpu, with_wall})), leaf(root) {
    root->counter.start();
  }

  /*!
   * @brief Traverse down to a child node and start timing
   * @param name name of the child node
   * @return
   */
  Profiler& start(const std::string& name) {
    auto ch = leaf->children.find(name);
    if (ch == leaf->children.end()) {
      auto count = Counter{root->counter.cpu.not_dummy(), root->counter.wall.not_dummy()}.start();
      leaf->children[name] = std::make_shared<CounterNode>(name, count, leaf);
      leaf = leaf->children[name];
    } else {
      ch->second->counter.start();
      leaf = ch->second;
    }
    return *this;
  }

  //! Stop timing current leaf and traverse up to its parent
  Profiler& stop() {
    leaf->counter.stop();
    leaf = leaf->parent;
    return *this;
  }

  //! Access leaf counter
  Counter& counter() { return leaf->counter; }
};

std::shared_ptr<CounterNode> remove_first_leaf(std::shared_ptr<CounterNode>& node) {
  auto leaf = node;
  if (leaf->children.empty())
    return nullptr;
  while (!leaf->children.empty()) {
    leaf = leaf->children.begin()->second;
  }
  auto it = leaf->parent->children.begin();
  leaf->parent->children.erase(it);
  return leaf;
}

std::string tree_path(const std::shared_ptr<CounterNode>& leaf) {
  std::string path;
  auto node = leaf;
  while (node) {
    path = node->name + "." + path;
    node = node->parent;
  }
  path.erase(prev(path.end()));
  return path;
}

void report(Profiler& prof) {
  struct Compare {
    bool operator()(const Counter& x, const Counter& y) { return x.wall.cumulative_time() > y.wall.cumulative_time(); }
  };
  auto paths = std::map<Counter, std::string, Compare>{};
  while (auto leaf = remove_first_leaf(prof.root)) {
    paths[leaf->counter] = tree_path(leaf);
  }
  std::cout << "Profiler " << '"' << prof.description << '"' << std::endl;
  std::for_each(begin(paths), end(paths), [](decltype(paths)::value_type& node) {
    std::cout << node.second;
    std::cout << "    calls=" << node.first.call_count << ", ";
    std::cout << " wall=" << node.first.wall.cumulative_time();
    std::cout << std::endl;
    std::cout << std::endl;
  });
}

} // namespace tree
} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_PROFILER_H
