#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORTER_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORTER_H
#include <molpro/Profiler/Tree/Counter.h>
#include <molpro/Profiler/Tree/Node.h>
#include <molpro/Profiler/Tree/Profiler.h>

#include <algorithm>
#include <iostream>

namespace molpro {
namespace profiler {
namespace tree {

/*!
 * @brief Navigates the Profiler tree to access, accumulate and report its content
 * @todo for now this can be done with free functions
 */
struct Reporter {};

/*!
 * @brief Removes the first leaf from the tree and returns it
 * @param node root of the subtree
 * @return
 */
std::shared_ptr<Node<Counter>> remove_first_leaf(std::shared_ptr<Node<Counter>>& node) {
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

//! Returns path to the leaf by joining names from the root to the leaf
std::string tree_path(const std::shared_ptr<Node<Counter>>& leaf) {
  std::string path;
  auto node = leaf;
  while (node) {
    path = node->name + "." + path;
    node = node->parent;
  }
  path.erase(prev(path.end()));
  return path;
}

//! Depth-first-search through the tree and store Counter and its tree_path
template <class Compare>
void extract_counters(const std::shared_ptr<Node<Counter>>& node, std::map<Counter, std::string, Compare>& paths) {
  paths[node->counter] = tree_path(node);
  std::for_each(
      begin(node->children), end(node->children),
      [&paths](const typename decltype(node->children)::value_type& ch) { extract_counters(ch.second, paths); });
}

void report(Profiler& prof) {
  struct Compare {
    bool operator()(const Counter& x, const Counter& y) const {
      return x.get_wall().cumulative_time() > y.get_wall().cumulative_time();
    }
  };
  auto paths = std::map<Counter, std::string, Compare>{};
  extract_counters(prof.root, paths);
  //  while (auto leaf = remove_first_leaf(prof.root)) {
  //    paths[leaf->counter] = tree_path(leaf);
  //  }
  std::cout << "Profiler " << '"' << prof.description << '"' << " cumulative " << std::endl;
  std::for_each(begin(paths), end(paths), [](decltype(paths)::value_type& node) {
    std::cout << node.second;
    std::cout << "    calls=" << node.first.get_call_count() << ", ";
    std::cout << " wall=" << node.first.get_wall().cumulative_time();
    std::cout << std::endl;
  });
  std::cout << std::endl;
}

} // namespace tree
} // namespace profiler
} // namespace molpro

#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORTER_H
