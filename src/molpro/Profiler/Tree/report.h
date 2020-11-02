#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
#include <molpro/Profiler/Tree/Counter.h>
#include <molpro/Profiler/Tree/Node.h>
#include <molpro/Profiler/Tree/Profiler.h>

#include <algorithm>
#include <iostream>

namespace molpro {
namespace profiler {
namespace tree {

//! Returns path to the leaf by joining names from the root to the leaf
std::string tree_path(const std::shared_ptr<Node<Counter>>& leaf);

//! Depth-first-search through the tree and store Counter and its tree_path
template <class Compare>
void extract_counters(const std::shared_ptr<Node<Counter>>& node, std::map<Counter, std::string, Compare>& paths) {
  paths[node->counter] = tree_path(node);
  std::for_each(
      begin(node->children), end(node->children),
      [&paths](const typename decltype(node->children)::value_type& ch) { extract_counters(ch.second, paths); });
}

void report(Profiler& prof);

} // namespace tree
} // namespace profiler
} // namespace molpro

#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
