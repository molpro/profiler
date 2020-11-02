#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
#include <molpro/Profiler/Tree/Counter.h>
#include <molpro/Profiler/Tree/Node.h>
#include <molpro/Profiler/Tree/Profiler.h>

#include <list>
#include <memory>
#include <ostream>
#include <string>

namespace molpro {
namespace profiler {
namespace tree {

//! Utility for storing a node as a path from root to that node and corresponding Counter
struct TreePath {
  explicit TreePath(std::shared_ptr<Node<Counter>> node);

  //! Performs Depth-First-Search and converts the whole tree to a list of TreePath objects
  static std::list<TreePath> convert_subtree_to_paths(const std::shared_ptr<Node<Counter>>& root);

  //! convert path to a formatted string
  std::string format_path() const;

  Counter counter;             //!< copy of the counter object
  std::list<std::string> path; //!< concatenation of names from root to the node
};

void report(Profiler& prof, std::ostream& out);

} // namespace tree
} // namespace profiler
} // namespace molpro

#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
