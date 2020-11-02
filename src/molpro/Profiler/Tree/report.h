#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
#include <molpro/Profiler/Tree/Counter.h>
#include <molpro/Profiler/Tree/Node.h>
#include <molpro/Profiler/Tree/Profiler.h>

#include <list>
#include <vector>
#include <memory>
#include <ostream>
#include <string>

namespace molpro {
namespace profiler {
namespace tree {

/*!
 * @brief Reports content of Profiler
 * @param prof profiler to analyse and write
 * @param out output stream to write to
 * @param cumulative whether cumulative timings should be used or only time spend by the node and not its children
 */
void report(const Profiler& prof, std::ostream& out, bool cumulative = true);

namespace detail {
//! Utility for storing a node as a path from root to that node and corresponding Counter
struct TreePath {
  explicit TreePath(std::shared_ptr<Node<Counter>> node);

  //! Performs Depth-First-Search and converts the whole tree to a list of TreePath objects
  static std::list<TreePath> convert_subtree_to_paths(const std::shared_ptr<Node<Counter>>& root);

  Counter counter;               //!< copy of the counter object
  std::list<std::string> path;   //!< concatenation of names from root to the node
  double wall_time_children = 0; //!< wall time spent by children
  double cpu_time_children = 0;  //!< cpu time spent by children
};

//! convert path to a formatted string
std::string format_path_cumulative(const std::list<std::string>& path);
//! convert path to a formatted string
std::string format_path_not_cumulative(const std::list<std::string>& path);

struct ReportData {
  std::list<std::string> formatted_path_names;
  std::vector<int> depth;
  std::vector<size_t> calls;
  std::vector<double> wall_times;
  std::vector<double> cpu_times;
};

//! extracts data from TreePath objects
ReportData get_report_data(const std::list<TreePath>& paths, bool cumulative);

//! Sorts the data based on depth (ascending) and wall time (descending)
ReportData sort_data(const ReportData& data);

void format_paths(std::list<std::string>& path_names, bool cumulative);

void write_report(const Profiler& prof, std::ostream& out, const ReportData& data, bool cumulative);

} // namespace detail
} // namespace tree
} // namespace profiler
} // namespace molpro

#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H