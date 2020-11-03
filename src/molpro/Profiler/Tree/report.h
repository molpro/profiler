#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
#ifdef MOLPRO_PROFILER_MPI
#include <mpi.h>
#endif

#include <molpro/Profiler/Tree/Counter.h>
#include <molpro/Profiler/Tree/Node.h>
#include <molpro/Profiler/Tree/Profiler.h>
#include <molpro/Profiler/Tree/SortBy.h>

#include <list>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

namespace molpro {
namespace profiler {
namespace tree {

/*!
 * @brief Reports content of Profiler
 * @param prof profiler to analyse and write
 * @param out output stream to write to
 * @param sort_by what parameter to use for sorting. Sorting is done on the same level only.
 * @param cumulative whether cumulative timings should be used or only time spend by the node and not its children
 */
void report(const Profiler& prof, std::ostream& out, bool cumulative = true, SortBy sort_by = SortBy::wall);

#ifdef MOLPRO_PROFILER_MPI
void report(const Profiler& prof, std::ostream& out, MPI_Comm communicator, bool cumulative = true,
            SortBy sort_by = SortBy::wall);
#endif

namespace detail {
//! Utility for storing a node as a path from root to that node and corresponding Counter
struct TreePath {
  //! Processes path from root nodes into list of strings and copies Counter accounting for cumulative effects
  explicit TreePath(std::shared_ptr<Node<Counter>> node, bool cumulative);

  /*!
   * @brief Performs Depth-First-Search and converts the whole tree to a list of TreePath objects
   *
   * The counter is processed so that all data is either cumulative or not. Particularly, wall time and operation
   * count have to be taken care of.
   *
   * All children are added in an ordered fashion based on sort_by.
   *
   * @param root root node
   * @param cumulative whether to use cumulative times and operation counts
   * @param sort_by what parameter to sort by
   */
  static std::list<TreePath> convert_tree_to_paths(const std::shared_ptr<Node<Counter>>& root, bool cumulative,
                                                   SortBy sort_by);

private:
  template <class CompareTreePaths>
  static std::list<TreePath> convert_tree_to_paths(const std::shared_ptr<Node<Counter>>& root, TreePath path,
                                                   bool cumulative);

public:
  Counter counter;             //!< copy of the counter object with cumulative effects or lack of them already accounted
  std::list<std::string> path; //!< concatenation of names from root to the node
  size_t depth = 0;            //!< depth of the node (root is 0)
};

//! Returns path of node names from root to node
std::list<std::string> path_to_node(std::shared_ptr<Node<Counter>> node);

//! Performs depth first search through the tree and accumulates operation counter value
size_t total_operation_count(const std::shared_ptr<Node<Counter>>& node);

//! Less than comparison operator for TreePath with access to different parameters.
//! The implicit equality condition (!Compare{}(a,b) && !Compare{}(b,a)) is false by design.
template <class AccessParameter>
struct Compare {
  bool operator()(const TreePath& l, const TreePath& r) {
    bool depth_check = l.depth < r.depth;
    bool parameter_check = AccessParameter{}(l) > AccessParameter{}(r);
    bool result = depth_check ? depth_check : parameter_check;
    if (!result) {
      if (l.depth == r.depth && AccessParameter{}(l) == AccessParameter{}(r)) {
        result = true;
      }
    }
    return result;
  }
};
struct AccessWall {
  double operator()(const TreePath& t) { return t.counter.get_wall().cumulative_time(); }
};
struct AccessCPU {
  double operator()(const TreePath& t) { return t.counter.get_cpu().cumulative_time(); }
};
struct AccessCalls {
  double operator()(const TreePath& t) { return t.counter.get_call_count(); }
};
struct AccessOperations {
  double operator()(const TreePath& t) { return t.counter.get_operation_count(); }
};

/*!
 * @brief convert path to a formatted string
 *
 * All parent nodes are replaced by dots
 * {root,A,B,C} becomes "...C"
 *
 * @param path list of names from root to selected node
 */
std::string format_path_cumulative(const std::list<std::string>& path);
/*!
 * @brief convert path to a formatted string
 *
 * Names are joined with ":" delimiter
 * {root,A,B,C} becomes "root:A:B:C"
 *
 * @param path list of names from root to selected node
 */
std::string format_path_not_cumulative(const std::list<std::string>& path);

namespace remove {
struct ReportData {
  std::list<std::string> formatted_path_names;
  std::vector<int> depth;
  std::vector<size_t> calls;
  std::vector<double> operation_count;
  std::vector<double> wall_times;
  std::vector<double> cpu_times;
};

//! extracts data from TreePath objects
ReportData get_report_data(const std::list<TreePath>& paths, bool cumulative);

//! Sorts the data based on depth (ascending) and sort_by parameter (descending)
ReportData sort_data(const ReportData& data, SortBy sort_by);
} // namespace remove

/*!
 * @brief Format paths for output
 *
 * Appends or prepends blank lines to each path if cumulative is true or not, respectively.
 * This ensures that all names are the same length.
 *
 * @param path_names list of paths (see format_path_cumulative and format_path_not_cumulative)
 * @param append whether to append or prepend blank lines
 */
void format_paths(std::list<std::string>& path_names, bool append);

void write_timing(std::ostream& out, double time, size_t n_op);

namespace remove {
void write_report(const Profiler& prof, std::ostream& out, const ReportData& data, bool cumulative);
} // namespace remove

} // namespace detail
} // namespace tree
} // namespace profiler
} // namespace molpro

#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_REPORT_H
