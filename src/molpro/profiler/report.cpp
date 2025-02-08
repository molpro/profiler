#include "report.h"

#include <cassert>
#include <cmath>
#include <functional>
#include <iostream>
#include <map>
#include <utility>

namespace molpro {
namespace profiler {
namespace detail {

TreePath::TreePath(std::shared_ptr<Node<Counter>> node, bool cumulative) {
  if (node) {
    auto call_count = node->counter.get_call_count();
    auto operation_count = node->counter.get_operation_count();
    auto wall_time = node->counter.get_wall().cumulative_time();
    auto cpu_time = node->counter.get_cpu().cumulative_time();
    if (cumulative) {
      operation_count = total_operation_count(node);
    } else {
      for (const auto& child : node->children) {
        wall_time -= child.second->counter.get_wall().cumulative_time();
        cpu_time -= child.second->counter.get_cpu().cumulative_time();
      }
    }
    counter = Counter(call_count, operation_count, wall_time, cpu_time, false, false);
  }
  path = path_to_node(node);
  depth = path.empty() ? 0 : path.size() - 1;
}

std::list<std::string> path_to_node(std::shared_ptr<Node<Counter>> node) {
  auto path = std::list<std::string>{};
  while (node) {
    path.push_front(node->name);
    node = node->parent;
  }
  return path;
}

size_t total_operation_count(const std::shared_ptr<Node<Counter>>& node) {
  size_t total_count = 0;
  if (node) {
    total_count += node->counter.get_operation_count();
    for (const auto& child : node->children)
      total_count += total_operation_count(child.second);
  }
  return total_count;
}

std::string format_path_cumulative(const std::list<std::string>& path) {
  auto result = std::string();
  for (size_t i = 1; i < path.size(); ++i) {
    result += ".";
  }
  result += path.back();
  return result;
}

std::string format_path_not_cumulative(const std::list<std::string>& path) {
  auto result = std::string();
  for (const auto& p : path) {
    result += p + ":";
  }
  if (!result.empty())
    result.erase(prev(end(result)));
  return result;
}

template <class CompareTreePaths>
std::map<TreePath, std::shared_ptr<Node<Counter>>, CompareTreePaths>
sort_children(const std::shared_ptr<Node<Counter>>& root, bool cumulative) {
  auto children = std::map<TreePath, std::shared_ptr<Node<Counter>>, CompareTreePaths>{};
  for (auto it_child = root->children.rbegin(); it_child != root->children.rend(); ++it_child) {
    children.emplace(TreePath(it_child->second, cumulative), it_child->second);
  }
  return children;
}

// explicit instantiation of sort_children
template std::map<TreePath, std::shared_ptr<Node<Counter>>, Compare<AccessWall>>
sort_children<Compare<AccessWall>>(const std::shared_ptr<Node<Counter>>& root, bool cumulative);
template std::map<TreePath, std::shared_ptr<Node<Counter>>, Compare<AccessCPU>>
sort_children<Compare<AccessCPU>>(const std::shared_ptr<Node<Counter>>& root, bool cumulative);
template std::map<TreePath, std::shared_ptr<Node<Counter>>, Compare<AccessCalls>>
sort_children<Compare<AccessCalls>>(const std::shared_ptr<Node<Counter>>& root, bool cumulative);
template std::map<TreePath, std::shared_ptr<Node<Counter>>, Compare<AccessOperations>>
sort_children<Compare<AccessOperations>>(const std::shared_ptr<Node<Counter>>& root, bool cumulative);
template std::map<TreePath, std::shared_ptr<Node<Counter>>, Compare<None>>
sort_children<Compare<None>>(const std::shared_ptr<Node<Counter>>& root, bool cumulative);

std::list<TreePath> TreePath::convert_tree_to_paths(const std::shared_ptr<Node<Counter>>& root, bool cumulative,
                                                    SortBy sort_by) {
  auto path = TreePath(root, cumulative);
  if (sort_by == SortBy::wall) {
    return TreePath::convert_tree_to_paths<Compare<AccessWall>>(root, path, cumulative);
  } else if (sort_by == SortBy::cpu) {
    return TreePath::convert_tree_to_paths<Compare<AccessCPU>>(root, path, cumulative);
  } else if (sort_by == SortBy::calls) {
    return TreePath::convert_tree_to_paths<Compare<AccessCalls>>(root, path, cumulative);
  } else if (sort_by == SortBy::operations) {
    return TreePath::convert_tree_to_paths<Compare<AccessOperations>>(root, path, cumulative);
  } else if (sort_by == SortBy::none) {
    return TreePath::convert_tree_to_paths<Compare<None>>(root, path, cumulative);
  } else {
    assert(false);
  }
  return std::list<TreePath>();
}

template <class CompareTreePaths>
std::list<TreePath> TreePath::convert_tree_to_paths(const std::shared_ptr<Node<Counter>>& root, TreePath path,
                                                    bool cumulative) {
  auto paths = std::list<TreePath>{};
  paths.emplace_back(std::move(path));
  auto children = sort_children<CompareTreePaths>(root, cumulative);
  for (const auto& child : children) {
    auto child_paths = convert_tree_to_paths<CompareTreePaths>(child.second, std::move(child.first), cumulative);
    paths.splice(paths.end(), child_paths, child_paths.begin(), child_paths.end());
  }
  return paths;
}

void format_paths(std::list<std::string>& path_names, bool append) {
  size_t max_path_size = 0;
  for (const auto& path_name : path_names)
    if (path_name.size() > max_path_size)
      max_path_size = path_name.size();
  for (auto& path_name : path_names) {
    auto n_blank = max_path_size - path_name.size();
    std::string blank;
    for (size_t i = 0; i < n_blank; ++i)
      blank += " ";
    if (append)
      path_name += blank;
    else
      path_name = blank + path_name;
    path_name += " : ";
  }
}

std::string frequency(size_t n_op, double time) {
  std::stringstream ss;
  const std::string prefixes{"yzafpnum kMGTPEZY"};
  const int prefix_base = prefixes.find(" ");
  auto rate = double(n_op) / time;
  int prefix_rate = std::min(prefixes.size() - 1, size_t(std::max(0.0, (std::log10(rate) / 3) + prefix_base)));
  ss << " (" << rate / std::pow(1e3, (prefix_rate - prefix_base)) << " ";
  if (prefix_rate != prefix_base)
    ss << prefixes[prefix_rate];
  ss << "Hz)";
  return ss.str();
}

std::string seconds(double time) {
  std::stringstream out;
  const std::string prefixes{"yzafpnum kMGTPEZY"};
  const int prefix_base = prefixes.find(" ");
  int prefix_time = time <= 0
                        ? prefix_base
                        : std::min(prefixes.size() - 1, size_t(std::max(0.0, (std::log10(time) / 3) + prefix_base)));
  out << time / std::pow(1e3, (prefix_time - prefix_base)) << " ";
  if (prefix_time != prefix_base)
    out << prefixes[prefix_time];
  out << "s";
  return out.str();
}

void write_timing(std::ostream& out, double time, size_t n_op) {
  out << seconds(time);
  if (n_op and time > 0) {
    out << frequency(n_op, time);
  }
}

void write_report(const Node<Counter>& root, const std::string& description, const std::list<TreePath>& paths,
                  std::ostream& out, bool cumulative) {
  auto path_names = std::list<std::string>{};
  for (const auto& path : paths)
    path_names.emplace_back(format_single_path(path.path, cumulative));
  format_paths(path_names, cumulative);
  bool with_wall = !root.counter.get_wall().dummy();
  bool with_cpu = !root.counter.get_cpu().dummy();
  out << "Profiler " << '"' << description << '"';
  if (cumulative)
    out << " (cumulative) ";
  out << std::endl;
  auto path_name = path_names.begin();
  auto path = paths.begin();
  for (size_t i = 0; i < paths.size(); ++i, ++path_name, ++path) {
    out << *path_name;
    auto& counter = path->counter;
    out << "    calls=" << counter.get_call_count();
    if (with_wall)
      write_timing(out << " wall=", counter.get_wall().cumulative_time(), counter.get_operation_count());
    if (with_cpu)
      write_timing(out << " cpu=", counter.get_cpu().cumulative_time(), counter.get_operation_count());
    out << std::endl;
  }
}
} // namespace detail

void report(const Profiler& prof, std::ostream& out, bool cumulative, SortBy sort_by) {
  report(prof.root, prof.description(), out, cumulative, sort_by);
}

void report(const std::shared_ptr<Node<Counter>>& root, const std::string& description, std::ostream& out,
            bool cumulative, SortBy sort_by) {
  auto paths = detail::TreePath::convert_tree_to_paths(root, cumulative, sort_by);
  detail::write_report(*root, description, paths, out, cumulative);
}

#ifdef MOLPRO_PROFILER_MPI
namespace detail {

void reduce_all(long long int& operation_count, double& wall_time, double& cpu_time, MPI_Comm comm) {
  MPI_Request requests[3];
  MPI_Iallreduce(MPI_IN_PLACE, &operation_count, 1, MPI_LONG_LONG_INT, MPI_SUM, comm, &requests[0]);
  MPI_Iallreduce(MPI_IN_PLACE, &wall_time, 1, MPI_DOUBLE, MPI_MAX, comm, &requests[1]);
  MPI_Iallreduce(MPI_IN_PLACE, &cpu_time, 1, MPI_DOUBLE, MPI_SUM, comm, &requests[2]);
  MPI_Waitall(3, requests, MPI_STATUSES_IGNORE);
}

void reduce_root_only(long long int& operation_count, double& wall_time, double& cpu_time, MPI_Comm comm,
                      int root_process) {
  MPI_Request requests[3];
  MPI_Ireduce(MPI_IN_PLACE, &operation_count, 1, MPI_LONG_LONG_INT, MPI_SUM, root_process, comm, &requests[0]);
  MPI_Ireduce(MPI_IN_PLACE, &wall_time, 1, MPI_DOUBLE, MPI_MAX, root_process, comm, &requests[1]);
  MPI_Ireduce(MPI_IN_PLACE, &cpu_time, 1, MPI_DOUBLE, MPI_SUM, root_process, comm, &requests[2]);
  MPI_Waitall(3, requests, MPI_STATUSES_IGNORE);
}

/*!
 * @brief Data from the tree is reduced among all processors to an average profile
 * @note All trees must have the same structure
 *
 * Reduce operations
 * -----------------
 * call count - unchanged
 * operation count - total value on all process
 * wall times - maximum value on any process
 * cpu times - total of all processes
 *
 * @param root root of the tree
 * @param comm communicator
 * @return
 */
std::shared_ptr<Node<Counter>> synchronised_tree(const std::shared_ptr<Node<Counter>>& node,
                                                 const std::shared_ptr<Node<Counter>>& parent, MPI_Comm comm,
                                                 int root_process) {
  auto call_count = node->counter.get_call_count();
  long long int operation_count = node->counter.get_operation_count();
  auto wall_time = node->counter.get_wall().cumulative_time();
  auto cpu_time = node->counter.get_cpu().cumulative_time();
  if (root_process > 0)
    reduce_root_only(operation_count, wall_time, cpu_time, comm, root_process);
  else
    reduce_all(operation_count, wall_time, cpu_time, comm);
  auto counter = Counter(call_count, operation_count, wall_time, cpu_time, false, false);
  auto node_copy = Node<Counter>::make_root(node->name, counter);
  node_copy->parent = parent;
  for (const auto& child : node->children)
    node_copy->children.emplace(child.first, synchronised_tree(child.second, node_copy, comm, root_process));
  return node_copy;
}
} // namespace detail

void report(const Profiler& prof, std::ostream& out, MPI_Comm communicator, bool cumulative, SortBy sort_by) {
  report_root_process(prof,out,communicator,-1,cumulative,sort_by);
}

void report_root_process(const Profiler& prof, std::ostream& out, MPI_Comm communicator, int root_process,
                         bool cumulative, SortBy sort_by) {
  int size, rank, n_loc;
  MPI_Comm_size(communicator, &size);
  MPI_Comm_rank(communicator, &rank);
  n_loc = prof.root->count_nodes();
  std::vector<int> n_root(size);
  n_root[rank] = n_loc;
  for (int root = 0; root < size; ++root) {
    MPI_Bcast(&n_root[root], 1, MPI_INT, root, communicator);
    if (n_root[root] != n_root[0]) {
      out << "Profiler trees are not compatible between MPI ranks; report cannot be made." << std::endl;
      return;
    }
  }
  auto root_sync = detail::synchronised_tree(prof.root, nullptr, communicator, root_process);
  if (rank == root_process) {
    auto paths = detail::TreePath::convert_tree_to_paths(root_sync, cumulative, sort_by);
    detail::write_report(*prof.root, prof.description(), paths, out, cumulative);
  }
}

std::string get_dotgraph(const Profiler& prof, MPI_Comm communicator, int root_process, int* hot, int* cool,
                         double threshold, bool get_percentage_time) {
  int rank, n_loc, n_root;
  MPI_Comm_rank(communicator, &rank);
  n_loc = prof.root->count_nodes();
  if (rank == 0)
    n_root = n_loc;
  MPI_Bcast(&n_root, 1, MPI_INT, 0, communicator);
  if (n_root != n_loc)
    MPI_Abort(communicator, 0); // Profiler trees are not compatible
  auto root_sync = detail::synchronised_tree(prof.root, nullptr, communicator, root_process);
  return rank == root_process ? dotgraph::make_dotgraph(prof.root, prof.root->counter.get_wall().cumulative_time(), hot,
                                                        cool, threshold, get_percentage_time)
                              : "";
}
#endif

std::string get_dotgraph(const Profiler& prof, int hot[3], int cool[3], double threshold, bool get_percentage_time) {
  return dotgraph::make_dotgraph(prof.root, prof.root->counter.get_wall().cumulative_time(), hot, cool, threshold,
                                 get_percentage_time);
}

} // namespace profiler
} // namespace molpro