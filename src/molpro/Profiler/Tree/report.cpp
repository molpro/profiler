#include "report.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <queue>

namespace molpro {
namespace profiler {
namespace tree {
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
  } else {
    assert(false);
  }
}

template <class CompareTreePaths>
std::list<TreePath> TreePath::convert_tree_to_paths(const std::shared_ptr<Node<Counter>>& root, TreePath path,
                                                    bool cumulative) {
  auto paths = std::list<TreePath>{};
  paths.emplace_back(std::move(path));
  auto children = std::map<TreePath, std::shared_ptr<Node<Counter>>, CompareTreePaths>{};
  // iterate in reverse to preserve ordering of equivalent nodes
  for (auto it_child = root->children.rbegin(); it_child != root->children.rend(); ++it_child)
    children.emplace(TreePath(it_child->second, cumulative), it_child->second);
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

void write_timing(std::ostream& out, double time, size_t n_op) {
  out << time << ", ";
  if (n_op) {
    if (n_op >= 1e9)
      out << n_op / time / 1.e9 << " Gop/s";
    else if (n_op >= 1e6)
      out << n_op / time / 1.e6 << " Mop/s";
    else if (n_op >= 1e3)
      out << n_op / time / 1.e3 << " Kop/s";
    else
      out << n_op / time << " op/s";
  }
}

void write_report(const Profiler& prof, const std::list<TreePath>& paths, std::ostream& out, bool cumulative) {
  auto path_names = std::list<std::string>{};
  for (const auto& path : paths)
    path_names.emplace_back(format_single_path(path.path, cumulative));
  format_paths(path_names, cumulative);
  bool with_wall = !prof.root->counter.get_wall().dummy();
  bool with_cpu = !prof.root->counter.get_cpu().dummy();
  out << "Profiler " << '"' << prof.description << '"';
  if (cumulative)
    out << " (cumulative) ";
  out << std::endl;
  auto path_name = path_names.begin();
  auto path = paths.begin();
  for (size_t i = 0; i < paths.size(); ++i, ++path_name, ++path) {
    out << *path_name;
    auto& counter = path->counter;
    out << "    calls=" << counter.get_call_count() << "  ";
    if (with_wall)
      write_timing(out << "wall=", counter.get_wall().cumulative_time(), counter.get_operation_count());
    if (with_cpu)
      write_timing(out << "cpu=", counter.get_cpu().cumulative_time(), counter.get_operation_count());
    out << std::endl;
  }
}
} // namespace detail

void report(const Profiler& prof, std::ostream& out, bool cumulative, SortBy sort_by) {
  auto paths = detail::TreePath::convert_tree_to_paths(prof.root, cumulative, sort_by);
  detail::write_report(prof, paths, out, cumulative);
}

#ifdef MOLPRO_PROFILER_MPI
void report(const Profiler& prof, std::ostream& out, MPI_Comm communicator, bool cumulative, SortBy sort_by) {
//  auto paths = detail::TreePath::convert_tree_to_paths(prof.root, cumulative, sort_by);
//  auto data = detail::remove::get_report_data(paths, cumulative);
//  MPI_Request requests[3];
//  auto n = data.wall_times.size();
//  MPI_Iallreduce(&data.wall_times[0], MPI_IN_PLACE, n, MPI_DOUBLE, MPI_MAX, communicator, &requests[0]);
//  MPI_Iallreduce(&data.cpu_times[0], MPI_IN_PLACE, n, MPI_DOUBLE, MPI_SUM, communicator, &requests[1]);
//  MPI_Iallreduce(&data.operation_count[0], MPI_IN_PLACE, n, MPI_DOUBLE, MPI_SUM, communicator, &requests[2]);
//  MPI_Waitall(3, requests, MPI_STATUSES_IGNORE);
//  int np;
//  MPI_Comm_size(communicator, &np);
//  for (auto& op : data.operation_count)
//    op /= np;
//  data = detail::remove::sort_data(data, sort_by);
//  detail::format_paths(data.formatted_path_names, cumulative);
//  detail::remove::write_report(prof, out, data, cumulative);
}
#endif

} // namespace tree
} // namespace profiler
} // namespace molpro