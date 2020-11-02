#include "report.h"

#include <algorithm>
#include <functional>
#include <queue>

namespace molpro {
namespace profiler {
namespace tree {
namespace detail {

TreePath::TreePath(std::shared_ptr<Node<Counter>> node) {
  if (node) {
    counter = node->counter;
    for (const auto& child : node->children) {
      wall_time_children += child.second->counter.get_wall().cumulative_time();
      cpu_time_children += child.second->counter.get_wall().cumulative_time();
    }
  }
  while (node) {
    path.push_front(node->name);
    node = node->parent;
  }
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

std::list<TreePath> TreePath::convert_subtree_to_paths(const std::shared_ptr<Node<Counter>>& root) {
  auto paths = std::list<TreePath>{};
  auto children = std::queue<std::shared_ptr<Node<Counter>>, std::list<std::shared_ptr<Node<Counter>>>>{};
  auto add_children = [&children](const std::shared_ptr<Node<Counter>>& nd) {
    for (const auto& ch : nd->children)
      children.push(ch.second);
  };
  children.push(root);
  while (!children.empty()) {
    auto node = children.front();
    children.pop();
    paths.emplace_back(node);
    add_children(node);
  }
  return paths;
}

ReportData get_report_data(const std::list<TreePath>& paths, bool cumulative) {
  auto data = ReportData();
  for (const auto& path : paths) {
    data.depth.push_back(path.path.size());
    data.calls.push_back(path.counter.get_call_count());
    if (cumulative) {
      data.formatted_path_names.emplace_back(detail::format_path_cumulative(path.path));
      data.wall_times.push_back(path.counter.get_wall().cumulative_time());
      data.cpu_times.push_back(path.counter.get_cpu().cumulative_time());
    } else {
      data.formatted_path_names.emplace_back(detail::format_path_not_cumulative(path.path));
      data.wall_times.push_back(path.counter.get_wall().cumulative_time() - path.wall_time_children);
      data.cpu_times.push_back(path.counter.get_cpu().cumulative_time() - path.cpu_time_children);
    }
  }
  return data;
}

void format_paths(std::list<std::string>& path_names, bool cumulative) {
  size_t max_path_size = 0;
  for (const auto& path_name : path_names)
    if (path_name.size() > max_path_size)
      max_path_size = path_name.size();
  for (auto& path_name : path_names) {
    auto n_blank = max_path_size - path_name.size();
    std::string blank;
    for (size_t i = 0; i < n_blank; ++i)
      blank += " ";
    if (cumulative)
      path_name += blank;
    else
      path_name = blank + path_name;
    path_name += " : ";
  }
}

void write_report(const Profiler& prof, std::ostream& out, const ReportData& data, bool cumulative) {
  bool with_wall = !prof.root->counter.get_wall().dummy();
  bool with_cpu = !prof.root->counter.get_cpu().dummy();
  out << "Profiler " << '"' << prof.description << '"';
  if (cumulative)
    out << " (cumulative) ";
  out << std::endl;
  auto path_name = data.formatted_path_names.begin();
  for (size_t i = 0; i < data.formatted_path_names.size(); ++i, ++path_name) {
    out << *path_name;
    out << "    calls=" << data.calls[i] << ", ";
    if (with_wall)
      out << " wall=" << data.wall_times[i];
    if (with_cpu)
      out << " cpu=" << data.cpu_times[i];
    out << std::endl;
  }
}

ReportData sort_data(const ReportData& data, const SortBy sort_by) {
  struct DataWrapper {
    std::reference_wrapper<const std::string> path_name;
    std::reference_wrapper<const int> depth;
    std::reference_wrapper<const size_t> calls;
    std::reference_wrapper<const double> wall_times;
    std::reference_wrapper<const double> cpu_times;
  };
  auto n = data.formatted_path_names.size();
  auto wdata = std::vector<DataWrapper>{};
  auto it_path = data.formatted_path_names.begin();
  for (size_t i = 0; i < n; ++i, ++it_path)
    wdata.push_back(
        DataWrapper{*it_path, data.depth.at(i), data.calls.at(i), data.wall_times.at(i), data.cpu_times.at(i)});
  struct Compare {
    bool operator()(const DataWrapper& l, const DataWrapper& r) {
      bool result = l.depth < r.depth;
      if (l.depth == r.depth) {
        if (sortBy == SortBy::wall)
          result = l.wall_times > r.wall_times;
        else if (sortBy == SortBy::cpu)
          result = l.cpu_times > r.cpu_times;
        else if (sortBy == SortBy::calls)
          result = l.calls > r.calls;
      }
      return result;
    }
    SortBy sortBy;
  };
  std::sort(begin(wdata), end(wdata), Compare{sort_by});
  auto sorted_data = ReportData{};
  for (const auto& d : wdata) {
    sorted_data.formatted_path_names.emplace_back(d.path_name);
    sorted_data.depth.push_back(d.depth);
    sorted_data.calls.push_back(d.calls);
    sorted_data.wall_times.push_back(d.wall_times);
    sorted_data.cpu_times.push_back(d.cpu_times);
  }
  return sorted_data;
}

} // namespace detail

void report(const Profiler& prof, std::ostream& out, bool cumulative, SortBy sort_by) {
  auto paths = detail::TreePath::convert_subtree_to_paths(prof.root);
  auto data = detail::get_report_data(paths, cumulative);
  data = detail::sort_data(data, sort_by);
  detail::format_paths(data.formatted_path_names, cumulative);
  detail::write_report(prof, out, data, cumulative);
}

#ifdef MOLPRO_PROFILER_MPI
void report(const Profiler& prof, std::ostream& out, MPI_Comm communicator, bool cumulative, SortBy sort_by) {
  auto paths = detail::TreePath::convert_subtree_to_paths(prof.root);
  auto data = detail::get_report_data(paths, cumulative);
  MPI_Request requests[2];
  MPI_Iallreduce(&data.wall_times[0], MPI_IN_PLACE, data.wall_times.size(), MPI_DOUBLE, MPI_SUM, communicator,
                 &requests[0]);
  MPI_Iallreduce(&data.cpu_times[0], MPI_IN_PLACE, data.wall_times.size(), MPI_DOUBLE, MPI_SUM, communicator,
                 &requests[0]);
  MPI_Waitall(2, requests, MPI_STATUSES_IGNORE);
  data = detail::sort_data(data, sort_by);
  detail::format_paths(data.formatted_path_names, cumulative);
  detail::write_report(prof, out, data, cumulative);
}
#endif

} // namespace tree
} // namespace profiler
} // namespace molpro