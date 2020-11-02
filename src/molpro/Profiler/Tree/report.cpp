#include "report.h"

#include <iostream>
#include <queue>

namespace molpro {
namespace profiler {
namespace tree {

TreePath::TreePath(std::shared_ptr<Node<Counter>> node) {
  if (node)
    counter = node->counter;
  while (node) {
    path.push_front(node->name);
    node = node->parent;
  }
}
std::string TreePath::format_path() const {
  auto result = std::string();
  for (size_t i = 1; i < path.size(); ++i) {
    result += ".";
  }
  result += path.back();
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

void report(Profiler& prof) {
  auto paths = TreePath::convert_subtree_to_paths(prof.root);
  bool with_wall = !prof.root->counter.get_wall().dummy();
  bool with_cpu = !prof.root->counter.get_cpu().dummy();
  std::list<std::string> formatted_path_names;
  std::vector<size_t> calls;
  std::vector<double> cpu_times, wall_times;
  for (const auto& path : paths) {
    formatted_path_names.emplace_back(path.format_path());
    calls.push_back(path.counter.get_call_count());
    wall_times.push_back(path.counter.get_wall().cumulative_time());
    cpu_times.push_back(path.counter.get_cpu().cumulative_time());
  }
  size_t max_path_size = 0;
  for (const auto& path_name : formatted_path_names)
    if (path_name.size() > max_path_size)
      max_path_size = path_name.size();
  for (auto& path_name : formatted_path_names) {
    auto n_blank = max_path_size - path_name.size();
    for (size_t i = 0; i < n_blank; ++i)
      path_name += " ";
    path_name += " : ";
  }
  std::cout << "Profiler " << '"' << prof.description << '"' << " cumulative " << std::endl;
  auto path_name = formatted_path_names.begin();
  for (size_t i = 0; i < formatted_path_names.size(); ++i, ++path_name) {
    std::cout << *path_name;
    std::cout << "    calls=" << calls[i] << ", ";
    if (with_wall)
      std::cout << " wall=" << wall_times[i];
    if (with_cpu)
      std::cout << " cpu=" << cpu_times[i];
    std::cout << std::endl;
  }
}

} // namespace tree
} // namespace profiler
} // namespace molpro