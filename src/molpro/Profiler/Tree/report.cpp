#include "report.h"
namespace molpro {
namespace profiler {
namespace tree {

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

void report(Profiler& prof) {
  struct Compare {
    bool operator()(const Counter& x, const Counter& y) const {
      return x.get_wall().cumulative_time() > y.get_wall().cumulative_time();
    }
  };
  auto paths = std::map<Counter, std::string, Compare>{};
  extract_counters(prof.root, paths);
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