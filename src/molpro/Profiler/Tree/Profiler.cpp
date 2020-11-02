#include "Profiler.h"
#include "Counter.h"
#include "Node.h"
#include "WeakSingleton.h"
#include "report.h"

#include <algorithm>
#include <sstream>

namespace molpro {
namespace profiler {
namespace tree {

Profiler::~Profiler() = default;

Profiler::Profiler(std::string description_, const bool with_wall, const bool with_cpu)
    : description(std::move(description_)), root(Node<Counter>::make_root(root_name, Counter{with_cpu, with_wall})),
      active_node(root), m_max_depth{std::numeric_limits<int>::max()} {
  root->counter.start();
}

template <>
std::list<std::pair<std::string, std::weak_ptr<Profiler>>> WeakSingleton<Profiler>::m_register = {};

std::shared_ptr<Profiler> Profiler::single(const std::string& description_, bool with_wall, bool with_cpu) {
  return WeakSingleton<Profiler>::single(description_, description_, with_wall, with_cpu);
}

std::shared_ptr<Profiler> Profiler::single() { return WeakSingleton<Profiler>::single(); }

int Profiler::get_max_depth() const { return m_max_depth; };
void Profiler::set_max_depth(int new_max_depth) { m_max_depth = new_max_depth; };
int Profiler::get_current_depth() const { return m_current_depth; }

Profiler& Profiler::start(const std::string& name) {
  if (m_current_depth < m_max_depth) {
    auto ch = active_node->children.find(name);
    if (ch == active_node->children.end()) {
      auto count = Counter{!root->counter.get_cpu().dummy(), !root->counter.get_wall().dummy()};
      count.start();
      active_node = Node<Counter>::add_child(name, count, active_node);
    } else {
      ch->second->counter.start();
      active_node = ch->second;
    }
  }
  ++m_current_depth;
  return *this;
}

Profiler& Profiler::stop() {
  if (m_current_depth <= m_max_depth) {
    active_node->counter.stop();
    if (active_node->parent) {
      active_node = active_node->parent;
      --m_current_depth;
    }
  } else
    --m_current_depth;
  return *this;
}

Profiler& Profiler::stop_all() {
  while (active_node != root)
    stop();
  stop();
  return *this;
}

Counter& Profiler::counter() { return active_node->counter; }

std::string Profiler::str(bool cumulative) const {
  std::stringstream out;
  report(*this, out, cumulative);
  return out.str();
}

#ifdef MOLPRO_PROFILER_MPI
std::string Profiler::str(MPI_Comm communicator, bool cumulative) const {
  std::stringstream out;
  report(*this, out, communicator, cumulative);
  return out.str();
}
#endif

std::ostream& operator<<(std::ostream& os, const Profiler& obj) {
  os << obj.str() << std::endl;
  return os;
}

} // namespace tree
} // namespace profiler
} // namespace molpro
