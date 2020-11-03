#include "Profiler.h"
#include "molpro/profiler/Counter.h"
#include "molpro/profiler/Node.h"
#include "molpro/profiler/WeakSingleton.h"
#include "molpro/profiler/report.h"

#include <algorithm>
#include <sstream>

namespace molpro {
namespace profiler {
namespace tree {

Profiler::~Profiler() = default;

Profiler::Profiler(std::string description_, const bool with_wall, const bool with_cpu)
    : m_description(std::move(description_)), m_max_depth{std::numeric_limits<int>::max()},
      root(Node<Counter>::make_root(m_root_name, Counter{with_cpu, with_wall})), active_node(root) {
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

void Profiler::operator+=(size_t operations) { counter().add_operations(operations); }

void Profiler::operator++() { counter().add_operations(1); }

size_t Profiler::operator++(int) {
  auto temp = counter().get_operation_count();
  ++(*this);
  return temp;
}

void Profiler::Proxy::operator+=(size_t operations) { prof.counter().add_operations(operations); }

void Profiler::Proxy::operator++() { prof.counter().add_operations(1); }

size_t Profiler::Proxy::operator++(int) {
  auto temp = prof.counter().get_operation_count();
  ++(*this);
  return temp;
}

std::string Profiler::str(bool cumulative, SortBy sort_by) const {
  std::stringstream out;
  report(*this, out, cumulative, sort_by);
  return out.str();
}

#ifdef MOLPRO_PROFILER_MPI
std::string Profiler::str(MPI_Comm communicator, bool cumulative, SortBy sort_by) const {
  std::stringstream out;
  report(*this, out, communicator, cumulative, sort_by);
  return out.str();
}
#endif

std::ostream& operator<<(std::ostream& os, const Profiler& obj) {
  os << obj.str() << std::endl;
  return os;
}
Profiler& Profiler::reset(const std::string& name) {
  auto temp = Profiler(name, !root->counter.get_wall().dummy(), !root->counter.get_cpu().dummy());
  using std::swap;
  swap(*this, temp);
  return *this;
}

} // namespace tree
} // namespace profiler
} // namespace molpro
