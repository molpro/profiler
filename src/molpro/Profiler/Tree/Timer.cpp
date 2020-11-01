#include "Timer.h"

#include <chrono>
#include <ctime>
namespace molpro {
namespace profiler {
namespace tree {
namespace {
using clock_type = typename std::conditional<std::chrono::high_resolution_clock::is_steady,
                                             std::chrono::high_resolution_clock, std::chrono::steady_clock>::type;

const auto global_start_time = clock_type::now();
} // namespace

Timer::Timer(Timer::Type type, bool is_dummy) : type(type), m_dummy(is_dummy) {}

Timer& Timer::start() {
  if (not m_dummy && m_start == 0) {
    if (type == cpu) {
      m_start = double(clock()) / CLOCKS_PER_SEC;
    } else if (type == wall) {
      auto now = clock_type::now();
      m_start = std::chrono::duration<double>(now - global_start_time).count();
    }
    m_stopped = false;
  }
  return *this;
}

Timer& Timer::stop() {
  if (not m_stopped) {
    m_stop = Timer{type, m_dummy}.start().m_start;
    m_cumulative += m_stop - m_start;
    m_stop = m_cumulative;
    m_start = 0;
  }
  m_stopped = true;
  return *this;
}

void Timer::operator+=(const Timer& other) { m_cumulative += other.m_cumulative; }

} // namespace tree
} // namespace profiler

} // namespace molpro
