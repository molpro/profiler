#include "Timer.h"

#include <chrono>
#include <ctime>
#include <utility>
namespace molpro {
namespace profiler {
namespace {
using clock_type = typename std::conditional<std::chrono::high_resolution_clock::is_steady,
                                             std::chrono::high_resolution_clock, std::chrono::steady_clock>::type;

const auto global_start_time = clock_type::now();
} // namespace

Timer::Timer(Timer::Type _type, bool is_dummy) : m_type(_type), m_dummy(is_dummy) {}

Timer::Timer(double cumulative_time, Timer::Type type, bool is_dummy)
    : m_type(type), m_cumulative(cumulative_time), m_dummy(is_dummy) {}

Timer& Timer::start() {
  if (not m_dummy && m_stopped) {
    if (m_type == cpu) {
      m_start = double(clock()) / CLOCKS_PER_SEC;
    } else if (m_type == wall) {
      auto now = clock_type::now();
      m_start = std::chrono::duration<double>(now - global_start_time).count();
    }
    m_stopped = false;
  }
  return *this;
}

Timer& Timer::stop() {
  if (not m_stopped) {
    m_stop = Timer{m_type, m_dummy}.start().m_start;
    m_cumulative += m_stop - m_start;
  }
  m_stopped = true;
  return *this;
}

void Timer::operator+=(const Timer& other) { m_cumulative += other.m_cumulative; }

void Timer::reset() {
  auto temp = Timer{m_type, m_dummy};
  using std::swap;
  swap(*this, temp);
}

} // namespace profiler
} // namespace molpro
