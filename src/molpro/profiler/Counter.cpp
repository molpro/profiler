#include "Counter.h"
namespace molpro {
namespace profiler {

Counter& Counter::start() {
  ++call_count;
  cpu.start();
  wall.start();
  return *this;
}

Counter& Counter::stop() {
  cpu.stop();
  wall.stop();
  return *this;
}

void Counter::operator+=(const Counter& other) {
  call_count += other.call_count;
  operation_count += other.operation_count;
  cpu += other.cpu;
  wall += other.wall;
}

Counter& Counter::reset() {
  *this = Counter(!cpu.dummy(), !wall.dummy());
  return *this;
}

Counter::Counter(bool with_cpu_time, bool with_wall_time)
    : cpu(Timer{Timer::cpu, !with_cpu_time}), wall(Timer{Timer::wall, !with_wall_time}) {}

Counter::Counter(size_t call_count_, size_t operation_count_, double wall_time_, double cpu_time_, bool with_cpu_time,
                 bool with_wall_time)
    : call_count(call_count_), operation_count(operation_count_), cpu(Timer{cpu_time_, Timer::cpu, !with_cpu_time}),
      wall(Timer{wall_time_, Timer::wall, !with_wall_time}) {}

} // namespace profiler
} // namespace molpro
