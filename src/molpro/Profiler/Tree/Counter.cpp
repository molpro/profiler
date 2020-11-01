#include "Counter.h"
namespace molpro {
namespace profiler {
namespace tree {

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

} // namespace tree
} // namespace profiler
} // namespace molpro
