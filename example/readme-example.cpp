#include <chrono>
#include <iostream>
#include <molpro/Profiler.h>
#include <thread>
#include <cmath>
using molpro::Profiler;

void sleep() { std::this_thread::sleep_for(std::chrono::milliseconds{20}); }
void read_input(Profiler& p) {
  p.start("read_input()");
  sleep();
  p.stop();
}
void restore_backup(Profiler& p) {
  p.start("restore_backup()");
  sleep();
  p.stop();
}
void operation1() { sleep(); }
void operation2() { sleep(); }

int main() {
  auto prof = Profiler{"main()"};
  sleep();
  {
    auto p = prof.push("initialise()");
    sleep();
    read_input(prof);
    restore_backup(prof);
  }
  prof.start("perform_calculation()");
  sleep();
  for (size_t container = 0; container < 2; ++container)
    for (size_t i = 0; i < 2; ++i) {
      auto ps = prof.push("container " + std::to_string(container));
      {
        auto p = prof.push("operation1()");
        operation1();
        p += std::pow(10,container);
      }
      {
        auto p = prof.push("operation2()");
        operation2();
        p += std::pow(100,container);
      }
    }
  prof.stop();
  std::cout << prof << std::endl;
  std::cout << prof.str(false) << std::endl;
  prof.dotgraph("readme-example.gv");
  prof.dotgraph("readme-example.noncumulative.gv", 0.01, false);
  return 0;
}
