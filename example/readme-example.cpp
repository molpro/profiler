#include <chrono>
#include <iostream>
#include <molpro/Profiler.h>
#include <thread>
using molpro::profiler::tree::Profiler;

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
  for (size_t i = 0; i < 2; ++i) {
    {
      auto p = prof.push("operation1()");
      operation1();
    }
    {
      auto p = prof.push("operation2()");
      operation2();
    }
  }
  prof.stop();
  std::cout << prof << std::endl;
  std::cout << prof.str(false) << std::endl;
  return 0;
}
