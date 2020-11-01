#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <chrono>
#include <molpro/Profiler/Tree/Timer.h>
#include <thread>

using molpro::profiler::tree::Timer;
using namespace std::literals::chrono_literals;

TEST(Timer, constructor) {
  for (auto type : {Timer::Type::cpu, Timer::Type::wall}) {
    for (auto dummy : {true, false}) {
      auto t = Timer(type, dummy);
      ASSERT_EQ(t.type, type);
      ASSERT_EQ(t.dummy(), dummy);
      ASSERT_TRUE(t.stopped());
      ASSERT_EQ(t.start_time(), t.stop_time());
      ASSERT_EQ(t.cumulative_time(), 0);
    }
  }
}

TEST(Timer, start__repeated) {
  auto t = Timer(Timer::Type::wall, false);
  t.start();
  const auto init_time = t.start_time();
  std::this_thread::sleep_for(10ms);
  t.start();
  ASSERT_EQ(t.start_time(), init_time);
  std::this_thread::sleep_for(10ms);
  t.start();
  ASSERT_EQ(t.start_time(), init_time);
  t.stop();
  ASSERT_NE(t.stop_time(), 0);
  ASSERT_NE(t.cumulative_time(), 0);
}

TEST(Timer, start_stop) {
  const bool not_dummy = false;
  constexpr auto delay = 1ms;
  for (auto type : {Timer::Type::wall, Timer::Type::cpu}) {
    auto t = Timer(type, not_dummy);
    t.start();
    std::this_thread::sleep_for(delay);
    t.stop();
    ASSERT_TRUE(t.stopped());
    ASSERT_NE(t.start_time(), t.stop_time());
    ASSERT_NE(t.cumulative_time(), 0);
  }
}

TEST(Timer, start_stop_dummy) {
  const bool is_dummy = true;
  constexpr auto delay = 1ms;
  for (auto type : {Timer::Type::wall, Timer::Type::cpu}) {
    auto t = Timer(type, is_dummy);
    t.start();
    std::this_thread::sleep_for(delay);
    t.stop();
    ASSERT_TRUE(t.stopped());
    ASSERT_EQ(t.start_time(), t.stop_time());
    ASSERT_EQ(t.cumulative_time(), 0);
  }
}
