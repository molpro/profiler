#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <chrono>
#include <molpro/profiler/Timer.h>
#include <thread>

using molpro::profiler::Timer;

TEST(Timer, constructor) {
  for (auto type : {Timer::Type::cpu, Timer::Type::wall}) {
    for (auto dummy : {true, false}) {
      auto t = Timer(type, dummy);
      ASSERT_EQ(t.type(), type);
      ASSERT_EQ(t.dummy(), dummy);
      ASSERT_TRUE(t.stopped());
      ASSERT_EQ(t.start_time(), t.stop_time());
      ASSERT_EQ(t.cumulative_time(), 0);
    }
  }
}

TEST(Timer, constructor__with_initial_time) {
  const double initial_time = 11.5;
  for (auto type : {Timer::Type::cpu, Timer::Type::wall}) {
    for (auto dummy : {true, false}) {
      auto t = Timer(initial_time, type, dummy);
      ASSERT_EQ(t.type(), type);
      ASSERT_EQ(t.dummy(), dummy);
      ASSERT_TRUE(t.stopped());
      ASSERT_EQ(t.start_time(), t.stop_time());
      ASSERT_EQ(t.cumulative_time(), initial_time);
    }
  }
}

TEST(Timer, start__repeated) {
  const auto delay = std::chrono::milliseconds{10};
  auto t = Timer(Timer::Type::wall, false);
  t.start();
  const auto init_time = t.start_time();
  std::this_thread::sleep_for(delay);
  t.start();
  ASSERT_EQ(t.start_time(), init_time);
  std::this_thread::sleep_for(delay);
  t.start();
  ASSERT_EQ(t.start_time(), init_time);
  t.stop();
  ASSERT_NE(t.stop_time(), 0);
  ASSERT_NE(t.cumulative_time(), 0);
}

TEST(Timer, start_stop) {
  const bool not_dummy = false;
  constexpr auto delay = std::chrono::milliseconds{1};
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
  constexpr auto delay = std::chrono::milliseconds{1};
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

TEST(Timer, reset) {
  for (auto type : {Timer::Type::wall, Timer::Type::cpu}) {
    for (auto dummy : {true, false}) {
      auto temp = Timer(type, dummy);
      auto t = Timer(type, dummy);
      t.start();
      t.reset();
      ASSERT_EQ(t.stopped(), temp.stopped());
      ASSERT_EQ(t.start_time(), temp.start_time());
      ASSERT_EQ(t.cumulative_time(), temp.cumulative_time());
      ASSERT_EQ(t.stopped(), temp.stopped());
    }
  }
}

TEST(Timer, cumulative_time){
  constexpr auto delay = std::chrono::milliseconds{5};
  auto t = Timer(Timer::wall, false);
  t.start();
  std::this_thread::sleep_for(delay);
  auto cumulative_time = t.cumulative_time();
  ASSERT_NE(cumulative_time, 0);
  std::this_thread::sleep_for(delay);
  auto cumulative_time2 = t.cumulative_time();
  ASSERT_GT(cumulative_time2, cumulative_time);
  std::this_thread::sleep_for(delay);
  t.stop();
  auto cumulative_time3 = t.cumulative_time();
  ASSERT_GT(cumulative_time3, cumulative_time2);
}