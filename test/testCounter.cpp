#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <molpro/profiler/Counter.h>

using molpro::profiler::tree::Counter;
using molpro::profiler::tree::Timer;

TEST(Counter, constructor_default) {
  auto c = Counter();
  ASSERT_EQ(c.get_call_count(), 0);
  ASSERT_EQ(c.get_operation_count(), 0);
  ASSERT_EQ(c.get_cpu().type(), Timer::Type::cpu);
  ASSERT_EQ(c.get_wall().type(), Timer::Type::wall);
  ASSERT_TRUE(c.get_cpu().dummy());
  ASSERT_TRUE(c.get_wall().dummy());
}

TEST(Counter, constructor) {
  auto check = [](bool cpu, bool wall) {
    auto c = Counter(cpu, wall);
    ASSERT_EQ(c.get_cpu().dummy(), !cpu);
    ASSERT_EQ(c.get_wall().dummy(), !wall);
  };
  check(true, true);
  check(false, true);
  check(true, false);
  check(false, false);
}

TEST(Counter, constructor_with_initial_values) {
  size_t initial_call_count = 3;
  size_t initial_operation_count = 7;
  double initial_wall_time = 11.2;
  double initial_cpu_time = 0.3;
  auto c = Counter(initial_call_count, initial_operation_count, initial_wall_time, initial_cpu_time, false, false);
  ASSERT_EQ(c.get_call_count(), initial_call_count);
  ASSERT_EQ(c.get_operation_count(), initial_operation_count);
  ASSERT_EQ(c.get_wall().cumulative_time(), initial_wall_time);
  ASSERT_EQ(c.get_cpu().cumulative_time(), initial_cpu_time);
  ASSERT_EQ(c.get_cpu().type(), Timer::Type::cpu);
  ASSERT_EQ(c.get_wall().type(), Timer::Type::wall);
  ASSERT_TRUE(c.get_cpu().dummy());
  ASSERT_TRUE(c.get_wall().dummy());
}

TEST(Counter, stop) {
  auto c = Counter();
  c.stop();
  ASSERT_EQ(c.get_call_count(), 0);
}

TEST(Counter, start_stop) {
  auto c = Counter();
  c.start();
  ASSERT_EQ(c.get_call_count(), 1);
  c.start();
  ASSERT_EQ(c.get_call_count(), 2);
  c.start();
  ASSERT_EQ(c.get_call_count(), 3);
  c.stop();
  ASSERT_EQ(c.get_call_count(), 3);
}

TEST(Counter, reset) {
  auto c = Counter(true, true);
  c.start();
  c.stop();
  ASSERT_NE(c.get_call_count(), 0);
  c.reset();
  ASSERT_EQ(c.get_call_count(), 0);
  ASSERT_EQ(c.get_operation_count(), 0);
  ASSERT_TRUE(c.get_wall().stopped());
  ASSERT_TRUE(c.get_cpu().stopped());
  ASSERT_EQ(c.get_wall().cumulative_time(), 0);
  ASSERT_EQ(c.get_cpu().cumulative_time(), 0);
}
