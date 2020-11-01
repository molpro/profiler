#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <molpro/Profiler/Tree/Counter.h>

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
