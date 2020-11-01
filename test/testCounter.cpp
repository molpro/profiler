#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <molpro/Profiler/Tree/Counter.h>

using molpro::profiler::tree::Counter;
using molpro::profiler::tree::Timer;

TEST(profiler_tree, Counter_constructor_default) {
  auto c = Counter();
  ASSERT_EQ(c.call_count, 0);
  ASSERT_EQ(c.operation_count, 0);
  ASSERT_EQ(c.cpu.m_type, Timer::Type::cpu);
  ASSERT_EQ(c.wall.m_type, Timer::Type::wall);
  ASSERT_TRUE(c.cpu.dummy());
  ASSERT_TRUE(c.wall.dummy());
}

TEST(profiler_tree, Counter_constructor) {
  auto check = [](bool cpu, bool wall) {
    auto c = Counter(cpu, wall);
    ASSERT_EQ(c.cpu.dummy(), !cpu);
    ASSERT_EQ(c.wall.dummy(), !wall);
  };
  check(true, true);
  check(false, true);
  check(true, false);
  check(false, false);
}

TEST(profiler_tree, Counter_stop) {
  auto c = Counter();
  c.stop();
  ASSERT_EQ(c.call_count, 0);
}

TEST(profiler_tree, Counter_start_stop) {
  auto c = Counter();
  c.start();
  ASSERT_EQ(c.call_count, 1);
  c.start();
  ASSERT_EQ(c.call_count, 2);
  c.start();
  ASSERT_EQ(c.call_count, 3);
  c.stop();
  ASSERT_EQ(c.call_count, 3);
}
