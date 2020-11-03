#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <molpro/Profiler/Tree/Counter.h>
#include <molpro/Profiler/Tree/Node.h>
#include <molpro/Profiler/Tree/Profiler.h>

using molpro::profiler::tree::Profiler;

TEST(Profiler, constructor) {
  const auto description = "test";
  Profiler p(description);
  ASSERT_EQ(p.description(), description);
  ASSERT_FALSE(p.counter().get_wall().dummy());
  ASSERT_TRUE(p.counter().get_cpu().dummy());
  ASSERT_TRUE(p.root);
  ASSERT_EQ(p.root->parent, nullptr);
  ASSERT_EQ(p.root->name, p.root->name);
  ASSERT_EQ(p.root, p.active_node);
  ASSERT_EQ(p.get_max_depth(), std::numeric_limits<int>::max());
  ASSERT_EQ(p.get_current_depth(), 0);
}

namespace {
void test_start(Profiler& p, const std::string& name) {
  ASSERT_EQ(p.get_current_depth(), 1);
  ASSERT_NE(p.root, p.active_node);
  ASSERT_EQ(p.active_node->parent, p.root);
  ASSERT_EQ(p.active_node->name, name);
}

void test_stop(Profiler& p) {
  ASSERT_EQ(p.get_current_depth(), 0);
  ASSERT_EQ(p.root, p.active_node);
  ASSERT_EQ(p.active_node->parent, nullptr);
}
} // namespace

TEST(Profiler, start_stop) {
  Profiler p("test");
  const auto name = "level 1";
  p.start(name);
  test_start(p, name);
  p.stop();
  test_stop(p);
}

TEST(Profiler, push) {
  Profiler prof("test");
  {
    const auto name = "level 1";
    auto proxy = prof.push(name);
    test_start(prof, name);
  }
  test_stop(prof);
}

TEST(Profiler, addition_assignment) {
  const size_t n_op = 11;
  Profiler prof("test");
  const auto n_init = prof.counter().get_operation_count();
  prof += n_op;
  ASSERT_EQ(prof.counter().get_operation_count(), n_init + n_op);
  prof += n_op;
  ASSERT_EQ(prof.counter().get_operation_count(), n_init + 2 * n_op);
}

TEST(Profiler, pre_increment) {
  Profiler prof("test");
  const auto n_init = prof.counter().get_operation_count();
  ++prof;
  ASSERT_EQ(prof.counter().get_operation_count(), n_init + 1);
  ++prof;
  ASSERT_EQ(prof.counter().get_operation_count(), n_init + 2);
}

TEST(Profiler, post_increment) {
  Profiler prof("test");
  auto n_init = prof.counter().get_operation_count();
  auto nop = prof++;
  ASSERT_EQ(nop, n_init);
  ASSERT_EQ(prof.counter().get_operation_count(), n_init + 1);
  nop = prof++;
  ASSERT_EQ(nop, n_init + 1);
  ASSERT_EQ(prof.counter().get_operation_count(), n_init + 2);
}

TEST(Profiler, Proxy_addition_assignment) {
  const size_t n_op = 11;
  Profiler prof("test");
  {
    const auto name = "level 1";
    auto proxy = prof.push(name);
    const auto n_init = prof.counter().get_operation_count();
    proxy += n_op;
    ASSERT_EQ(prof.counter().get_operation_count(), n_init + n_op);
    proxy += n_op;
    ASSERT_EQ(prof.counter().get_operation_count(), n_init + 2 * n_op);
  }
}

TEST(Profiler, Proxy_pre_increment) {
  Profiler prof("test");
  {
    const auto name = "level 1";
    auto proxy = prof.push(name);
    const auto n_init = prof.counter().get_operation_count();
    ++proxy;
    ASSERT_EQ(prof.counter().get_operation_count(), n_init + 1);
    ++proxy;
    ASSERT_EQ(prof.counter().get_operation_count(), n_init + 2);
  }
}

TEST(Profiler, Proxy_post_increment) {
  Profiler prof("test");
  {
    const auto name = "level 1";
    auto proxy = prof.push(name);
    auto n_init = prof.counter().get_operation_count();
    auto nop = proxy++;
    ASSERT_EQ(nop, n_init);
    ASSERT_EQ(prof.counter().get_operation_count(), n_init + 1);
    nop = proxy++;
    ASSERT_EQ(nop, n_init + 1);
    ASSERT_EQ(prof.counter().get_operation_count(), n_init + 2);
  }
}

namespace {
void construct_stack(Profiler& p, int n) {
  for (size_t i = 0; i < n; ++i)
    p.start("level " + std::to_string(i));
}

int measure_depth(const Profiler& p) {
  int n = 0;
  auto node = p.active_node;
  while (node->parent) {
    node = node->parent;
    ++n;
  }
  return n;
}
} // namespace

TEST(Profiler, start__stop_all__stack) {
  constexpr int depth = 4;
  Profiler p("");
  construct_stack(p, depth);
  ASSERT_EQ(p.get_current_depth(), depth);
  ASSERT_EQ(measure_depth(p), depth);
  p.stop_all();
  test_stop(p);
  ASSERT_EQ(measure_depth(p), 0);
}

TEST(Profiler, max_depth) {
  constexpr int max_depth = 2, depth = max_depth + 2;
  Profiler p("");
  p.set_max_depth(max_depth);
  ASSERT_EQ(p.get_max_depth(), max_depth);
  construct_stack(p, depth);
  ASSERT_EQ(p.get_current_depth(), depth);
  ASSERT_EQ(measure_depth(p), max_depth);
}

TEST(Profiler, counter) {
  Profiler p("");
  ASSERT_NO_FATAL_FAILURE(p.counter());
  auto& c = p.counter();
  ASSERT_FALSE(c.get_wall().stopped());
}

TEST(Profiler, single) {
  auto prof = Profiler::single("test");
  {
    auto prof_single = Profiler::single();
    ASSERT_EQ(prof_single, prof);
  }
}

TEST(Profiler, str) {
  Profiler p("test");
  auto s = p.str();
  ASSERT_FALSE(s.empty());
}

TEST(Profiler, ostream_operator) {
  Profiler p("test");
  std::stringstream os;
  os << p;
  auto s = os.str();
  ASSERT_FALSE(s.empty());
}
