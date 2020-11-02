#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <molpro/Profiler/Tree/Counter.h>
#include <molpro/Profiler/Tree/Node.h>
#include <molpro/Profiler/Tree/report.h>

#include <sstream>

using molpro::profiler::tree::Counter;
using molpro::profiler::tree::Node;
using molpro::profiler::tree::Profiler;
using molpro::profiler::tree::report;
using molpro::profiler::tree::detail::format_path_cumulative;
using molpro::profiler::tree::detail::format_path_not_cumulative;
using molpro::profiler::tree::detail::TreePath;

struct TreePath_Fixture : ::testing::Test {
  TreePath_Fixture() : prof("TreePath_Fixture") {
    prof.push("A").push("B").push("C");
    prof.stop();
    a = prof.root->children.at("A");
    b = a->children.at("B");
    c = b->children.at("C");
  }

  Profiler prof;
  std::shared_ptr<Node<Counter>> a, b, c;
};

TEST_F(TreePath_Fixture, constructor) {
  auto tp = TreePath(c);
  ASSERT_EQ(tp.counter.get_call_count(), c->counter.get_call_count());
  ASSERT_EQ(tp.counter.get_wall().cumulative_time(), c->counter.get_wall().cumulative_time());
  ASSERT_EQ(tp.counter.get_cpu().cumulative_time(), c->counter.get_cpu().cumulative_time());
  ASSERT_EQ(tp.path.size(), 4);
  auto ref = std::list<std::string>{{"All"}, {"A"}, {"B"}, {"C"}};
  ASSERT_THAT(tp.path, ::testing::Pointwise(::testing::Eq(), ref));
}

TEST_F(TreePath_Fixture, convert_subtree_to_paths) {
  auto paths = TreePath::convert_subtree_to_paths(prof.root);
  auto reference_paths = std::list<TreePath>{TreePath{prof.root}, TreePath{a}, TreePath{b}, TreePath{c}};
  ASSERT_EQ(paths.size(), reference_paths.size());
  auto it_path = paths.begin();
  auto it_ref_path = reference_paths.begin();
  for (size_t i = 0; i < paths.size(); ++i, ++it_path, ++it_ref_path)
    ASSERT_THAT(it_path->path, ::testing::Pointwise(::testing::Eq(), it_ref_path->path));
}

TEST_F(TreePath_Fixture, format_path_cumulative) {
  auto tp = TreePath(c);
  auto s = format_path_cumulative(tp.path);
  auto ref = std::string{"...C"};
  ASSERT_FALSE(s.empty());
  ASSERT_EQ(s, ref);
}

TEST_F(TreePath_Fixture, format_path_not_cumulative) {
  auto tp = TreePath(c);
  auto s = format_path_not_cumulative(tp.path);
  auto ref = std::string{"All:A:B:C"};
  ASSERT_FALSE(s.empty());
  ASSERT_EQ(s, ref);
}

TEST_F(TreePath_Fixture, report) {
  std::stringstream out;
  report(prof, out);
  ASSERT_FALSE(out.str().empty());
}