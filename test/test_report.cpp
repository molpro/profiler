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
using molpro::profiler::tree::SortBy;
using molpro::profiler::tree::detail::format_path_cumulative;
using molpro::profiler::tree::detail::format_path_not_cumulative;
using molpro::profiler::tree::detail::format_paths;
using molpro::profiler::tree::detail::ReportData;
using molpro::profiler::tree::detail::sort_data;
using molpro::profiler::tree::detail::total_operation_count;
using molpro::profiler::tree::detail::TreePath;
using molpro::profiler::tree::detail::write_report;

struct TreePath_Fixture : ::testing::Test {
  TreePath_Fixture() : prof("TreePath_Fixture") {
    prof.push("A").push("B").push("C");
    for (auto key : {"A", "B", "C"}) {
      prof.start(key);
      ++prof;
    }
    prof.stop_all();
    a = prof.root->children.at("A");
    b = a->children.at("B");
    c = b->children.at("C");
  }

  Profiler prof;
  std::shared_ptr<Node<Counter>> a, b, c;
};

TEST_F(TreePath_Fixture, constructor__cumulative) {
  auto tp = TreePath(b, true);
  ASSERT_EQ(tp.counter.get_call_count(), b->counter.get_call_count());
  ASSERT_EQ(tp.counter.get_wall().cumulative_time(), b->counter.get_wall().cumulative_time());
  ASSERT_EQ(tp.counter.get_cpu().cumulative_time(), b->counter.get_cpu().cumulative_time());
  ASSERT_EQ(tp.path.size(), 3);
  auto ref = std::list<std::string>{{"All"}, {"A"}, {"B"}};
  ASSERT_THAT(tp.path, ::testing::Pointwise(::testing::Eq(), ref));
  ASSERT_EQ(tp.counter.get_operation_count(), 2);
}

TEST_F(TreePath_Fixture, constructor__not_cumulative) {
  auto tp = TreePath(b, false);
  ASSERT_EQ(tp.counter.get_call_count(), b->counter.get_call_count());
  ASSERT_DOUBLE_EQ(tp.counter.get_wall().cumulative_time(),
                   b->counter.get_wall().cumulative_time() - c->counter.get_wall().cumulative_time());
  ASSERT_DOUBLE_EQ(tp.counter.get_cpu().cumulative_time(),
                   b->counter.get_cpu().cumulative_time() - c->counter.get_cpu().cumulative_time());
  ASSERT_EQ(tp.path.size(), 3);
  auto ref = std::list<std::string>{{"All"}, {"A"}, {"B"}};
  ASSERT_THAT(tp.path, ::testing::Pointwise(::testing::Eq(), ref));
  ASSERT_EQ(tp.counter.get_operation_count(), 1);
}

TEST_F(TreePath_Fixture, convert_subtree_to_paths) {
  auto paths = TreePath::convert_tree_to_paths(prof.root, true, SortBy::wall);
  auto reference_paths =
      std::list<TreePath>{TreePath{prof.root, true}, TreePath{a, true}, TreePath{b, true}, TreePath{c, true}};
  ASSERT_EQ(paths.size(), reference_paths.size());
  auto it_path = paths.begin();
  auto it_ref_path = reference_paths.begin();
  for (size_t i = 0; i < paths.size(); ++i, ++it_path, ++it_ref_path)
    ASSERT_THAT(it_path->path, ::testing::Pointwise(::testing::Eq(), it_ref_path->path));
}

TEST_F(TreePath_Fixture, format_path_cumulative) {
  auto tp = TreePath(c, true);
  auto s = format_path_cumulative(tp.path);
  auto ref = std::string{"...C"};
  ASSERT_FALSE(s.empty());
  ASSERT_EQ(s, ref);
}

TEST_F(TreePath_Fixture, format_path_not_cumulative) {
  auto tp = TreePath(c, true);
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

TEST(report_detail, format_paths__cumulative) {
  auto path_names = std::list<std::string>{{"A"}, {"**B"}, {"*C"}};
  format_paths(path_names, true);
  auto reference = std::list<std::string>{{"A   : "}, {"**B : "}, {"*C  : "}};
  ASSERT_EQ(path_names, reference);
}

TEST(report_detail, format_paths__not_cumulative) {
  auto path_names = std::list<std::string>{{"A"}, {"**B"}, {"*C"}};
  format_paths(path_names, false);
  auto reference = std::list<std::string>{{"  A : "}, {"**B : "}, {" *C : "}};
  ASSERT_EQ(path_names, reference);
}

TEST(report_detail, sort_data__wall) {
  auto data = ReportData{{{"A"}, {"B"}, {"C"}, {"D"}},     // names
                         {1, 0, 2, 1},                     // depth
                         {0, 0, 0, 0},                     // calls
                         {0, 0, 0, 0},                     // operations
                         {5.1, 6, 7, 5.2},                 // wall
                         {0, 0, 0, 0}};                    // cpu
  auto ref_data = ReportData{{{"B"}, {"D"}, {"A"}, {"C"}}, // names
                             {0, 1, 1, 2},                 // depth
                             {0, 0, 0, 0},                 // calls
                             {0, 0, 0, 0},                 // operations
                             {6, 5.2, 5.1, 7},             // wall
                             {0, 0, 0, 0}};                // cpu
  auto sorted_data = sort_data(data, SortBy::wall);
  ASSERT_EQ(sorted_data.formatted_path_names, ref_data.formatted_path_names);
  ASSERT_EQ(sorted_data.depth, ref_data.depth);
  ASSERT_EQ(sorted_data.calls, ref_data.calls);
  ASSERT_EQ(sorted_data.wall_times, ref_data.wall_times);
  ASSERT_EQ(sorted_data.cpu_times, ref_data.cpu_times);
}

TEST(report_detail, sort_data__cpu) {
  auto data = ReportData{{{"A"}, {"B"}, {"C"}, {"D"}},     // names
                         {1, 0, 2, 1},                     // depth
                         {0, 0, 0, 0},                     // calls
                         {0, 0, 0, 0},                     // operations
                         {0, 0, 0, 0},                     // wall
                         {5.1, 6, 7, 5.2}};                // cpu
  auto ref_data = ReportData{{{"B"}, {"D"}, {"A"}, {"C"}}, // names
                             {0, 1, 1, 2},                 // depth
                             {0, 0, 0, 0},                 // calls
                             {0, 0, 0, 0},                 // operations
                             {0, 0, 0, 0},                 // wall
                             {6, 5.2, 5.1, 7}};            // cpu
  auto sorted_data = sort_data(data, SortBy::cpu);
  ASSERT_EQ(sorted_data.formatted_path_names, ref_data.formatted_path_names);
  ASSERT_EQ(sorted_data.depth, ref_data.depth);
  ASSERT_EQ(sorted_data.calls, ref_data.calls);
  ASSERT_EQ(sorted_data.wall_times, ref_data.wall_times);
  ASSERT_EQ(sorted_data.cpu_times, ref_data.cpu_times);
}

TEST(report_detail, sort_data__calls) {
  auto data = ReportData{{{"A"}, {"B"}, {"C"}, {"D"}},     // names
                         {1, 0, 2, 1},                     // depth
                         {1, 2, 3, 4},                     // calls
                         {0, 0, 0, 0},                     // operations
                         {0, 0, 0, 0},                     // wall
                         {0, 0, 0, 0}};                    // cpu
  auto ref_data = ReportData{{{"B"}, {"D"}, {"A"}, {"C"}}, // names
                             {0, 1, 1, 2},                 // depth
                             {2, 4, 1, 3},                 // calls
                             {0, 0, 0, 0},                 // operations
                             {0, 0, 0, 0},                 // wall
                             {0, 0, 0, 0}};                // cpu
  auto sorted_data = sort_data(data, SortBy::calls);
  ASSERT_EQ(sorted_data.formatted_path_names, ref_data.formatted_path_names);
  ASSERT_EQ(sorted_data.depth, ref_data.depth);
  ASSERT_EQ(sorted_data.calls, ref_data.calls);
  ASSERT_EQ(sorted_data.wall_times, ref_data.wall_times);
  ASSERT_EQ(sorted_data.cpu_times, ref_data.cpu_times);
}

TEST(report_detail, sort_data__operation_count) {
  auto data = ReportData{{{"A"}, {"B"}, {"C"}, {"D"}},     // names
                         {1, 0, 2, 1},                     // depth
                         {0, 0, 0, 0},                     // calls
                         {1, 2, 3, 4},                     // operations
                         {0, 0, 0, 0},                     // wall
                         {0, 0, 0, 0}};                    // cpu
  auto ref_data = ReportData{{{"B"}, {"D"}, {"A"}, {"C"}}, // names
                             {0, 1, 1, 2},                 // depth
                             {0, 0, 0, 0},                 // calls
                             {2, 4, 1, 3},                 // operations
                             {0, 0, 0, 0},                 // wall
                             {0, 0, 0, 0}};                // cpu
  auto sorted_data = sort_data(data, SortBy::operations);
  ASSERT_EQ(sorted_data.formatted_path_names, ref_data.formatted_path_names);
  ASSERT_EQ(sorted_data.depth, ref_data.depth);
  ASSERT_EQ(sorted_data.calls, ref_data.calls);
  ASSERT_EQ(sorted_data.wall_times, ref_data.wall_times);
  ASSERT_EQ(sorted_data.cpu_times, ref_data.cpu_times);
}

TEST(report_detail, total_operation_count__nullptr) {
  auto tot_count = total_operation_count(nullptr);
  ASSERT_EQ(tot_count, 0);
}

TEST(report_detail, total_operation_count__root_only) {
  const size_t n_op = 3;
  Profiler p("test");
  p += n_op;
  auto tot_count = total_operation_count(p.root);
  ASSERT_EQ(tot_count, n_op);
}

TEST(report_detail, total_operation_count__tree) {
  const size_t n_op = 3;
  Profiler p("test");
  p += n_op;
  for (auto key : {"A", "B"}) {
    auto p1 = p.push(key);
    p1 += n_op;
    for (auto key2 : {"C", "D"}) {
      auto p2 = p.push(key2);
      p2 += n_op;
    }
  }
  auto tot_count = total_operation_count(p.root);
  ASSERT_EQ(tot_count, 7 * n_op);
}
