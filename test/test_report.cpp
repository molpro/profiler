#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <molpro/profiler/Counter.h>
#include <molpro/profiler/Node.h>
#include <molpro/profiler/dotgraph.h>
#include <molpro/profiler/report.h>

#include <sstream>
#include <string>

using molpro::profiler::Counter;
using molpro::profiler::Node;
using molpro::profiler::Profiler;
using molpro::profiler::report;
using molpro::profiler::SortBy;
using molpro::profiler::detail::format_path_cumulative;
using molpro::profiler::detail::format_path_not_cumulative;
using molpro::profiler::detail::format_paths;
using molpro::profiler::detail::path_to_node;
using molpro::profiler::detail::total_operation_count;
using molpro::profiler::detail::TreePath;

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

TEST(TreePath, convert_tree_to_paths__all_equal) {
  const bool with_wall = false, with_cpu = false;
  auto root = Node<Counter>::make_root("All", Counter(0, 0, 0, 0, with_wall, with_cpu));
  auto a = Node<Counter>::add_child("A", Counter(0, 0, 0, 0, with_wall, with_cpu), root);
  auto b = Node<Counter>::add_child("B", Counter(0, 0, 0, 0, with_wall, with_cpu), root);
  auto aa = Node<Counter>::add_child("AA", Counter(0, 0, 0, 0, with_wall, with_cpu), a);
  auto ab = Node<Counter>::add_child("AB", Counter(0, 0, 0, 0, with_wall, with_cpu), a);
  auto ba = Node<Counter>::add_child("BA", Counter(0, 0, 0, 0, with_wall, with_cpu), b);
  auto bb = Node<Counter>::add_child("BB", Counter(0, 0, 0, 0, with_wall, with_cpu), b);
  auto tree_paths = TreePath::convert_tree_to_paths(root, true, SortBy::wall);
  auto paths = std::vector<std::list<std::string>>{};
  for (const auto& p : tree_paths)
    paths.emplace_back(p.path);
  auto reference_paths = std::vector<std::list<std::string>>{};
  for (const auto& node : {root, a, aa, ab, b, ba, bb})
    reference_paths.emplace_back(path_to_node(node));
  ASSERT_EQ(paths, reference_paths);
}

TEST(TreePath, convert_tree_to_paths__sort_by_wall) {
  const bool with_wall = false, with_cpu = false;
  auto root = Node<Counter>::make_root("All", Counter(0, 0, 0, 0, with_wall, with_cpu));
  auto a = Node<Counter>::add_child("A", Counter(0, 0, 1, 0, with_wall, with_cpu), root);
  auto b = Node<Counter>::add_child("B", Counter(0, 0, 0, 0, with_wall, with_cpu), root);
  auto aa = Node<Counter>::add_child("AA", Counter(0, 0, 1, 0, with_wall, with_cpu), a);
  auto ab = Node<Counter>::add_child("AB", Counter(0, 0, 0, 0, with_wall, with_cpu), a);
  auto ba = Node<Counter>::add_child("BA", Counter(0, 0, 0, 0, with_wall, with_cpu), b);
  auto bb = Node<Counter>::add_child("BB", Counter(0, 0, 1, 0, with_wall, with_cpu), b);
  auto tree_paths = TreePath::convert_tree_to_paths(root, true, SortBy::wall);
  auto paths = std::vector<std::list<std::string>>{};
  for (const auto& p : tree_paths)
    paths.emplace_back(p.path);
  auto reference_paths = std::vector<std::list<std::string>>{};
  for (const auto& node : {root, a, aa, ab, b, bb, ba})
    reference_paths.emplace_back(path_to_node(node));
  ASSERT_EQ(paths, reference_paths);
}

TEST(TreePath, convert_tree_to_paths__sort_by_cpu) {
  const bool with_wall = false, with_cpu = false;
  auto root = Node<Counter>::make_root("All", Counter(0, 0, 0, 0, with_wall, with_cpu));
  auto a = Node<Counter>::add_child("A", Counter(0, 0, 0, 1, with_wall, with_cpu), root);
  auto b = Node<Counter>::add_child("B", Counter(0, 0, 0, 0, with_wall, with_cpu), root);
  auto aa = Node<Counter>::add_child("AA", Counter(0, 0, 0, 1, with_wall, with_cpu), a);
  auto ab = Node<Counter>::add_child("AB", Counter(0, 0, 0, 0, with_wall, with_cpu), a);
  auto ba = Node<Counter>::add_child("BA", Counter(0, 0, 0, 0, with_wall, with_cpu), b);
  auto bb = Node<Counter>::add_child("BB", Counter(0, 0, 0, 1, with_wall, with_cpu), b);
  auto tree_paths = TreePath::convert_tree_to_paths(root, true, SortBy::cpu);
  auto paths = std::vector<std::list<std::string>>{};
  for (const auto& p : tree_paths)
    paths.emplace_back(p.path);
  auto reference_paths = std::vector<std::list<std::string>>{};
  for (const auto& node : {root, a, aa, ab, b, bb, ba})
    reference_paths.emplace_back(path_to_node(node));
  ASSERT_EQ(paths, reference_paths);
}

TEST(TreePath, convert_tree_to_paths__sort_by_call) {
  const bool with_wall = false, with_cpu = false;
  auto root = Node<Counter>::make_root("All", Counter(0, 0, 0, 0, with_wall, with_cpu));
  auto a = Node<Counter>::add_child("A", Counter(1, 0, 0, 0, with_wall, with_cpu), root);
  auto b = Node<Counter>::add_child("B", Counter(0, 0, 0, 0, with_wall, with_cpu), root);
  auto aa = Node<Counter>::add_child("AA", Counter(1, 0, 0, 0, with_wall, with_cpu), a);
  auto ab = Node<Counter>::add_child("AB", Counter(0, 0, 0, 0, with_wall, with_cpu), a);
  auto ba = Node<Counter>::add_child("BA", Counter(0, 0, 0, 0, with_wall, with_cpu), b);
  auto bb = Node<Counter>::add_child("BB", Counter(1, 0, 0, 0, with_wall, with_cpu), b);
  auto tree_paths = TreePath::convert_tree_to_paths(root, true, SortBy::calls);
  auto paths = std::vector<std::list<std::string>>{};
  for (const auto& p : tree_paths)
    paths.emplace_back(p.path);
  auto reference_paths = std::vector<std::list<std::string>>{};
  for (const auto& node : {root, a, aa, ab, b, bb, ba})
    reference_paths.emplace_back(path_to_node(node));
  ASSERT_EQ(paths, reference_paths);
}

TEST(TreePath, convert_tree_to_paths__sort_by_operations) {
  const bool with_wall = false, with_cpu = false;
  auto root = Node<Counter>::make_root("All", Counter(0, 0, 0, 0, with_wall, with_cpu));
  auto a = Node<Counter>::add_child("A", Counter(0, 1, 0, 0, with_wall, with_cpu), root);
  auto b = Node<Counter>::add_child("B", Counter(0, 0, 0, 0, with_wall, with_cpu), root);
  auto aa = Node<Counter>::add_child("AA", Counter(0, 1, 0, 0, with_wall, with_cpu), a);
  auto ab = Node<Counter>::add_child("AB", Counter(0, 0, 0, 0, with_wall, with_cpu), a);
  auto ba = Node<Counter>::add_child("BA", Counter(0, 0, 0, 0, with_wall, with_cpu), b);
  auto bb = Node<Counter>::add_child("BB", Counter(0, 1, 0, 0, with_wall, with_cpu), b);
  auto tree_paths = TreePath::convert_tree_to_paths(root, true, SortBy::operations);
  auto paths = std::vector<std::list<std::string>>{};
  for (const auto& p : tree_paths)
    paths.emplace_back(p.path);
  auto reference_paths = std::vector<std::list<std::string>>{};
  for (const auto& node : {root, a, aa, ab, b, bb, ba})
    reference_paths.emplace_back(path_to_node(node));
  ASSERT_EQ(paths, reference_paths);
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

TEST_F(TreePath_Fixture, report__from_node) {
  std::stringstream out;
  report(prof, out);
  std::stringstream out2;
  report(prof.root, prof.description(), out2);
  ASSERT_EQ(out2.str(), out.str());
}

TEST(graphviz, output_dot) {
  const bool with_wall = true, with_cpu = false;
  auto root = Node<Counter>::make_root("All", Counter(1, 0, 10, 0, with_wall, with_cpu));
  auto a = Node<Counter>::add_child("A", Counter(1, 0, 6, 0, with_wall, with_cpu), root);
  auto b = Node<Counter>::add_child("B", Counter(2, 0, 4, 0, with_wall, with_cpu), root);
  auto aa = Node<Counter>::add_child("AA", Counter(4, 0, 5, 0, with_wall, with_cpu), a);
  auto ab = Node<Counter>::add_child("AB", Counter(4, 0, 1, 0, with_wall, with_cpu), a);
  auto ba = Node<Counter>::add_child("BA", Counter(1, 0, 1, 0, with_wall, with_cpu), b);
  auto bb = Node<Counter>::add_child("BB", Counter(2, 0, 3, 0, with_wall, with_cpu), b);
  int hot[3] = {255, 0, 0};
  int cool[3] = {0, 0, 255};
  for (double i = 0; i < 0.1; i += 0.001) {
    std::cout << molpro::profiler::dotgraph::blend_colours(i, hot, cool) << "\n";
  }
  molpro::profiler::dotgraph::make_dotgraph(root, 10.0, hot, cool, 0.000, true);
  // std::string dotgraph = get_dotgraph(prof, hot, cool, 0.00001, false);
  //  std::cout << dotgraph;
  //  TODO: currently there is no way to validate this dotgraph without introducing an external dependency on dot
  //  TODO: test conversion to intermediate data structure, merging, and change this to a different type of test
}

TEST(graphviz, merge) {
  const bool with_wall = true, with_cpu = false;
  auto root = Node<Counter>::make_root("All", Counter(1, 0, 10, 0, with_wall, with_cpu));
  auto a = Node<Counter>::add_child("A", Counter(1, 0, 6, 0, with_wall, with_cpu), root);
  auto b = Node<Counter>::add_child("B", Counter(2, 0, 4, 0, with_wall, with_cpu), root);
  auto aa = Node<Counter>::add_child("AA", Counter(4, 0, 5, 0, with_wall, with_cpu), a);
  auto ab = Node<Counter>::add_child("AB", Counter(4, 0, 1, 0, with_wall, with_cpu), a);
  auto ba = Node<Counter>::add_child("BA", Counter(1, 0, 1, 0, with_wall, with_cpu), b);
  auto bb = Node<Counter>::add_child("AB", Counter(2, 0, 3, 0, with_wall, with_cpu), b);
  std::vector<molpro::profiler::dotgraph::GraphEntry> graph_entries;
  molpro::profiler::dotgraph::make_dotgraph_vec(root, 10, graph_entries);
  molpro::profiler::dotgraph::merge_vec(graph_entries);
  //  int hot[3] = {255,0,0};
  //  int cool[3] = {0,0,255};
  // std::cout << molpro::profiler::dotgraph::get_graph_markup(graph_entries, 10, hot, cool) << "\n";
  // check the time of AB is equal to 4
  for (size_t i = 0; i < graph_entries.size(); i++) {
    if (graph_entries[i].name == "AB") {
      ASSERT_EQ(graph_entries[i].runtime, 4);
    }
  }
}

TEST(graphviz, frequency) {
  const bool with_wall = true, with_cpu = false;
  auto root = Node<Counter>::make_root("All", Counter(1, 0, 10, 0, with_wall, with_cpu));
  auto a = Node<Counter>::add_child("A", Counter(1, 0, 6, 0, with_wall, with_cpu), root);
  auto b = Node<Counter>::add_child("B", Counter(1, 0, 4, 0, with_wall, with_cpu), root);
  auto c = Node<Counter>::add_child("C", Counter(1, 500, 6, 0, with_wall, with_cpu), a);
  auto d = Node<Counter>::add_child("C", Counter(1, 500, 4, 0, with_wall, with_cpu), b);
  std::vector<molpro::profiler::dotgraph::GraphEntry> graph_entries;
  molpro::profiler::dotgraph::make_dotgraph_vec(root, 10, graph_entries);
  molpro::profiler::dotgraph::merge_vec(graph_entries);
  //  int hot[3] = {255, 0, 0};
  //  int cool[3] = {0, 0, 255};
  ASSERT_EQ(molpro::profiler::detail::frequency(graph_entries[4].operations, graph_entries[4].runtime), " (100 Hz)");
}

TEST(graphviz, cull_orphans) {
  const bool with_wall = true, with_cpu = false;
  auto root = Node<Counter>::make_root("All", Counter(1, 0, 10, 0, with_wall, with_cpu));
  auto a = Node<Counter>::add_child("A", Counter(1, 0, 6, 0, with_wall, with_cpu), root);
  auto b = Node<Counter>::add_child("B", Counter(1, 0, 0.0001, 0, with_wall, with_cpu), root);
  auto aa = Node<Counter>::add_child("AA", Counter(4, 0, 5, 0, with_wall, with_cpu), a);
  auto ab = Node<Counter>::add_child("AB", Counter(4, 0, 1, 0, with_wall, with_cpu), a);
  auto ba = Node<Counter>::add_child("BA", Counter(1, 0, 1, 0, with_wall, with_cpu), b);
  auto bb = Node<Counter>::add_child("AB", Counter(2, 0, 3, 0, with_wall, with_cpu), b);
  std::vector<molpro::profiler::dotgraph::GraphEntry> graph_entries;
  molpro::profiler::dotgraph::make_dotgraph_vec(root, 10, graph_entries);
  molpro::profiler::dotgraph::apply_threshold(graph_entries, 0.01, 10);
  molpro::profiler::dotgraph::destroy_orphans(graph_entries);
  //  int hot[3] = {255, 0, 0};
  //  int cool[3] = {0, 0, 255};
  // std::cout << molpro::profiler::dotgraph::get_graph_markup(graph_entries, 10, hot, cool, true) << "\n";
  ASSERT_EQ(graph_entries.size(), 8); // 5 nodes and 3 edges
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

TEST(report_detail, total_operation_count__nullptr) {
  auto tot_count = total_operation_count(nullptr);
  ASSERT_EQ(tot_count, 0);
}

TEST(report_detail, path_to_node__null) {
  auto result = path_to_node(nullptr);
  ASSERT_TRUE(result.empty());
}

TEST(report_detail, path_to_node) {
  Profiler p("test");
  p.start("A").start("B").start("C");
  auto result = path_to_node(p.active_node);
  auto reference = std::list<std::string>{p.root->name, "A", "B", "C"};
  ASSERT_FALSE(result.empty());
  ASSERT_EQ(result, reference);
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

void test_write_timing(double time, size_t n_op, std::string expected) {
  std::stringstream result;
  molpro::profiler::detail::write_timing(result, time, n_op);
  EXPECT_EQ(result.str(), expected);
}
TEST(report_detail, write_timing) {
  test_write_timing(0, 0, "0 s");
  test_write_timing(0, 1, "0 s");
  test_write_timing(1, 0, "1 s");
  test_write_timing(999, 0, "999 s");
  test_write_timing(1000, 0, "1 ks");
  test_write_timing(1e24, 0, "1 Ys");
  test_write_timing(1e28, 0, "10000 Ys");
  test_write_timing(1e-6, 0, "1 us");
  test_write_timing(9.999999e-7, 0, "1000 ns");
  test_write_timing(1e-28, 0, "0.0001 ys");
  test_write_timing(1e-6, 10, "1 us (10 MHz)");
  test_write_timing(1e-6, 999, "1 us (999 MHz)");
  test_write_timing(1e-6, 1000, "1 us (1 GHz)");
  test_write_timing(1e-21, 1234000, "1 zs (1234 YHz)");
}