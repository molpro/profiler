#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <molpro/profiler/Counter.h>
#include <molpro/profiler/Node.h>
#include <molpro/profiler/Profiler.h>

using molpro::profiler::Counter;
using molpro::profiler::Node;
using molpro::profiler::Profiler;

struct DummyCounter {
  int i = 0;
};

TEST(Node, make_root) {
  const auto name = "root";
  auto node = Node<DummyCounter>::make_root(name, {});
  ASSERT_EQ(node->name, name);
  ASSERT_FALSE(node->parent);
  ASSERT_TRUE(node->children.empty());
}

TEST(Node, add_child) {
  auto root = Node<DummyCounter>::make_root("root", {});
  auto child = Node<DummyCounter>::add_child("child", {}, root);
  auto grand_child = Node<DummyCounter>::add_child("grand child", {}, child);
  ASSERT_FALSE(root->children.empty());
  ASSERT_EQ(root->children["child"], child);
  ASSERT_FALSE(child->children.empty());
  ASSERT_EQ(child->children["grand child"], grand_child);
  ASSERT_TRUE(grand_child->children.empty());
}

TEST(Node, deep_copy) {
  auto root = Node<DummyCounter>::make_root("root", {});
  auto child = Node<DummyCounter>::add_child("child", {}, root);
  auto grand_child = Node<DummyCounter>::add_child("grand child", {}, child);
  auto root_copy = Node<DummyCounter>::deep_copy(root, nullptr);
  auto& child_copy = root_copy->children["child"];
  auto& grand_child_copy = child_copy->children["grand child"];
  root_copy->counter.i = 1;
  child_copy->counter.i = 2;
  grand_child_copy->counter.i = 3;
  ASSERT_NE(root_copy, root);
  ASSERT_NE(child_copy, child);
  ASSERT_NE(grand_child_copy, grand_child);
  ASSERT_NE(root_copy->counter.i, root->counter.i);
  ASSERT_NE(child_copy->counter.i, child->counter.i);
  ASSERT_NE(grand_child_copy->counter.i, grand_child->counter.i);
}

TEST(Node, walk__null) {
  auto n = Node<Counter>::make_root("test", {});
  ASSERT_THROW(n->walk({""}), molpro::profiler::NodePathError);
}

TEST(Node, walk__branch_node) {
  Profiler p("test");
  p.start("A").start("B");
  auto b = p.active_node;
  p.start("C");
  ASSERT_EQ(p.root->walk({"A", "B"}), b);
}

TEST(Node, walk__leaf_node) {
  Profiler p("test");
  p.start("A").start("B").start("C");
  auto c = p.active_node;
  ASSERT_EQ(p.root->walk({"A", "B", "C"}), c);
}

TEST(Node, walk__vector) {
  Profiler p("test");
  p.start("A").start("B").start("C");
  auto c = p.active_node;
  std::vector<std::string> path{"A", "B", "C"};
  ASSERT_EQ(p.root->walk(begin(path), end(path)), c);
}

TEST(Node, count_nodes) {
  Profiler p("test");
  p.start("A").start("AA").stop().start("AB").stop().stop();
  p.start("B").start("BA").stop().start("BB").stop().stop();
  auto n_nodes = p.root->count_nodes();
  ASSERT_EQ(n_nodes, 7);
}