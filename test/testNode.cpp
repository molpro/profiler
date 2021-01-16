#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <molpro/profiler/Counter.h>
#include <molpro/profiler/Node.h>
#include <molpro/profiler/Profiler.h>

using molpro::profiler::Counter;
using molpro::profiler::Node;
using molpro::profiler::NodePathError;
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

TEST(Node, child__throw) {
  auto n = Node<Counter>::make_root("test", {});
  ASSERT_THROW(n->walk({""}), NodePathError);
  auto ch = Node<Counter>::add_child("A", {}, n);
  ASSERT_THROW(n->walk({"B"}), NodePathError);
}

TEST(Node, child) {
  auto n = Node<Counter>::make_root("test", {});
  auto child = Node<Counter>::add_child("A", {}, n);
  ASSERT_EQ(n->child({"A"}), child);
}

TEST(Node, find_parent__null) {
  auto n = Node<Counter>::make_root("test", {});
  ASSERT_EQ(n->find_parent(""), nullptr);
}

TEST(Node, find_parent__not_found) {
  Profiler p("Test");
  p.start("A").start("B");
  ASSERT_EQ(p.active_node->find_parent(""), nullptr);
}

TEST(Node, find_parent__found) {
  Profiler p("Test");
  p.start("A").start("B");
  auto ref_a = p.start("A").active_node;
  p.start("B");
  auto n = p.active_node->find_parent("A");
  ASSERT_EQ(n, ref_a);
  n = p.active_node->find_parent("All");
  ASSERT_EQ(n, p.root);
}

TEST(Node, walk_up__throw) {
  auto node = Node<Counter>::make_root("test", {});
  ASSERT_THROW(node->walk_up(0), NodePathError);
  ASSERT_THROW(node->walk_up(-1), NodePathError);
}

TEST(Node, walk_up__nullptr) {
  Profiler p("test");
  auto a = p.start("A").active_node;
  auto b = p.start("B").active_node;
  auto c = p.start("C").active_node;
  ASSERT_EQ(c->walk_up(1), b);
  ASSERT_EQ(c->walk_up(2), a);
  ASSERT_EQ(c->walk_up(3), p.root);
  ASSERT_EQ(c->walk_up(4), nullptr);
  ASSERT_EQ(c->walk_up(10), nullptr);
  ASSERT_EQ(p.root->walk_up(1), nullptr);
}

TEST(Node, count_nodes) {
  Profiler p("test");
  p.start("A").start("AA").stop().start("AB").stop().stop();
  p.start("B").start("BA").stop().start("BB").stop().stop();
  auto n_nodes = p.root->count_nodes();
  ASSERT_EQ(n_nodes, 7);
}