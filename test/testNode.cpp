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

TEST(Node, count_nodes) {
  Profiler p("test");
  p.start("A").start("AA").stop().start("AB").stop().stop();
  p.start("B").start("BA").stop().start("BB").stop().stop();
  auto n_nodes = Node<Counter>::count_nodes(p.root);
  ASSERT_EQ(n_nodes, 7);
}